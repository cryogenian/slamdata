module SlamData.Workspace.Card.Eval.Process
  ( elaborate
  , elaborateQuery
  , processIdent
  ) where

import SlamData.Prelude
import Control.Monad.State.Class (class MonadState)
import Control.Monad.Writer.Class (class MonadWriter)
import Control.Monad.RWS as RWS
import Data.Foldable as F
import Data.List as L
import Data.List.NonEmpty as NL
import Data.Map as Map
import Data.Path.Pathy as Path
import Data.Set as Set
import Data.String as String
import Data.StrMap as SM
import Matryoshka (cataM, embed)
import SlamData.Workspace.Card.CardId as CID
import SlamData.Workspace.Card.Port as Port
import SlamData.Workspace.Card.Port.VarMap as VM
import SqlSquared as Sql
import Utils.Path as PU

-- | Process elaboration is a transformation from a SQL^2 Query to a
-- | SQL^2 Module based on a VarMap. The reason we do this is because a Query
-- | might contain references to other Resources which may not refer to a
-- | static path on disk, but to a Process/Function call. The goals of
-- | elaboration are to:
-- |
-- | * Determine if a Query should be a Module. A Query should be a Module if
-- |   it contains "true" parameterization. True parameterization occurs when
-- |   the query contains references a URL VarMap binding or another Process.
-- |   Even though a Query may reference something like `:resource`, it may not
-- |   be truly parameterized if that variable refers to a static path. In such
-- |   a case, the path is instead inlined and does not qualify as true
-- |   parameterization.
-- | * Dereference Processes as a SQL^2 import and a call to the specified
-- |   function. We also alias these calls in a local temporary binding.
-- | * Track variable hygiene. A card at the beginning of the stack may
-- |   reference a URL variable, but that variable may be shadowed by a
-- |   subsequent card. To the end user they have the same name, but we still
-- |   must pass in both references when executing the query.
-- | * Elaborate Function declarations in Query cards to take free variables
-- |   as arguments. When a Query is turned into a Module, the "index" function
-- |   must take all free variables and pass them to the functions since they
-- |   can't just simply be templated. TODO: Can this be elided if all variables
-- |   are renamed to be uniquely addressed?

type Elaborate m a
  = MonadReader RewriteEnv m
  ⇒ MonadState RewriteState m
  ⇒ MonadWriter RewriteLog m
  ⇒ a

type VarMapValues =
  NL.NonEmptyList (CID.CardId × VM.VarMapValue)

type RewriteLog = Set.Set String

type RewriteState =
  { fresh ∷ Int
  , decls ∷ L.List (Sql.SqlDeclF Sql.Sql)
  , bindings ∷ L.List (String × Sql.Sql)
  , aliases ∷ Map.Map (CID.CardId × String) String
  , freeVars ∷ Map.Map String (L.List String)
  , isProcess ∷ Boolean
  }

type RewriteEnv =
  { varMap ∷ Port.VarMap
  , varScope ∷ Set.Set String
  , shouldAlias ∷ Boolean
  }

type RewriteM =
  RWS.RWS RewriteEnv RewriteLog RewriteState

initialEnv ∷ VM.VarMap → RewriteEnv
initialEnv varMap =
  { varMap
  , varScope: Set.empty
  , shouldAlias: true
  }

initialState ∷ RewriteState
initialState =
  { fresh: 0
  , decls: mempty
  , bindings: mempty
  , aliases: mempty
  , freeVars: mempty
  , isProcess: false
  }

elaborate
  ∷ CID.CardId
  → VM.VarMap
  → Sql.SqlQuery
  → Path.FileName × Either Sql.SqlQuery Sql.SqlModule
elaborate currentSource varMap query =
  case RWS.runRWS go (initialEnv varMap) initialState of
    RWS.RWSResult st res _ →
      Path.FileName (processIdent currentSource) × res

  where
  selfName ∷ String
  selfName = processIdent currentSource

  go ∷ RewriteM (Either Sql.SqlQuery Sql.SqlModule)
  go = do
    Sql.Query decls sql ← elaborateQuery' query
    st ← RWS.get
    vars ← (processVars <<< _.varMap) =<< ask
    pure if st.isProcess
      then Right $ sqlModule
        (st.decls <> decls)
        selfName
        (uncurry VM.uniqueVarName ∘ snd <$> vars)
        st.bindings
        sql
      else Left $ Sql.Query decls sql

elaborateQuery
  ∷ VM.VarMap
  → Sql.SqlQuery
  → Sql.SqlQuery
elaborateQuery varMap query =
  case RWS.runRWS go (initialEnv varMap) initialState of
    RWS.RWSResult _ res _ → res

  where
  go ∷ RewriteM Sql.SqlQuery
  go = do
    Sql.Query decls sql ← elaborateQuery' query
    st ← RWS.get
    pure $ Sql.Query
      (st.decls <> decls)
      (foldBindings sql st.bindings)

elaborateQuery' ∷ ∀ m. Elaborate m (Sql.SqlQuery → m Sql.SqlQuery)
elaborateQuery' (Sql.Query decls expr) =
  Sql.Query
    <$> traverse elaborateDecl decls
    <*> elaborateExpr expr

elaborateDecl ∷ ∀ m. Elaborate m (Sql.SqlDeclF Sql.Sql → m (Sql.SqlDeclF Sql.Sql))
elaborateDecl = case _ of
  Sql.Import a → pure $ Sql.Import a
  Sql.FunctionDecl { ident, args, body } → do
    body' × freeVars ←
      RWS.listen $ RWS.local
        (\env → env
          { varScope = foldr Set.insert env.varScope args
          , shouldAlias = false
          })
        (elaborateExpr body)
    let
      freeVars' = L.fromFoldable freeVars
    if L.null freeVars'
      then
        pure $ Sql.FunctionDecl { ident, args, body: body' }
      else do
        addFreeVars ident freeVars'
        pure $ Sql.FunctionDecl { ident, args: freeVars' <> args, body: body' }

elaborateExpr ∷ ∀ m. Elaborate m (Sql.Sql → m Sql.Sql)
elaborateExpr = cataM case _ of
  Sql.Vari vari → elaborateVar vari
  Sql.InvokeFunction { name, args } → substInvoke name args
  Sql.Select sel@({ relations: Just (Sql.VariRelation { vari, alias }) }) →
    substSelect sel vari alias
  sql → pure $ embed sql

  where
  substSelect ∷ Sql.SelectR Sql.Sql → String → Maybe String → m Sql.Sql
  substSelect sel vari alias = do
    subst ← elaborateVar vari
    { bindings } ← RWS.get
    relations ← case unwrap subst of
      Sql.Vari vari' →
        pure $ Just $ Sql.VariRelation { vari: vari', alias }
      Sql.Ident i | not (F.any (eq i ∘ fst) bindings), Just path ← PU.parseAnyFilePath i →
        pure $ Just $ Sql.TableRelation { path, alias }
      expr → do
        aliasName ← maybe tmpName pure alias
        pure $ Just $ Sql.ExprRelation { expr: embed expr, aliasName }
    pure $ embed $ Sql.Select sel { relations = relations }

  substInvoke ∷ String → L.List Sql.Sql → m Sql.Sql
  substInvoke name args = do
    st ← RWS.get
    { shouldAlias } ← ask
    args' ← case Map.lookup name st.freeVars of
      Just freeVars | shouldAlias → do
        freeVars' ← traverse elaborateVar freeVars
        pure (freeVars' <> args)
      Just freeVars → pure (map Sql.vari freeVars <> args)
      Nothing → pure args
    pure $ embed $ Sql.InvokeFunction { name, args: args' }

elaborateVar ∷ ∀ m. Elaborate m (String → m Sql.Sql)
elaborateVar vari = do
  { varMap, varScope } ← ask
  if Set.member vari varScope
    then default
    else maybe default substValue $ SM.lookup vari varMap

  where
  default ∷ m Sql.Sql
  default = pure $ Sql.vari vari

  substValue ∷ VarMapValues → m Sql.Sql
  substValue vals | { head: source × value, tail } ← NL.uncons vals = do
    RWS.tell $ Set.singleton vari
    case value of
      VM.Expr expr → substExpr (NL.length vals - 1) expr
      VM.Resource res → substResource source res
      VM.Union vals' → substUnion source vari vals'

  substExpr ∷ Int → Sql.Sql → m Sql.Sql
  substExpr mark sql = do
    activateProcess
    pure $ Sql.vari $ VM.uniqueVarName vari mark

  substResource ∷ CID.CardId → VM.Resource → m Sql.Sql
  substResource source = case _ of
    VM.Path filePath → pure $ sqlPath filePath
    VM.View filePath _ _ → pure $ sqlPath filePath
    VM.Process filePath _ localVarMap → do
      { shouldAlias } ← ask
      if shouldAlias
        then do
          activateProcess
          substProcessVar source filePath localVarMap vari
        else default

substUnion ∷ ∀ m. Elaborate m (CID.CardId → String → Array VM.VarMapValue → m Sql.Sql)
substUnion source vari vals = do
  exprs ← traverse substValue vals
  name ← addAlias source vari $ embed $ Sql.SetLiteral $ L.fromFoldable exprs
  pure $ Sql.ident name

  where
  substValue ∷ VM.VarMapValue → m Sql.Sql
  substValue = case _ of
    VM.Expr sql → pure sql
    VM.Resource res → substResource res
    VM.Union vals' → do
      -- TODO: Maybe this should be indexed instead of unique?
      tmp ← tmpName
      substUnion source tmp vals'

  substResource ∷ VM.Resource → m Sql.Sql
  substResource = case _ of
    VM.Path filePath → pure $ sqlPath filePath
    VM.View filePath _ _ → pure $ sqlPath filePath
    VM.Process filePath _ localVarMap → do
      activateProcess
      -- TODO: Maybe this should be indexed instead of unique?
      tmp ← tmpName
      substProcessVar source filePath localVarMap tmp

substProcessVar ∷ ∀ m. Elaborate m (CID.CardId → PU.RelFilePath → VM.VarMap → String → m Sql.Sql)
substProcessVar source filePath localVarMap vari = do
  { aliases } ← RWS.get
  maybe genAlias (pure ∘ Sql.ident) $
    Map.lookup (source × vari) aliases

  where
  default ∷ m Sql.Sql
  default = pure $ Sql.vari vari

  genAlias ∷ m Sql.Sql
  genAlias = case Path.peel filePath of
    Just (dir × Right fileName) → do
      addProcessImport dir
      expr ← invokeProcess fileName localVarMap
      Sql.ident <$> addAlias source vari expr
    _ → default

invokeProcess ∷ ∀ m. Elaborate m (Path.FileName → VM.VarMap → m Sql.Sql)
invokeProcess (Path.FileName fileName) localVarMap = do
  { aliases } ← RWS.get
  vars ← processVars localVarMap
  let
    args ∷ L.List Sql.Sql
    args = vars <#> \(source × vari × mark) →
      case Map.lookup (source × vari) aliases of
        Just name → Sql.ident name
        Nothing → Sql.vari $ VM.uniqueVarName vari mark
  pure $ embed $ Sql.InvokeFunction { name: fileName, args }

processVars ∷ ∀ m. Elaborate m (VM.VarMap → m (L.List (CID.CardId × String × Int)))
processVars localVarMap = do
  { varMap } ← ask
  let marks = VM.markIndex varMap
  pure $ L.mapMaybe
    (\{ source, name, value } → do
      guard $ not (VM.isResource value)
      mark ← Map.lookup (source × name) marks
      pure (source × name × mark))
    (VM.expand localVarMap)

processIdent ∷ CID.CardId → String
processIdent cid =
  "card" <> String.replaceAll
    (String.Pattern "-")
    (String.Replacement "")
    (CID.toString cid)

sqlPath ∷ ∀ a b. Path.Path a b Path.Sandboxed → Sql.Sql
sqlPath = embed ∘ Sql.Ident ∘ Path.printPath

sqlModule
  ∷ L.List (Sql.SqlDeclF Sql.Sql)
  → String
  → L.List String
  → L.List (String × Sql.Sql)
  → Sql.Sql
  → Sql.SqlModule
sqlModule decls name args bindings expr =
  Sql.Module
    $ L.snoc decls
    $ Sql.FunctionDecl { ident: name, args, body }
  where
  body = foldBindings expr bindings

foldBindings ∷ ∀ f. Foldable f ⇒ Sql.Sql → f (String × Sql.Sql) → Sql.Sql
foldBindings = foldl \in_ (ident × bindTo) → embed $ Sql.Let { ident, bindTo, in_ }

tmpName ∷ ∀ m. MonadState RewriteState m ⇒ m String
tmpName = RWS.state \st → "tmp__" <> show st.fresh × st { fresh = st.fresh + 1 }

addAlias ∷ ∀ m. MonadState RewriteState m ⇒ CID.CardId → String → Sql.Sql → m String
addAlias source old sql = do
  tmp ← tmpName
  RWS.modify \st → st
    { bindings = L.Cons (tmp × sql) st.bindings
    , aliases = Map.insert (source × old) tmp st.aliases
    }
  pure tmp

addProcessImport ∷ ∀ m. MonadState RewriteState m ⇒ PU.RelDirPath → m Unit
addProcessImport dir = RWS.modify \st → st { decls = L.Cons (Sql.Import (Path.printPath dir)) st.decls }

activateProcess ∷ ∀ m. MonadState RewriteState m ⇒ m Unit
activateProcess = RWS.modify _ { isProcess = true }

addFreeVars ∷ ∀ m. MonadState RewriteState m ⇒ String → L.List String → m Unit
addFreeVars key value = RWS.modify \st → st { freeVars = Map.insert key value st.freeVars }
