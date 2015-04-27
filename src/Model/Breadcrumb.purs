module Model.Breadcrumb where

-- | Model for one breadcrumb
type Breadcrumb = {
  link :: String,
  name :: String
  }

rootBreadcrumb :: Breadcrumb
rootBreadcrumb = {
  name: "Home",
  link: "/"
  }
