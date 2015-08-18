module Test.Platform (Platform(..), platform) where

data Platform = Mac | FreeBSD | Linux | SunOS | Win | Unknown

foreign import _platform :: String

platform = case _platform of
 "darwin" -> Mac
 "freebsd" -> FreeBSD
 "linux" -> Linux
 "sunos" -> SunOS
 "win32" -> Win
 _ -> Unknown
