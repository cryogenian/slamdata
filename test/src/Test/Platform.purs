{-
Copyright 2015 SlamData, Inc.

Licensed under the Apache License, Version 2.0 (the "License");
you may not use this file except in compliance with the License.
You may obtain a copy of the License at

    http://www.apache.org/licenses/LICENSE-2.0

Unless required by applicable law or agreed to in writing, software
distributed under the License is distributed on an "AS IS" BASIS,
WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
See the License for the specific language governing permissions and
limitations under the License.
-}

module Test.Platform (Platform(..), platform, parsePlatform) where

data Platform = Mac | FreeBSD | Linux | SunOS | Win | Unknown

foreign import _platform :: String

parsePlatform :: String -> Platform
parsePlatform p = case p of
 "darwin" -> Mac
 "freebsd" -> FreeBSD
 "linux" -> Linux
 "sunos" -> SunOS
 "win32" -> Win
 _ -> Unknown


platform :: Platform
platform = parsePlatform _platform
