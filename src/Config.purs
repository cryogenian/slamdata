module Config where

uploadUrl :: String
uploadUrl = "/upload"

metadataUrl :: String
metadataUrl = "/metadata/fs/"

dataUrl :: String
dataUrl = "/data/fs/"


searchTimeout :: Number
searchTimeout = 500

slamDataHome :: String
slamDataHome = "http://slamdata.com"


userEnabled :: Boolean
userEnabled = false
