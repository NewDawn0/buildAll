{-# LANGUAGE OverloadedStrings #-}

module Flake
  ( FlakeOutput(..)
  , getFlakeOutput
  ) where

import Data.Aeson
import Control.Monad (foldM)
import System.Directory (getHomeDirectory)
import System.FilePath ((</>), splitDirectories)
import System.Process (readProcess)
import qualified Data.ByteString.Lazy.Char8 as BL
import qualified Data.Aeson.KeyMap as KM
import qualified Data.Aeson.Key as K

data FlakeOutput = FlakeOutput
  { flakePath :: FilePath
  , systemArch :: String
  , packages :: [String]
  , devShells :: [String]
  } deriving (Show)

getFlakeJSON :: FilePath -> IO (Either String Value)
getFlakeJSON path = do
  putStrLn $ "Getting flake output for " ++ path
  raw <- readProcess "nix" ["flake", "show", path, "--json", "--all-systems"] ""
  return $ eitherDecode $ BL.pack raw

getSection :: String -> Object -> String -> [String]
getSection sectionName root system =
  let keys = do
        sectionObj <- KM.lookup (K.fromString sectionName) root >>= asObject
        systemObj  <- KM.lookup (K.fromString system) sectionObj >>= asObject
        return $ map K.toString (KM.keys systemObj)
  in maybe [] id keys

buildFlakeOutput :: FilePath -> String -> Object -> FlakeOutput
buildFlakeOutput path system root = FlakeOutput
  { flakePath = path
  , systemArch = system
  , packages = getSection "packages" root system
  , devShells = getSection "devShells" root system
  }

getFlakeOutput :: String -> IO (Maybe FlakeOutput)
getFlakeOutput path = do
  system <- getSystem
  absPath <- absolutize path
  flakeJSON <- getFlakeJSON absPath
  return $ case flakeJSON of
    Right (Object root) -> Just $ buildFlakeOutput absPath system root
    _ -> Nothing

-- Helpers
getSystem :: IO String
getSystem  = do
  putStrLn "Getting system architecture"
  readProcess "nix" ["eval", "--raw", "--impure", "--expr", "builtins.currentSystem"] ""

absolutize :: FilePath -> IO FilePath
absolutize path = do
    home <- getHomeDirectory
    let (base, components) = case path of
          '~':'/':rest -> (home, splitDirectories rest)
          "~"          -> (home, [])
          _            -> ("", splitDirectories path)
    foldM resolve base components
  where
    resolve acc ".." = return $ takeDirectory acc
    resolve acc "."  = return acc
    resolve acc dir  = return $ acc </> dir
    takeDirectory = reverse . drop 1 . dropWhile (/= '/') . reverse

asObject :: Value -> Maybe Object
asObject (Object o) = Just o
asObject _ = Nothing
