{-# LANGUAGE OverloadedStrings #-}

module Flake
  ( FlakeOutput (..),
    getFlakeOutput,
  )
where

import Control.Monad (foldM)
import Data.Aeson
import qualified Data.Aeson.Key as K
import qualified Data.Aeson.KeyMap as KM
import qualified Data.ByteString.Lazy.Char8 as BL
import System.Directory (getHomeDirectory, makeAbsolute)
import System.FilePath (isPathSeparator, splitDirectories, (</>))
import System.Process (readProcess)

data FlakeOutput = FlakeOutput
  { flakePath :: FilePath,
    systemArch :: String,
    packages :: [String],
    devShells :: [String]
  }
  deriving (Show)

getFlakeJSON :: FilePath -> IO (Either String Value)
getFlakeJSON path = do
  putStrLn $ "> Getting flake output for " ++ take 30 path
  raw <- readProcess "nix" ["flake", "show", path, "--json", "--all-systems"] ""
  return $ eitherDecode $ BL.pack raw

getSection :: String -> Object -> String -> [String]
getSection sectionName root system =
  let keys = do
        sectionObj <- KM.lookup (K.fromString sectionName) root >>= asObject
        systemObj <- KM.lookup (K.fromString system) sectionObj >>= asObject
        return $ map K.toString (KM.keys systemObj)
   in maybe [] id keys

buildFlakeOutput :: FilePath -> String -> Object -> FlakeOutput
buildFlakeOutput path system root =
  FlakeOutput
    { flakePath = path,
      systemArch = system,
      packages = getSection "packages" root system,
      devShells = getSection "devShells" root system
    }

getFlakeOutput :: String -> IO (Maybe FlakeOutput)
getFlakeOutput path = do
  system <- getSystem
  absPath <- absolutise path
  flakeJSON <- getFlakeJSON absPath
  return $ case flakeJSON of
    Right (Object root) -> Just $ buildFlakeOutput absPath system root
    _ -> Nothing

-- Helpers
getSystem :: IO String
getSystem = do
  putStrLn "> Getting system architecture"
  readProcess "nix" ["eval", "--raw", "--impure", "--expr", "builtins.currentSystem"] ""

absolutise :: FilePath -> IO FilePath
absolutise path = do
  home <- getHomeDirectory
  let (base, components) = case path of
        '~' : '/' : rest -> (home, splitDirectories rest)
        "~" -> (home, [])
        "" -> (".", [])
        _
          | isPathSeparator (head path) -> ("/", splitDirectories (tail path))
          | otherwise -> (".", splitDirectories path)
  absBase <- if null base then makeAbsolute "." else makeAbsolute base
  foldM resolveComponent absBase components
  where
    resolveComponent :: FilePath -> String -> IO FilePath
    resolveComponent acc dir
      | dir == "." = return acc
      | dir == ".." = return $ takeDirectory acc
      | otherwise = return $ acc </> dir
    takeDirectory :: FilePath -> FilePath
    takeDirectory p =
      case reverse p of
        [] -> "."
        xs -> case dropWhile isPathSeparator xs of
          [] -> if any isPathSeparator p then "/" else "."
          ys -> case dropWhile (not . isPathSeparator) ys of
            [] -> "."
            zs -> reverse (dropWhile isPathSeparator zs)

asObject :: Value -> Maybe Object
asObject (Object o) = Just o
asObject _ = Nothing
