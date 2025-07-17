{-# LANGUAGE NamedFieldPuns #-}

--
module Build
  ( buildAllOutputs,
    buildDevShells,
    buildPackages,
  )
where

import Flake (FlakeOutput (..))
import System.Process (callProcess)

build :: FlakeOutput -> String -> IO ()
build FlakeOutput {flakePath} pkgAttr = do
  putStrLn $ "> Building " ++ pkgAttr
  callProcess "nix" ["build", flakePath ++ "#" ++ pkgAttr]

buildPackages :: FlakeOutput -> IO ()
buildPackages out@FlakeOutput {packages} = do
  mapM_ (build out) packages

buildDevShells :: FlakeOutput -> IO ()
buildDevShells out@FlakeOutput {devShells, systemArch} = do
  let flakeAttr shell = "devShells." ++ systemArch ++ "." ++ shell
  mapM_ (build out . flakeAttr) devShells

buildAllOutputs :: FlakeOutput -> IO ()
buildAllOutputs out = do
  buildPackages out
  buildDevShells out
