{-|
Module      : Main
Description : Documentation tests
Copyright   : (C) Richard Cook, 2019
Licence     : MIT
Maintainer  : rcook@rcook.org
Stability   : stable
Portability : portable
-}

{-# LANGUAGE CPP #-}

module Main (main) where

import           System.FilePath.Glob
import           Test.DocTest

defaultIncludeDirs :: [FilePath]
defaultIncludeDirs = []

doctestWithIncludeDirs :: [FilePath] -> IO ()
doctestWithIncludeDirs fs = doctest (map ("-I" ++) defaultIncludeDirs ++ fs)

sourceDirs :: [FilePath]
sourceDirs = ["app", "src"]

main :: IO ()
main = mconcat (map (glob . (++ "/**/*.hs")) sourceDirs) >>= doctestWithIncludeDirs
