#!/usr/bin/env stack
{-
    stack --resolver=lts-13.3 script
        --package Glob
        --package optparse-applicative
        --package process
        --package temporary
-}

{-# OPTIONS_GHC -Wall -Werror #-}

{-# LANGUAGE LambdaCase #-}

module Main (main) where

import           Control.Monad (when)
import           Data.List (groupBy)
import           Data.Traversable (for)
import           Options.Applicative
import           System.Exit (exitFailure, exitSuccess)
import           System.FilePath.Glob (glob)
import           System.IO (hClose, hPutStr)
import           System.IO.Error (tryIOError)
import           System.IO.Temp (withSystemTempFile)
import           System.Process (callProcess)
import           Text.Printf (printf)

data Line = Code String | Ignore deriving Show

data Config = Config
    { sourceDirs :: [FilePath]
    , packages :: [String]
    }

newtype AnsiColour = AnsiColour Int

ansiRed :: AnsiColour
ansiRed = AnsiColour 31

ansiGreen :: AnsiColour
ansiGreen = AnsiColour 32

ansiYellow :: AnsiColour
ansiYellow = AnsiColour 33

ansiCyan :: AnsiColour
ansiCyan = AnsiColour 36

inColour :: AnsiColour -> String -> String
inColour (AnsiColour colour) = printf "\x1b[%dm%s\x1b[0m" colour

putColour :: AnsiColour -> String -> IO ()
putColour colour = putStrLn . inColour colour

isCode :: Line -> Bool
isCode (Code _) = True
isCode _ = False

parseLine :: String -> Line
parseLine ">" = Code ""
parseLine ('>' : ' ' : rest) = Code rest
parseLine _ = Ignore

codeBlocks :: String -> [String]
codeBlocks s =
    map
        (unlines . map (\(Code l) -> l))
        (filter
            (\case (Code _ : _) -> True; _ -> False)
            (groupBy
                (\x y -> isCode x == isCode y)
                (map parseLine (lines s))))

getCodeBlocks :: FilePath -> IO [String]
getCodeBlocks fileName = codeBlocks <$> readFile fileName

checkCodeBlock :: [String] -> String -> IO Bool
checkCodeBlock ps codeBlock = withSystemTempFile "haddocks-program.hs" $ \path h -> do
    hPutStr h codeBlock
    hClose h
    let args =
                [ "ghc"
                , "--"
                , "-fno-code"
                , path
                , "-hide-all-packages"
                ] ++ concatMap (\p -> ["-package", p]) ps
    result <- tryIOError (callProcess "stack" args)
    case result of
        Left _ -> do
            putColour ansiYellow "Offending code block\nvvvvv"
            putColour ansiCyan codeBlock
            putColour ansiYellow "^^^^^\nOffending code block"
            pure False
        Right _ -> do
            putColour ansiGreen "Code block compiled successfully"
            pure True

run :: Config -> IO ()
run config = do
    fileNames <- mconcat (map (glob . (++ "/**/*.hs")) (sourceDirs config))
    allCodeBlocks <- concat <$> mapM getCodeBlocks fileNames
    when (null allCodeBlocks) $ do
        putColour ansiRed "No code blocks found"
        exitFailure
    results <- for allCodeBlocks (checkCodeBlock $ packages config)
    if and results
        then exitSuccess
        else exitFailure

main :: IO ()
main = parseConfig >>= run
    where
        parseConfig = execParser $ info
            (helper <*> configP)
            (fullDesc <> progDesc "Extract programs from Haddock documentation and compile them")
        configP = Config
            <$> some (option str (short 'd' <> long "source-dir" <> metavar "SOURCEDIR" <> help "Source directory"))
            <*> many (option str (short 'p' <> long "package" <> metavar "PACKAGE" <> help "Package"))
