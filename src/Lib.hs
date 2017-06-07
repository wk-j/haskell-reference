module Lib ( someFunc ) where

import System.Environment
import System.Directory
import System.FilePath.Windows
import System.Process
import Data.String.Utils
import Data.List

clearRoot :: String -> String -> String
clearRoot root path =
    replace root "" path

processDir root document = do
    exist <- doesDirectoryExist root
    docExist <- doesFileExist document
    if exist && docExist then
        do
            let docDir =  takeDirectory document
            fullPaths <- readProcess "mdfind" ["-onlyin", root, "-name", ".dll"] ""
            let rootPaths = map (clearRoot root) $ split "\n" fullPaths
            putStrLn $ head rootPaths
    else
        putStrLn "-- Dir Not Exist --"

someFunc :: IO ()
someFunc = do
    args <- getArgs
    case args of
        [root, document] ->
            processDir root document
        _ ->
            putStrLn "-- Invalid Argument --"
