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
            let docDir = takeDirectory document
            fullPaths <- readProcess "mdfind" ["-onlyin", root, "-name", ".dll"] ""
            let rootPaths = map (clearRoot root) $ split "\n" fullPaths

            putStrLn docDir

            let slashs = length $ filter ((==) '/') docDir
            let leader = if slashs > 0 then
                            intercalate "" $ replicate slashs "../"
                        else
                            "./"
            let append x = leader ++ x
            return $ map append rootPaths
    else
        return []

someFunc :: IO ()
someFunc = do
    args <- getArgs
    case args of
        [root, document] -> do
            results <- processDir root document
            mapM_ putStrLn results
        _ ->
            putStrLn "-- Invalid Argument --"
