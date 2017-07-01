{-# LANGUAGE ViewPatterns #-}


module Lib ( someFunc ) where

import System.Environment
import System.Directory
import System.FilePath.Windows
import System.Process
import Data.String.Utils
import Data.List
import Text.Printf

clearRoot :: String -> String -> String
clearRoot root = replace root ""

(|>) :: t -> (t -> t) -> t
(|>) x y = y x

processDir :: String -> String -> String -> IO [String]
processDir root document extension = do
    exist <- doesDirectoryExist root
    docExist <- doesFileExist document
    if exist && docExist then
        do
            let fullDocDir = takeDirectory document
            let docDir = replace root "" fullDocDir

            fullPaths <- readProcess "mdfind" ["-onlyin", root, "-name", extension] ""
            let notEmpty = split "\n" fullPaths |> filter ((/=) "")
            let rootPaths = notEmpty |> map (clearRoot root)
            let slashs = length $ filter ((==) '/') docDir
            let leader = 
                    if slashs > 0 then 
                        intercalate "" $ replicate slashs "../"
                    else 
                        if docDir == "." then 
                            "./" 
                        else 
                            "../"
            let append x = (leader ++ x) |> replace "//" "/"
            return $ map append rootPaths
    else
        return []

createReference :: String -> String
createReference = printf "#r \"%s\""

createLoad :: String -> String
createLoad = printf "#load \"%s\""

someFunc :: IO ()
someFunc = do
    args <- getArgs
    case args of
        [root, document] -> do
            dlls <- processDir root document ".dll"
            let reference = dlls |> map createReference
            mapM_ putStrLn reference

            fsx <- processDir root document ".fs"
            let loads = fsx |> map createLoad
            mapM_ putStrLn loads
        _ ->
            putStrLn "-- Invalid Argument --"
