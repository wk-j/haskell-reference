module Lib ( someFunc ) where

import System.Environment
import System.Directory
import System.FilePath.Windows
import System.Process
import Data.String.Utils
import Data.List
import Text.Printf

clearRoot :: String -> String -> String
clearRoot root path =
    replace root "" path

(|>) :: t -> (t -> t) -> t
(|>) x y = y x

processDir :: String -> String -> IO([String])
processDir root document = do
    exist <- doesDirectoryExist root
    docExist <- doesFileExist document
    if exist && docExist then
        do
            let fullDocDir = takeDirectory document
            let docDir = replace root "" fullDocDir

            fullPaths <- readProcess "mdfind" ["-onlyin", root, "-name", ".dll"] ""
            let notEmpty = split "\n" fullPaths |> filter ((/=) "")
            let rootPaths = notEmpty |> map (clearRoot root)
            let slashs = length $ filter ((==) '/') docDir 
            let leader = if slashs > 0 then
                            intercalate "" $ replicate slashs "../"
                        else
                            if docDir == "." then
                                "./"
                            else 
                                "../"
            -- let append x = replace "//" "/" $ leader ++ x
            let append x = (leader ++ x) |> replace "//" "/"
            return $ map append rootPaths
    else
        return []

createReference :: String -> String
createReference r =
    printf "#r \"%s\"" r

someFunc :: IO ()
someFunc = do
    args <- getArgs
    case args of
        [root, document] -> do
            results <- processDir root document
            let reference = results |> map createReference
            mapM_ putStrLn $ reference
        _ ->
            putStrLn "-- Invalid Argument --"
