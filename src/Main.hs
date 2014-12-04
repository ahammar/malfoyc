
module Main (main) where

import Control.Monad
import System.Console.GetOpt
import System.Environment (getArgs)
import System.Exit
import System.IO

import MalfoyCompiler.Assembly
import MalfoyCompiler.Compiler
import MalfoyCompiler.GraphViz

data Format = Assembly | GraphViz
    deriving (Eq)

data Flag = Format Format | Help
    deriving (Eq)

main = do
    (output, file) <- getArgs >>= parseArgs

    input <- readFile file
    graph <- case compile input of
        Left error -> do
            hPutStrLn stderr (show error)
            exitWith (ExitFailure 1)
        Right graph -> return graph

    case output of
        Assembly -> putStrLn (toAssembly graph)
        GraphViz -> putStrLn (toGraphViz graph)

header  = "Usage: malfoyc [OPTIONS] [FILE]\n\nOptions:"

parseArgs argv = case getOpt Permute flags argv of
    (args, files, []) ->
        if length files /= 1 || Help `elem` args
            then do hPutStrLn stderr (usageInfo header flags)
                    exitWith ExitSuccess
        else if Format GraphViz `elem` args
            then
                return (GraphViz, head files)
            else
                return (Assembly, head files)
    (_, _, errs) -> do
        hPutStrLn stderr (concat errs ++ usageInfo header flags)
        exitWith (ExitFailure 1)
    where 

flags = [
    Option []  ["graphviz"] (NoArg (Format GraphViz))
        "Output GraphViz dot file",
    Option []  ["help"]   (NoArg Help)
        "You're looking at it."]
