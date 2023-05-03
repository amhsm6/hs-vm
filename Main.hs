import System.Exit
import System.Environment

import Core
import Parser

main :: IO ()
main = do
    args <- getArgs

    if null args then do
        putStrLn "fatal error: no input files"
        exitFailure
    else
        pure ()

    input <- readFile $ head args 

    case runParser parseProgram input of
        Just (_, prog) -> case execProg prog of
                              Right io -> io
                              Left e -> putStrLn $ "ERROR: " ++ show e
        Nothing -> do
            putStrLn "parse error"
            exitFailure
