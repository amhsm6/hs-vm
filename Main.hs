import System.Exit
import System.Environment
import Data.List

import Core
import Parser

processLabels :: [Token] -> ([Inst], [(String, Int)])
processLabels prog = (map (\(_, TokenInst i) -> i) insts, map (\(addr, TokenLabel l) -> (l, addr)) labels)
    where (insts, labels) = partition isInst $ tail $ scanl address (0, TokenLabel "") prog
          isInst (_, (TokenInst _)) = True
          isInst (_, (TokenLabel _)) = False
          address (addr, _) t@(TokenInst _) = (addr + 1, t)
          address (addr, _) t@(TokenLabel _) = (addr, t)


main :: IO ()
main = do
    args <- getArgs

    if null args then do
        putStrLn "fatal error: no input files"
        exitFailure
    else
        pure ()

    input <- readFile $ head args 

    tokens <- case runParser parseProgram input of
                  Just (_, tokens) -> pure tokens
                  Nothing -> putStrLn "parse error" >> exitFailure

    let (prog, labels) = processLabels tokens

    res <- execProg prog labels
    case res of
        Right _ -> pure ()
        Left e -> putStrLn $ "ERROR: " ++ show e
