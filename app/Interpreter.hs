{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE DeriveGeneric #-}

import Control.Monad
import System.Environment
import System.Exit
import GHC.Generics
import Data.Binary

import Engine

deriving instance Generic Inst
instance Binary Inst

main :: IO ()
main = do
    args <- getArgs

    when (null args) $ putStrLn "fatal error: no input files" >> exitFailure

    prog <- decodeFile $ head args

    res <- execProg prog
    case res of
        Right _ -> pure ()
        Left e -> putStrLn $ "ERROR: " ++ show e
