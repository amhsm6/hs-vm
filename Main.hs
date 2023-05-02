import Core

prog :: [Inst]
prog = [ InstPush 0
       , InstPush 1
       , InstAdd
       , InstPrint
       , InstHlt
       ]

main :: IO ()
main = case execProg prog of
           Right io -> io
           Left e -> putStrLn $ "ERROR: " ++ show e
