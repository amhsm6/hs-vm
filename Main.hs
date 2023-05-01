import Core

prog :: [Inst]
prog = [ InstPush 0
       , InstPush 1
       , InstAdd
       , InstPrint
       , InstJmp 1
       ]

main :: IO ()
main = case execProg 100 prog of
           Right io -> io
           Left e -> putStrLn $ "ERROR: " ++ show e
