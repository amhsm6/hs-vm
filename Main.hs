import Action
import Instruction

program :: [Inst]
program = [ InstPush 1
          , InstPush 2
          , InstAdd
          , InstPush 3
          , InstAdd
          , InstPrint
          ]

main :: IO ()
main = do
    let act = mapM_ execInst program
    case runAction act initial of
        Right (finalState, _) -> io finalState
        Left e -> putStrLn $ "ERROR: " ++ show e
