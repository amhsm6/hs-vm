module Action where

stackCapacity :: Int
stackCapacity = 1024 

initial :: MachineState
initial = MachineState { io = pure ()
                       , stack = []
                       , ip = 0
                       , halted = False
                       }

data MachineState = MachineState { io :: IO ()
                                 , stack :: [Int]
                                 , ip :: Int
                                 , halted :: Bool
                                 }

data MachineError = StackUnderflow
                  | StackOverflow
                  | DivByZeroError
                  deriving Show

newtype Action a = Action { runAction :: MachineState -> Either MachineError (MachineState, a) }

instance Monad Action where
    return = pure
    (Action a) >>= g = Action $ \s -> a s >>= \(s', x) -> runAction (g x) s'

instance Applicative Action where
    pure x = Action $ \s -> Right (s, x)
    a1 <*> a2 = a1 >>= \f -> a2 >>= pure . f

instance Functor Action where
    fmap f a = a >>= pure . f

get :: Action MachineState
get = Action $ \s -> Right (s, s)

put :: MachineState -> Action ()
put x = Action $ \_ -> Right (x, ())

die :: MachineError -> Action ()
die err = Action $ \_ -> Left err

getStack :: Action [Int]
getStack = get >>= pure . stack

putStack :: [Int] -> Action ()
putStack x = get >>= \s -> put $ s { stack = x }

push :: Int -> Action ()
push x = getStack >>= push'
    where push' s
              | length s == stackCapacity = die StackOverflow
              | otherwise = putStack $ s ++ [x]

pop :: Action Int
pop = getStack >>= pop'
    where pop' s
              | null s = die StackUnderflow >> pure 0
              | otherwise = putStack (init s) >> pure (last s)
