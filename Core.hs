module Core where

import Control.Monad.IO.Class

stackCapacity :: Int
stackCapacity = 1024 

executionLimit :: Int
executionLimit = 10 ^ 2

data MachineState = MachineState { stack :: [Int]
                                 , ip :: Int
                                 , program :: [Inst]
                                 , labels :: [(String, Int)]
                                 , halted :: Bool
                                 , instsExecuted :: Int
                                 , zf :: Int
                                 }

data MachineError = StackUnderflow
                  | StackOverflow
                  | DivByZeroError
                  | IllegalInstAccess
                  | LabelNotFoundError
                  deriving Show

newtype Action a = Action { runAction :: MachineState -> IO (Either MachineError (MachineState, a)) }

instance Monad Action where
    return = pure
    (Action f) >>= g = Action $ \s -> f s >>= \res -> case res of Right (s', x) -> runAction (g x) s'
                                                                  Left e -> pure $ Left e
instance Applicative Action where
    pure x = Action $ \s -> pure $ Right (s, x)
    a1 <*> a2 = a1 >>= \f -> a2 >>= pure . f

instance Functor Action where
    fmap f a = a >>= pure . f

instance MonadIO Action where   
    liftIO io = Action $ \s -> io >>= pure . Right . (s,)

get :: Action MachineState
get = Action $ \s -> pure $ Right (s, s)

put :: MachineState -> Action ()
put x = Action $ \_ -> pure $ Right (x, ())

die :: MachineError -> Action ()
die err = Action $ \_ -> pure $ Left err

fetch :: Action Inst
fetch = get >>= \s -> let x = ip s
                          y = instsExecuted s
                      in if x < 0 || x >= length (program s) then
                             die IllegalInstAccess >> pure InstHlt
                         else
                             put (s { instsExecuted = y + 1 }) >> pure (program s !! x)

next :: Action ()
next = get >>= \s -> put $ s { ip = ip s + 1 }

jmp :: Int -> Action ()
jmp x = get >>= \s -> put $ s { ip = x }

jz :: Int -> Action ()
jz x = get >>= \s -> case zf s of 1 -> jmp x
                                  0 -> next
                                  _ -> undefined

hlt :: Action ()
hlt = get >>= \s -> put $ s { halted = True }

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

sez :: Action ()
sez = get >>= \s -> put $ s { zf = 1 }

clz :: Action ()
clz = get >>= \s -> put $ s { zf = 0 }

getStack :: Action [Int]
getStack = get >>= pure . stack

putStack :: [Int] -> Action ()
putStack x = get >>= \s -> put $ s { stack = x }

data Inst = InstPush Int
          | InstPop
          | InstDup
          | InstHlt
          | InstJmp String
          | InstJmpZero String
          | InstPrint
          | InstAdd
          | InstSub
          | InstMul
          | InstDiv
          | InstMod
          | InstEq

exec :: Inst -> Action ()
exec (InstPush x) = push x >> next
exec InstPop = pop >> next
exec InstDup = pop >>= \x -> push x >> push x >> next
exec (InstJmp l) = get >>= \s -> case lookup l $ labels s of
                                     Just addr -> jmp addr
                                     Nothing -> die LabelNotFoundError
exec (InstJmpZero l) = get >>= \s -> case lookup l $ labels s of
                                         Just addr -> jz addr
                                         Nothing -> die LabelNotFoundError
exec InstHlt = hlt
exec InstPrint = pop >>= liftIO . print >> next
exec InstAdd = do
    y <- pop
    x <- pop
    push $ x + y
    next
exec InstSub = do
    y <- pop
    x <- pop
    push $ x - y
    next
exec InstMul = do
    y <- pop
    x <- pop
    push $ x * y
    next
exec InstDiv = do
    y <- pop
    if y == 0 then die DivByZeroError else pure ()
    x <- pop
    push $ x `div` y
    next
exec InstMod = do
    y <- pop
    if y == 0 then die DivByZeroError else pure ()
    x <- pop
    push $ x `mod` y
    next
exec InstEq = do
    y <- pop
    x <- pop
    if x == y then
        sez
    else
        clz
    next

initial :: [Inst] -> [(String, Int)] -> MachineState
initial program labels = MachineState { stack = []
                                      , ip = 0
                                      , program = program
                                      , labels = labels
                                      , halted = False
                                      , instsExecuted = 0
                                      , zf = 0
                                      }

execProg :: [Inst] -> [(String, Int)] -> IO (Either MachineError (MachineState, ()))
execProg prog labels = runAction act $ initial prog labels
    where act = do
              fetch >>= exec
              s <- get
              if halted s || instsExecuted s > executionLimit then
                  pure ()
              else
                  act
