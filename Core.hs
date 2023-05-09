module Core where

import Control.Monad.IO.Class

stackCapacity :: Int
stackCapacity = 1024 

executionLimit :: Int
executionLimit = 10 ^ 3

data Frame = FrameInt Int
           | FrameFloat Float
           deriving Show

type Address = Int

data MachineState = MachineState { stack :: [Frame]
                                 , ip :: Address
                                 , program :: [Inst]
                                 , labels :: [(String, Address)]
                                 , halted :: Bool
                                 , instsExecuted :: Int
                                 , zf :: Int
                                 }

data MachineError = StackUnderflow
                  | StackOverflow
                  | DivByZeroError
                  | IllegalInstAccess
                  | LabelNotFoundError
                  | SomeError String
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

instance MonadFail Action where
    fail msg = die $ SomeError msg

get :: Action MachineState
get = Action $ \s -> pure $ Right (s, s)

put :: MachineState -> Action ()
put x = Action $ \_ -> pure $ Right (x, ())

die :: MachineError -> Action a
die err = Action $ \_ -> pure $ Left err

fetch :: Action Inst
fetch = get >>= \s -> let x = ip s
                          y = instsExecuted s
                      in if x < 0 || x >= length (program s) then
                             die IllegalInstAccess
                         else
                             put (s { instsExecuted = y + 1 }) >> pure (program s !! x)

next :: Action ()
next = get >>= \s -> put $ s { ip = ip s + 1 }

jmp :: Address -> Action ()
jmp x = get >>= \s -> put $ s { ip = x }

jz :: Address -> Action ()
jz x = get >>= \s -> case zf s of 1 -> jmp x
                                  0 -> next
                                  _ -> undefined

jnz :: Address -> Action ()
jnz x = get >>= \s -> case zf s of 0 -> jmp x
                                   1 -> next
                                   _ -> undefined

hlt :: Action ()
hlt = get >>= \s -> put $ s { halted = True }

push :: Frame -> Action ()
push x = getStack >>= push'
    where push' s
              | length s == stackCapacity = die StackOverflow
              | otherwise = putStack $ s ++ [x]

pop :: Action Frame
pop = getStack >>= pop'
    where pop' s
              | null s = die StackUnderflow
              | otherwise = putStack (init s) >> pure (last s)

sez :: Action ()
sez = get >>= \s -> put $ s { zf = 1 }

clz :: Action ()
clz = get >>= \s -> put $ s { zf = 0 }

getStack :: Action [Frame]
getStack = get >>= pure . stack

putStack :: [Frame] -> Action ()
putStack x = get >>= \s -> put $ s { stack = x }

data Inst = InstPushI Int
          | InstPushF Float
          | InstPop
          | InstDup
          | InstHlt
          | InstJmp String
          | InstJmpZero String
          | InstJmpNoZero String
          | InstPrint
          | InstAddI
          | InstSubI
          | InstMulI
          | InstDivI
          | InstModI
          | InstGtI
          | InstGeI
          | InstEqI
          | InstLeI
          | InstLtI
          | InstAddF
          | InstSubF
          | InstMulF
          | InstDivF
          | InstGtF
          | InstGeF
          | InstEqF
          | InstLeF
          | InstLtF

exec :: Inst -> Action ()
exec (InstPushI x) = push (FrameInt x) >> next
exec (InstPushF x) = push (FrameFloat x) >> next
exec InstPop = pop >> next
exec InstDup = pop >>= \x -> push x >> push x >> next
exec (InstJmp l) = get >>= \s -> case lookup l $ labels s of --TODO: Replace labels with addresses at parsing
                                     Just addr -> jmp addr
                                     Nothing -> die LabelNotFoundError
exec (InstJmpZero l) = get >>= \s -> case lookup l $ labels s of
                                         Just addr -> jz addr
                                         Nothing -> die LabelNotFoundError
exec (InstJmpNoZero l) = get >>= \s -> case lookup l $ labels s of
                                           Just addr -> jnz addr
                                           Nothing -> die LabelNotFoundError
exec InstHlt = hlt
exec InstPrint = pop >>= liftIO . print >> next
exec InstAddI = do --TODO: Report type error
    (FrameInt y) <- pop
    (FrameInt x) <- pop
    push $ FrameInt $ x + y
    next
exec InstSubI = do
    (FrameInt y) <- pop
    (FrameInt x) <- pop
    push $ FrameInt $ x - y
    next
exec InstMulI = do
    (FrameInt y) <- pop
    (FrameInt x) <- pop
    push $ FrameInt $ x * y
    next
exec InstDivI = do
    (FrameInt y) <- pop
    if y == 0 then die DivByZeroError else pure ()
    (FrameInt x) <- pop
    push $ FrameInt $ x `div` y
    next
exec InstModI = do
    (FrameInt y) <- pop
    if y == 0 then die DivByZeroError else pure ()
    (FrameInt x) <- pop
    push $ FrameInt $ x `mod` y
    next
exec InstGtI = do
    (FrameInt y) <- pop
    (FrameInt x) <- pop
    if x > y then sez else clz
    next
exec InstGeI = do
    (FrameInt y) <- pop
    (FrameInt x) <- pop
    if x >= y then sez else clz
    next
exec InstEqI = do
    (FrameInt y) <- pop
    (FrameInt x) <- pop
    if x == y then sez else clz
    next
exec InstLeI = do
    (FrameInt y) <- pop
    (FrameInt x) <- pop
    if x <= y then sez else clz
    next
exec InstLtI = do
    (FrameInt y) <- pop
    (FrameInt x) <- pop
    if x < y then sez else clz
    next
exec InstAddF = do
    (FrameFloat y) <- pop
    (FrameFloat x) <- pop
    push $ FrameFloat $ x + y
    next
exec InstSubF = do
    (FrameFloat y) <- pop
    (FrameFloat x) <- pop
    push $ FrameFloat $ x - y
    next
exec InstMulF = do
    (FrameFloat y) <- pop
    (FrameFloat x) <- pop
    push $ FrameFloat $ x * y
    next
exec InstDivF = do
    (FrameFloat y) <- pop
    (FrameFloat x) <- pop
    push $ FrameFloat $ x / y
    next
exec InstGtF = do
    (FrameFloat y) <- pop
    (FrameFloat x) <- pop
    if x > y then sez else clz
    next
exec InstGeF = do
    (FrameFloat y) <- pop
    (FrameFloat x) <- pop
    if x >= y then sez else clz
    next
exec InstEqF = do
    (FrameFloat y) <- pop
    (FrameFloat x) <- pop
    if x == y then sez else clz
    next
exec InstLeF = do
    (FrameFloat y) <- pop
    (FrameFloat x) <- pop
    if x <= y then sez else clz
    next
exec InstLtF = do
    (FrameFloat y) <- pop
    (FrameFloat x) <- pop
    if x < y then sez else clz
    next

initial :: [Inst] -> [(String, Address)] -> MachineState
initial program labels = MachineState { stack = []
                                      , ip = 0
                                      , program = program
                                      , labels = labels
                                      , halted = False
                                      , instsExecuted = 0
                                      , zf = 0
                                      }

execProg :: [Inst] -> [(String, Address)] -> IO (Either MachineError (MachineState, ()))
execProg prog labels = runAction act $ initial prog labels
    where act = do
              fetch >>= exec
              s <- get
              if halted s || instsExecuted s > executionLimit then
                  pure ()
              else
                  act
