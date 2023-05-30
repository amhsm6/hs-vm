module Engine where

import Control.Monad
import Control.Monad.IO.Class
import System.Posix.DynamicLinker
import Foreign.LibFFI
import Foreign.Ptr
import qualified Data.Map as M
import qualified Data.Vector as V
import Numeric

stackCapacity :: Int
stackCapacity = 1024 

executionLimit :: Int
executionLimit = 1024

type Address = Int

data Frame = FrameInt Integer
           | FrameFloat Float
           | FrameAddr Address
           | FramePtr Word

instance Show Frame where
    show (FrameInt x) = show x
    show (FrameFloat x) = show x
    show (FrameAddr x) = "#" ++ show x
    show (FramePtr x) = "0x" ++ showHex x ""

data MachineState = MachineState { stack :: V.Vector Frame
                                 , program :: V.Vector Inst
                                 , ip :: Address
                                 , zf :: Int
                                 , foreigns :: M.Map String ([Frame], Frame)
                                 , halted :: Bool
                                 , instsExecuted :: Int
                                 }

data MachineError = StackUnderflow
                  | StackOverflow
                  | DivByZeroError
                  | IllegalInstAccess
                  | TypeError
                  | IllegalForeignCall
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

die :: MachineError -> Action a
die err = Action $ \_ -> pure $ Left err

fetch :: Action Inst
fetch = get >>= \s -> let s' = s { instsExecuted = instsExecuted s + 1}
                      in maybe (die IllegalInstAccess) (\x -> put s' >> pure x) $ program s V.!? ip s

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

sez :: Action ()
sez = get >>= \s -> put $ s { zf = 1 }

clz :: Action ()
clz = get >>= \s -> put $ s { zf = 0 }

hlt :: Action ()
hlt = get >>= \s -> put $ s { halted = True }

push :: Frame -> Action ()
push x = getStack >>= push'
    where push' s
              | length s == stackCapacity = die StackOverflow
              | otherwise = putStack $ V.snoc s x

pop :: Action Frame
pop = getStack >>= pop'
    where pop' s
              | null s = die StackUnderflow
              | otherwise = putStack (V.init s) >> pure (V.last s)

getStack :: Action (V.Vector Frame)
getStack = get >>= pure . stack

putStack :: V.Vector Frame -> Action ()
putStack x = get >>= \s -> put $ s { stack = x }

popInt :: Action Integer
popInt = pop >>= f
    where f (FrameInt x) = pure x
          f _ = die TypeError

popFloat :: Action Float
popFloat = pop >>= f
    where f (FrameFloat x) = pure x
          f _ = die TypeError

popAddr :: Action Address
popAddr = pop >>= f
    where f (FrameAddr x) = pure x
          f _ = die TypeError

popPtr :: Action Word
popPtr = pop >>= f
    where f (FramePtr x) = pure x
          f _ = die TypeError

frameToArg :: Frame -> Arg
frameToArg (FrameInt x) = argInt64 $ fromIntegral x
frameToArg (FrameFloat x) = argCFloat $ realToFrac x
frameToArg (FramePtr x) = argPtr $ wordPtrToPtr $ WordPtr x
frameToArg _ = undefined

callForeign :: FunPtr a -> [Arg] -> Frame -> Action ()
callForeign fn args (FrameInt _) = do
    res <- liftIO $ callFFI fn retInt64 args
    push $ FrameInt $ fromIntegral res
callForeign fn args (FrameFloat _) = do
    res <- liftIO $ callFFI fn retCFloat args
    push $ FrameFloat $ realToFrac res
callForeign fn args (FramePtr _) = do
    res <- liftIO $ callFFI fn (retPtr retVoid) args
    let (WordPtr ptr) = ptrToWordPtr res
    push $ FramePtr ptr
callForeign _ _ _ = undefined

data Inst = InstPushI Integer
          | InstPushF Float
          | InstPop
          | InstDup Address
          | InstSwap Address
          | InstHlt
          | InstJmp Address
          | InstJmpZero Address
          | InstJmpNotZero Address
          | InstCall Address
          | InstRet
          | InstForeign String
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
exec (InstDup a) = do
    s <- getStack
    maybe (die StackUnderflow) push $ s V.!? (length s - a - 1)
    next
exec (InstSwap a) = do
    s <- getStack

    x <- maybe (die StackUnderflow) pure $ s V.!? (length s - a - 1)
    y <- pop

    putStack $ s V.// [(length s - 1, x), (length s - a - 1, y)]

    next
exec (InstJmp addr) = jmp addr
exec (InstJmpZero addr) = jz addr
exec (InstJmpNotZero addr) = jnz addr
exec (InstCall addr) = do
    get >>= push . FrameAddr . (+1) . ip
    jmp addr
exec InstRet = popAddr >>= jmp
exec (InstForeign name) = do
    foreignFunctions <- get >>= pure . foreigns
    case M.lookup name foreignFunctions of
        Nothing -> die IllegalForeignCall
        Just (argTypes, retType) -> do
            args <- forM (reverse argTypes) $ \t -> do
                x <- pop
                --TODO: t and x must match
                pure $ frameToArg x

            fn <- liftIO $ dlsym Default name
            callForeign fn (reverse args) retType
exec InstHlt = hlt
exec InstPrint = pop >>= liftIO . print >> next
exec InstAddI = do
    y <- popInt
    x <- popInt
    push $ FrameInt $ x + y
    next
exec InstSubI = do
    y <- popInt
    x <- popInt
    push $ FrameInt $ x - y
    next
exec InstMulI = do
    y <- popInt
    x <- popInt
    push $ FrameInt $ x * y
    next
exec InstDivI = do
    y <- popInt
    when (y == 0) $ die DivByZeroError
    x <- popInt
    push $ FrameInt $ x `div` y
    next
exec InstModI = do
    y <- popInt
    when (y == 0) $ die DivByZeroError
    x <- popInt
    push $ FrameInt $ x `mod` y
    next
exec InstGtI = do
    y <- popInt
    x <- popInt
    if x > y then sez else clz
    next
exec InstGeI = do
    y <- popInt
    x <- popInt
    if x >= y then sez else clz
    next
exec InstEqI = do
    y <- popInt
    x <- popInt
    if x == y then sez else clz
    next
exec InstLeI = do
    y <- popInt
    x <- popInt
    if x <= y then sez else clz
    next
exec InstLtI = do
    y <- popInt
    x <- popInt
    if x < y then sez else clz
    next
exec InstAddF = do
    y <- popFloat
    x <- popFloat
    push $ FrameFloat $ x + y
    next
exec InstSubF = do
    y <- popFloat
    x <- popFloat
    push $ FrameFloat $ x - y
    next
exec InstMulF = do
    y <- popFloat
    x <- popFloat
    push $ FrameFloat $ x * y
    next
exec InstDivF = do
    y <- popFloat
    x <- popFloat
    push $ FrameFloat $ x / y
    next
exec InstGtF = do
    y <- popFloat
    x <- popFloat
    if x > y then sez else clz
    next
exec InstGeF = do
    y <- popFloat
    x <- popFloat
    if x >= y then sez else clz
    next
exec InstEqF = do
    y <- popFloat
    x <- popFloat
    if x == y then sez else clz
    next
exec InstLeF = do
    y <- popFloat
    x <- popFloat
    if x <= y then sez else clz
    next
exec InstLtF = do
    y <- popFloat
    x <- popFloat
    if x < y then sez else clz
    next

initial :: [Inst] -> [(String, ([Frame], Frame))] -> MachineState
initial prog frgsDeclarations = MachineState { stack = V.empty
                                             , program = V.fromList prog
                                             , ip = 0
                                             , zf = 0
                                             , foreigns = M.fromList frgsDeclarations
                                             , halted = False
                                             , instsExecuted = 0
                                             }

execProg :: Address -> [Inst] -> [(String, ([Frame], Frame))] -> IO (Either MachineError (MachineState, ()))
execProg entry prog frgsDeclarations = runAction (jmp entry >> act) $ initial prog frgsDeclarations 
    where act = do
              fetch >>= exec
              s <- get
              unless (halted s || instsExecuted s > executionLimit) act
