module Engine where

import Control.Monad
import Control.Monad.IO.Class
import qualified Data.Map as M
import qualified Data.Vector as V
import Numeric
import System.Posix.DynamicLinker
import Foreign
import Foreign.LibFFI

stackCapacity :: Int
stackCapacity = 1024 

executionLimit :: Int
executionLimit = 1024

newtype Action a = Action { runAction :: MachineState -> IO (Either MachineError (MachineState, a)) }

instance Monad Action where
    return = pure
    (Action f) >>= g = Action $ \s -> f s >>= either (pure . Left) (\(s', x) -> runAction (g x) s')

instance Applicative Action where
    pure x = Action $ \s -> pure $ Right (s, x)
    m1 <*> m2 = m1 >>= \f -> m2 >>= pure . f

instance Functor Action where
    fmap f m = m >>= pure . f

instance MonadIO Action where   
    liftIO io = Action $ \s -> io >>= pure . Right . (s,)

data MachineState = MachineState { stack :: V.Vector Frame
                                 , program :: V.Vector Inst
                                 , ip :: Address
                                 , zf :: Int
                                 , foreignFunctions :: M.Map String ([Frame], Frame)
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

type Address = Int

data Frame = FrameAddr Address
           | FrameInt Integer
           | FrameByte Word8
           | FrameFloat Float
           | FramePtr Word
           deriving (Ord, Eq)

instance Num Frame where
    (FrameInt a) + (FrameInt b) = FrameInt $ a + b
    (FrameByte a) + (FrameByte b) = FrameByte $ a + b
    (FrameFloat a) + (FrameFloat b) = FrameFloat $ a + b
    (FramePtr a) + (FramePtr b) = FramePtr $ a + b
    _ + _ = undefined

    (FrameInt a) * (FrameInt b) = FrameInt $ a * b
    (FrameByte a) * (FrameByte b) = FrameByte $ a * b
    (FrameFloat a) * (FrameFloat b) = FrameFloat $ a * b
    (FramePtr a) * (FramePtr b) = FramePtr $ a * b
    _ * _ = undefined

    negate (FrameInt x) = FrameInt $ -x
    negate (FrameByte x) = FrameByte $ -x
    negate (FrameFloat x) = FrameFloat $ -x
    negate (FramePtr x) = FramePtr $ -x
    negate _ = undefined

    abs (FrameInt x) = FrameInt $ abs x
    abs (FrameByte x) = FrameByte $ abs x
    abs (FrameFloat x) = FrameFloat $ abs x
    abs (FramePtr x) = FramePtr $ abs x
    abs _ = undefined

    signum (FrameInt x) = FrameInt $ signum x
    signum (FrameByte x) = FrameByte $ signum x
    signum (FrameFloat x) = FrameFloat $ signum x
    signum (FramePtr x) = FramePtr $ signum x
    signum _ = undefined

    fromInteger = FrameInt

instance Show Frame where
    show (FrameAddr x) = "#" ++ show x
    show (FrameInt x) = show x
    show (FrameByte x) = show x
    show (FrameFloat x) = show x
    show (FramePtr x) = "0x" ++ showHex x ""

get :: Action MachineState
get = Action $ \s -> pure $ Right (s, s)

put :: MachineState -> Action ()
put x = Action $ \_ -> pure $ Right (x, ())

die :: MachineError -> Action a
die err = Action $ \_ -> pure $ Left err

fetch :: Action Inst
fetch = get >>= \s -> let s' = s { instsExecuted = instsExecuted s + 1}
                      in maybe (die IllegalInstAccess) (\x -> put s' >> pure x) $ program s' V.!? ip s'

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
getStack = stack <$> get

putStack :: V.Vector Frame -> Action ()
putStack x = get >>= \s -> put $ s { stack = x }

popAddr :: Action Address
popAddr = pop >>= f
    where f (FrameAddr x) = pure x
          f _ = die TypeError

popInt :: Action Integer
popInt = pop >>= f
    where f (FrameInt x) = pure x
          f _ = die TypeError

popByte :: Action Word8
popByte = pop >>= f
    where f (FrameByte x) = pure x
          f _ = die TypeError

popFloat :: Action Float
popFloat = pop >>= f
    where f (FrameFloat x) = pure x
          f _ = die TypeError

popPtr :: Action Word
popPtr = pop >>= f
    where f (FramePtr x) = pure x
          f _ = die TypeError

checkType :: Frame -> Frame -> Action ()
checkType (FrameInt _) (FrameInt _) = pure ()
checkType (FrameByte _) (FrameByte _) = pure ()
checkType (FrameFloat _) (FrameFloat _) = pure ()
checkType (FramePtr _) (FramePtr _) = pure ()
checkType _ _ = die TypeError

frameToArg :: Frame -> Arg
frameToArg (FrameInt x) = argInt64 $ fromIntegral x
frameToArg (FrameByte x) = argWord8 x
frameToArg (FrameFloat x) = argCFloat $ realToFrac x
frameToArg (FramePtr x) = argPtr $ wordPtrToPtr $ WordPtr x
frameToArg _ = undefined

callForeign :: FunPtr a -> [Arg] -> Frame -> Action ()
callForeign fn args (FrameInt _) = do
    res <- liftIO $ callFFI fn retInt64 args
    push $ FrameInt $ fromIntegral res
callForeign fn args (FrameByte _) = do
    res <- liftIO $ callFFI fn retWord8 args
    push $ FrameByte res
callForeign fn args (FrameFloat _) = do
    res <- liftIO $ callFFI fn retCFloat args
    push $ FrameFloat $ realToFrac res
callForeign fn args (FramePtr _) = do
    res <- liftIO $ callFFI fn (retPtr retVoid) args
    let (WordPtr ptr) = ptrToWordPtr res
    push $ FramePtr ptr
callForeign _ _ _ = undefined

data Inst = InstPushI Integer
          | InstPushB Word8
          | InstPushF Float
          | InstPushP Word

          | InstDrop
          | InstDup Address
          | InstSwap Address

          | InstJmp Address
          | InstJmpZero Address
          | InstJmpNotZero Address

          | InstCall Address
          | InstRet

          | InstForeign String

          | InstLoadI
          | InstLoadB
          | InstLoadF

          | InstStoreI
          | InstStoreB
          | InstStoreF

          | InstHlt

          | InstPrint

          | InstAdd
          | InstSub
          | InstMul
          | InstDivI
          | InstMod
          | InstDivF

          | InstGt
          | InstGe
          | InstEq
          | InstLe
          | InstLt

exec :: Inst -> Action ()

exec (InstPushI x) = push (FrameInt x) >> next
exec (InstPushB x) = push (FrameByte x) >> next
exec (InstPushF x) = push (FrameFloat x) >> next
exec (InstPushP x) = push (FramePtr x) >> next

exec InstDrop = pop >> next
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
    foreigns <- foreignFunctions <$> get
    case M.lookup name foreigns of
        Nothing -> die IllegalForeignCall
        Just (argTypes, retType) -> do
            args <- forM (reverse argTypes) $ \t -> do
                x <- pop
                checkType t x
                pure $ frameToArg x

            fn <- liftIO $ dlsym Default name
            callForeign fn (reverse args) retType

            next

exec InstLoadI = popPtr >>= liftIO . peek . wordPtrToPtr . WordPtr >>= push . FrameInt . (fromIntegral :: Int64 -> Integer) >> next
exec InstLoadB = popPtr >>= liftIO . peek . wordPtrToPtr . WordPtr >>= push . FrameByte >> next
exec InstLoadF = popPtr >>= liftIO . peek . wordPtrToPtr . WordPtr >>= push . FrameFloat >> next

exec InstStoreI = do
    val <- popInt
    ptr <- popPtr
    liftIO $ poke (wordPtrToPtr $ WordPtr ptr) (fromIntegral val :: Int64)
    next
exec InstStoreB = do
    val <- popByte
    ptr <- popPtr
    liftIO $ poke (wordPtrToPtr $ WordPtr ptr) val
    next
exec InstStoreF = do
    val <- popFloat
    ptr <- popPtr
    liftIO $ poke (wordPtrToPtr $ WordPtr ptr) val
    next

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
exec InstDivI = do
    y <- popInt
    when (y == 0) $ die DivByZeroError
    x <- popInt
    push $ FrameInt $ x `div` y
    next
exec InstMod = do
    y <- popInt
    when (y == 0) $ die DivByZeroError
    x <- popInt
    push $ FrameInt $ x `mod` y
    next
exec InstDivF = do
    y <- popFloat
    x <- popFloat
    push $ FrameFloat $ x / y
    next

exec InstGt = do
    y <- pop
    x <- pop
    if x > y then sez else clz
    next
exec InstGe = do
    y <- pop
    x <- pop
    if x >= y then sez else clz
    next
exec InstEq = do
    y <- pop
    x <- pop
    if x == y then sez else clz
    next
exec InstLe = do
    y <- pop
    x <- pop
    if x <= y then sez else clz
    next
exec InstLt = do
    y <- pop
    x <- pop
    if x < y then sez else clz
    next

initial :: [Inst] -> [(String, ([Frame], Frame))] -> MachineState
initial prog foreigns = MachineState { stack = V.empty
                                     , program = V.fromList prog
                                     , ip = 0
                                     , zf = 0
                                     , foreignFunctions = M.fromList foreigns
                                     , halted = False
                                     , instsExecuted = 0
                                     }

execProg :: Address -> [Inst] -> [(String, ([Frame], Frame))] -> IO (Either MachineError (MachineState, ()))
execProg entry prog foreigns = runAction (jmp entry >> act) $ initial prog foreigns
    where act = do
              fetch >>= exec
              s <- get
              unless (halted s || instsExecuted s > executionLimit) act
