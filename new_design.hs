data MachineState = MachineState { stack :: [Int]
                                 , ip :: Int
                                 , program :: [Int]
                                 , halted :: Bool
                                 }

data MachineError = StackUnderflow
                  | StackOverflow
                  | DivByZeroError
                  | IllegalInstAccess
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

main :: IO ()
main = pure ()
