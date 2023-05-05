newtype Action a = Action { runAction :: MachineState -> Either MachineError (MachineState, a) }

instance Monad Action where
    return = pure
    (Action a) >>= g = Action $ \s -> a s >>= \(s', x) -> runAction (g x) s'

instance Applicative Action where
    pure x = Action $ \s -> Right (s, x)
    a1 <*> a2 = a1 >>= \f -> a2 >>= pure . f

instance Functor Action where
    fmap f a = a >>= pure . f
