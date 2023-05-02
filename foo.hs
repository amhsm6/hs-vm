newtype Action a = Action { runAction :: MachineState -> Either MachineError (IO (MachineState, a)) }

StateT MachineState (EitherT MachineError)

(Action a) >>= g = Action $ \s -> a s >>= \io -> (io >>= \(s', x) -> pure $ runAction (g x) s')

pure x = Action $ \s -> Right $ pure $ (s, x)
