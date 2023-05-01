module Instruction where

import Action

data Inst = InstPush Int
          | InstPop
          | InstPrint
          | InstAdd
          | InstSub
          | InstMul
          | InstDiv
          | InstMod

execInst :: Inst -> Action ()
execInst (InstPush x) = push x
execInst (InstPop) = pop >> pure ()
execInst (InstPrint) = get >>= \s -> let newio = print $ stack s
                                         oldio = io s
                                     in put $ s { io = oldio >> newio }
execInst (InstAdd) = do
    y <- pop
    x <- pop
    push $ x + y
execInst (InstSub) = do
    y <- pop
    x <- pop
    push $ x - y
execInst (InstMul) = do
    y <- pop
    x <- pop
    push $ x * y
execInst (InstDiv) = do
    y <- pop
    if y == 0 then die DivByZeroError else pure ()
    x <- pop
    push $ x `div` y
execInst (InstMod) = do
    y <- pop
    if y == 0 then die DivByZeroError else pure ()
    x <- pop
    push $ x `mod` y
