module CompilerState where

import Control.Applicative
import Control.Monad

import TAMCode (VarEnvironment)
import MiniTriangle (Identifier)

newtype CompilerState s a = S (s -> (a, s))

instance Functor (CompilerState s) where
  fmap f (S st) = S (\s -> let (a, s') = st s in (f a, s'))

instance Applicative (CompilerState s) where
  pure a = S (\st -> (a, st))
  (S f) <*> (S g) = S (\s -> let (f', s') = f s
                                 (a, s'') = g s'
                             in (f' a, s''))

instance Monad (CompilerState s) where
  return x = pure x
  st >>= f = S (\s -> let (x, s') = app st s
                      in app (f x) s')

app :: CompilerState s a -> s -> (a, s)
app (S st) s = st s

stUpdate :: s -> CompilerState s ()
stUpdate env' = S (\_ -> ((), env'))

stState :: CompilerState s s
stState = S (\st -> (st, st))

getInstructionsAndState :: CompilerState s a -> s -> (a, s)
getInstructionsAndState (S st) env = st env

freshLabel :: CompilerState Int Identifier
freshLabel = do
  n <- stState
  stUpdate (n+1)
  return ('#':(show n))
