module CompilerState where

import Control.Applicative
import Control.Monad

import TAMCode (VarEnvironment)

newtype CompilerState a = S (VarEnvironment -> (a, VarEnvironment))

instance Functor CompilerState where
  fmap f (S st) = S (\s -> let (a, s') = st s in (f a, s'))

instance Applicative CompilerState where
  pure a = S (\st -> (a, st))
  (S f) <*> (S g) = S (\s -> let (f', s') = f s
                                 (a, s'') = g s'
                             in (f' a, s''))

instance Monad CompilerState where
  return x = pure x
  st >>= f = S (\s -> let (x, s') = app st s
                      in app (f x) s')

app :: CompilerState a -> VarEnvironment -> (a, VarEnvironment)
app (S st) s = st s

stUpdate :: VarEnvironment -> CompilerState ()
stUpdate env' = S (\_ -> ((), env'))

stState :: CompilerState VarEnvironment
stState = S (\st -> (st, st))

getInstructions :: CompilerState a -> VarEnvironment -> a
getInstructions st env = fst (app st env)
