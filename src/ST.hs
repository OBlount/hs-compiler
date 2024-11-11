module ST where

import Control.Applicative
import Control.Monad

newtype ST s a = S (s -> (a, s))

instance Functor (ST s) where
  fmap f (S st) = S (\s -> let (a, s') = st s in (f a, s'))

instance Applicative (ST s) where
  pure a = S (\st -> (a, st))
  (S f) <*> (S g) = S (\s -> let (f', s') = f s
                                 (a, s'') = g s'
                             in (f' a, s''))

instance Monad (ST s) where
  return = pure
  st >>= f = S (\s -> let (x, s') = app st s
                      in app (f x) s')

app :: ST s a -> s -> (a, s)
app (S st) s = st s

stUpdate :: s -> ST s ()
stUpdate env' = S (\_ -> ((), env'))

stState :: ST s s
stState = S (\st -> (st, st))
