module STIO where

import Control.Applicative
import Control.Monad

newtype StateIO st a = StT (st -> IO (a,st))

instance Functor (StateIO st) where
  fmap f (StT st) = StT (\s -> do
    (a, s') <- st s
    return (f a, s'))

instance Applicative (StateIO s) where
  pure a = StT (\st -> return (a, st))
  (StT f) <*> (StT g) = StT (\s -> do
    (f', s') <- f s
    (a, s'') <- g s'
    return (f' a, s''))

instance Monad (StateIO s) where
  return x = pure x
  st >>= f = StT (\s -> do
    (x, s') <- app st s
    app (f x) s')

app :: StateIO s a -> s -> IO (a, s)
app (StT st) s = st s

stUpdate :: s -> StateIO s ()
stUpdate env' = StT (\_ -> return ((), env'))

stState :: StateIO s s
stState = StT (\st -> return (st, st))

lift :: IO a -> StateIO st a
lift mx = StT (\s -> do
  x <- mx
  return (x,s))
