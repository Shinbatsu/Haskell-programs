{-# LANGUAGE NoImplicitPrelude #-}
{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}

module Monads where

import Data.Monoid
import Prelude hiding (Identity, Maybe (..), Monad, Reader, State, Writer)

class Monad m where
  return :: a -> m a
  (>>=) :: m a -> (a -> m b) -> m b

newtype Identity a = Identity a
  deriving (Show, Eq)

data Maybe a = Nothing | Just a
  deriving (Show, Eq)

instance Monad Identity where
  return = Identity
  (Identity v) >>= f = f v

instance Monad Maybe where
  return = Just
  Nothing >>= f = Nothing
  (Just v) >>= f = f v

newtype State s a = State {runState :: s -> (a, s)}

instance Monad (State s) where
  return a = State $ \s -> (a, s)
  m >>= k = State $ \s -> let (a, s') = runState m s in runState (k a) s'

newtype Reader s a = Reader {runReader :: s -> a}

instance Monad (Reader r) where
  return a = Reader $ const a
  m >>= k = Reader $ \r -> runReader (k (runReader m r)) r

newtype Writer w a = Writer {runWriter :: (w, a)}

instance Monoid w => Monad (Writer w) where
  return val = Writer (mempty, val)
  (Writer (log, val)) >>= f = Writer (log `mappend` log1, val1) where Writer (log1, val1) = f val