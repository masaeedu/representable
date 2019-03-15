{-# LANGUAGE ExistentialQuantification #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE UndecidableInstances #-}
{-# LANGUAGE FunctionalDependencies #-}
{-# LANGUAGE InstanceSigs #-}
{-# LANGUAGE ScopedTypeVariables #-}

import Data.Functor.Const
import Data.Functor.Compose
import Control.Applicative

import Data.Map

type f :~> g = forall a. f a -> g a
type Representation d k a = (d, k -> a)

class Representable d k f | f -> d, f -> k where
  represent :: f :~> Representation d k
  tabulate  :: Representation d k :~> f

index :: Representable d k f => k -> f a -> a
index = flip $ snd . represent

instance Representable Int Int [] where
  represent xs = (length xs, (!!) xs)
  tabulate (l, f) = f <$> [0..(l - 1)]

instance Ord k => Representable [k] k (Map k) where
  represent m = (keys m, (!) m)
  tabulate (ks, f) = fromList $ (\k -> (k, f k)) <$> ks

instance (Functor f, Representable d1 k1 f, Representable d2 k2 g)
         => Representable (f d2) (k1, k2) (Compose f g) where

  represent (Compose fga) = (domain, values) where
    domain = fmap (fst . represent) fga
    values (k1, k2) = index k2 . index k1 $ fga

  tabulate (fd2, f) = Compose x where
    x = let (d1, k1d2) = represent fd2
        in curry tabulate d1 $ \k1 -> curry tabulate (k1d2 k1) (\k2 -> f (k1, k2))

id_ :: Representable d k f => f a -> f a
id_ = tabulate . represent

main :: IO ()
main = do
  let example = Compose [[1, 2, 3, 4], [5, 6], [1, 2]]
  print $ id_ example
  -- => Compose [[1,2,3,4],[5,6],[1,2]]
  print $ index (0, 3) example
  -- => 4
  let example2 = Compose $ fromList [("foo", [1, 2, 3]), ("bar", [4, 5])]
  print $ id_ example2
  -- => Compose (fromList [("bar",[4,5]),("foo",[1,2,3])])
  print $ index ("foo", 2) example2
  -- => 3
