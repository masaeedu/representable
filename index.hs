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

type f :~> g = forall a. f a -> g a
type Representation d k a = (d, k -> a)

class Representable d k f | f -> d, f -> k where
  represent :: f :~> Representation d k
  tabulate  :: Representation d k :~> f

index :: Representable d k f => f a -> k -> a
index = snd . represent

instance Representable Int Int [] where
  represent xs = (length xs, (!!) xs)
  tabulate (l, f) = f <$> [0..(l - 1)]

instance (Functor f, Representable d1 k1 f, Representable d2 k2 g)
         => Representable (f d2) (k1, k2) (Compose f g) where

  represent (Compose fga) = (fmap fst frg, f) where
    frg = fmap represent fga
    f (k1, k2) = ($ k2) . snd . represent . ($ k1) . snd . represent $ fga

  tabulate (fd2, f) = Compose x where
    x = let (d1, k1d2) = represent fd2
        in curry tabulate d1 $ \k1 -> curry tabulate (k1d2 k1) (\k2 -> f (k1, k2))

id_ :: Representable d k f => f a -> f a
id_ = tabulate . represent

main :: IO ()
main = print $ id_ $ Compose [[1, 2, 3, 4], [5, 6], [1, 2]]
-- => Compose [[1,2,3,4],[5,6],[1,2]]