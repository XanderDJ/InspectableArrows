{-# LANGUAGE FlexibleInstances #-}
module Game.PaSe.Extensions where

import PaSe
import Data.Functor.Const
-- Extend DSL of Animations with a map function

class MapAnim f where
  mapAnimParr :: (a -> f ()) -> [a] -> f ()
  mapAnimSeq :: (a -> f ()) -> [a] -> f ()

instance Monoid m => MapAnim (Const m) where
  mapAnimParr f _ = Const (getConst (f undefined))
  mapAnimSeq f _ = Const (getConst (f undefined))

instance Monad m => MapAnim (Animation s m) where
  mapAnimParr _ [] = pure ()
  mapAnimParr f l = (foldl1 parallel . map f) l
  mapAnimSeq _ [] = pure ()
  mapAnimSeq f l = (foldl1 sequential . map f) l


class (MapAnim f, Parallel f, IfThenElse f, Applicative f, Delay f) => Animations f where

instance Animations (Const [Texture])
