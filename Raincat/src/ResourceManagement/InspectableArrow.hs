{-# LANGUAGE Arrows #-}
{-# LANGUAGE FlexibleInstances #-}

module ResourceManagement.InspectableArrow where

import Control.Arrow
import qualified Control.Category as C
import Control.Monad
import Data.Either
import Data.List (foldl')
import qualified Data.Set as S
import qualified Data.Text as T
import ResourceManagement.ResourceIdentifier
import Resource.Resource

newtype ConstA a i o = ConstA {getConstA :: a} deriving (Show, Eq, Ord)

instance Monoid a => C.Category (ConstA a) where
  id = ConstA mempty
  (.) (ConstA a1) (ConstA a2) = ConstA (a1 <> a2)

instance Monoid a => Arrow (ConstA a) where
  arr _ = ConstA mempty
  first (ConstA a) = ConstA a

instance Monoid a => ArrowChoice (ConstA a) where
  left (ConstA a) = ConstA a

class (ArrowChoice a) => PatternedArrow a where
  match :: a b (Either b c) -> a c d -> a b (Either b d)
  (<->) :: a b (Either b d) -> (a b (Either b c), a c d) -> a b (Either b d)
  (<-|->) :: a b (Either b c) -> a b c -> a b c

instance PatternedArrow (->) where
  match a1 a2 = a1 >>> right a2
  (<->) a1 (a2, a3) = a1 >>> (a2 >>> right a3) ||| arr Right
  (<-|->) a1 a2 = a1 >>> a2 ||| arr Prelude.id

instance Monoid a => PatternedArrow (ConstA a) where
  match (ConstA a1) (ConstA a2) = ConstA $ a1 <> a2
  (<->) (ConstA a1) (ConstA a2, ConstA a3) = ConstA $ a1 <> a2 <> a3
  (<-|->) (ConstA a1) (ConstA a2) = ConstA $ a1 <> a2

instance Monad m => PatternedArrow (Kleisli m) where
  match a1 a2 = a1 >>> right a2
  (<->) a1 (a2, a3) = a1 >>> (a2 >>> right a3) ||| arr Right
  (<-|->) a1 a2 = a1 >>> a2 ||| arr Prelude.id

class MapA a => TexturedArrow a where
  useTexture :: HasResource r => r -> (r -> b -> c) -> a b c
  useTextures :: HasResources r => r -> (r -> b -> c) -> a b c

instance TexturedArrow (ConstA (S.Set ResourceIdentifier)) where
  useTexture r _ = ConstA (S.singleton (RI (getResourceFilePath r) (getResourceType r)))
  useTextures r _ = ConstA (S.fromList (getResourceIdentifiers r))

instance TexturedArrow (->) where
  useTexture r f = f r
  useTextures r f = f r

instance Monad m => TexturedArrow (Kleisli m) where
  useTexture r f = arr (f r)
  useTextures r f = arr (f r)


class MapA a => RaincatArrow a where
  useResource :: HasResourceConfig rc => rc -> (rc -> b -> c) -> a b c
  useResources :: HasResourceConfigs rc => rc -> (rc -> b -> c) -> a b c

instance RaincatArrow (->) where
  useResource rc f = f rc
  useResources rc f = f rc

instance RaincatArrow (ConstA (S.Set ResourceConfig)) where
  useResource r _ = ConstA (S.singleton (getResourceConfig r))
  useResources r _ = ConstA (S.fromList (getResourceConfigs r))

-- If implemented for Textured Arrows
ifA :: ArrowChoice a => a b (Either c d) -> a d e -> a c e -> a b e
ifA a b1 b2 = a >>> b2 ||| b1

notA :: ArrowChoice a => a (Either c d) (Either d c)
notA = arr swap
  where
    swap (Right x) = Left x
    swap (Left x) = Right x

whenA :: ArrowChoice a => a b (Either b c) -> a c b -> a b b
whenA bool action = bool >>> arr id ||| action

andA :: ArrowChoice a => a b (Either c d) -> a b (Either c d) -> a b (Either c d)
andA a1 a2 = proc b -> do
  r1 <- a1 -< b
  if isRight r1
    then do
      r2 <- a2 -< b
      returnA -< r2
    else returnA -< r1

orA :: ArrowChoice a => a b (Either c d) -> a b (Either c d) -> a b (Either c d)
orA a1 a2 = proc b -> do
  r1 <- a1 -< b
  if isLeft r1
    then do
      r2 <- a2 -< b
      returnA -< r2
    else returnA -< r1

class PatternedArrow a => MapA a where
  mapA :: a b (b, [c]) -> a (b, c) b -> a b b

instance MapA (->) where
  mapA toList mapF b = b''
    where
      (b', cs) = toList b
      b'' = foldl' (curry mapF) b' cs

instance Monad m => MapA (Kleisli m) where
  mapA toList mapF = Kleisli $ \b -> do
    (b', cs) <- (runKleisli toList) b
    b'' <- foldM ((curry . runKleisli) mapF) b' cs
    return b''


instance Monoid a => MapA (ConstA a) where
  mapA (ConstA rs) (ConstA rs') = ConstA (rs <> rs')
