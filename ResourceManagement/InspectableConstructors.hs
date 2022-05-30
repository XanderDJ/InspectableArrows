{-# LANGUAGE FlexibleInstances #-}
module ResourceManagement.InspectableConstructors where
import Control.Applicative
import qualified Data.Set as S
import ResourceManagement.ResourceIdentifier
import qualified Data.Text as T

newtype Object a = Object { getObject :: a} deriving (Show, Eq)

instance Functor Object where
  fmap f (Object a) = Object $ f a

instance Applicative Object where
  pure a = Object a
  (Object f) <*> (Object a) = Object $ f a

instance Semigroup a => Semigroup (Object a) where
  (Object a1) <> (Object a2) = Object $ a1 <> a2

instance Monoid a => Monoid (Object a) where
  mempty = Object mempty 

class Applicative a => InitialiseObject a where
  initialiseTexture :: HasResource r => a (r -> b) -> r -> a b
  addFilePath :: a (T.Text -> b) -> T.Text -> a b

instance InitialiseObject Object where
  initialiseTexture (Object f) t = Object $ f t
  addFilePath (Object f) fp = Object $ f fp

instance InitialiseObject (Const (S.Set ResourceIdentifier)) where
  initialiseTexture (Const ts) t = Const $ S.insert (RI (getResourceFilePath t) (getResourceType t)) ts
  addFilePath (Const ts) fp = Const $ S.insert (RI fp Other) ts


class InitialiseObject a => InitialiseList a where
  addObject :: a [b] -> a b -> a [b]
  addTexturedObject :: HasResource r => a [b] -> (r, r -> b) -> a [b]
  initialList :: a [b]

instance InitialiseList Object where
  addObject (Object bs) (Object b) = Object $ b : bs
  addTexturedObject (Object bs) (t, f) = Object $ f t : bs
  initialList = pure []

instance InitialiseList (Const (S.Set ResourceIdentifier)) where
  addObject (Const ts1) (Const ts2) = Const $ ts1 <> ts2
  addTexturedObject (Const ts1) (t, _) = Const $ S.insert (RI (getResourceFilePath t) (getResourceType t)) ts1
  initialList = Const S.empty