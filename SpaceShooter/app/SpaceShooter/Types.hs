{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE FunctionalDependencies #-}

module SpaceShooter.Types where

import SpaceShooter.Data
import SpaceShooter.Function
import Graphics.Gloss
import Graphics.Gloss.Interface.Pure.Game
import Lens.Micro 
import Game
import Control.Arrow
import Data.Functor.Const
import qualified Data.Set as S
import qualified Data.Map as Map
import ResourceLoader
import Control.Monad.Identity
import System.Random

class Renderable a where
  render :: a -> Maybe Picture -> Maybe Picture

instance Renderable Ship where
  render ship (Just picture) = Just $ (translate x y . rotate angle) picture
    where
      (x, y) = tileToCoords $ ship ^. sPos
      heading = ship ^. sHeading
      offset = ship ^. sOffset
      angle = getAngle heading + offset
  render _ Nothing = Nothing

instance Renderable Player where
  render player (Just picture) = Just $ rotate angle picture
    where
      heading = player ^. pHeading
      offset = player ^. pOffset
      angle = getAngle heading + offset
  render _ Nothing = Nothing

instance Renderable Bullet where
  render bullet (Just picture) = Just $ (translate x y . rotate angle) picture
    where
      pos = bullet ^. bCoords
      (x, y) = tileToCoords pos
      heading = bullet ^. bHeading
      offset = bullet ^. bOffset
      angle = getAngle heading + offset
  render _ Nothing = Nothing

instance Renderable Background where
  render background (Just picture) = Just $ translate x y picture
    where
     (x, y) = background ^. bgPosition
  render _ Nothing = Nothing

instance Renderable Border where
  render (Border t (x, y)) (Just picture) = Just $ translate x y picture
  render _ Nothing = Nothing   

instance Renderable Entity where
  render (EntityShip ship) = render ship
  render (EntityBullet bullet) = render bullet
  render (EntityBorder border) = render border


class (Arrow a1, Arrow a2, InitialiseObject f, InitialiseList f) => GlossInterface a1 a2 f | a2 -> f a1 where
  playI :: Display -> Color -> Int -> (StdGen -> f Game) -> (Game -> Picture) -> a1 (Event, Game) Game -> a1 (Float, Game) Game -> a2 [Texture] ()

instance GlossInterface (->) (Kleisli IO) Object where
  playI d c fps f render handleKeys updateWorld =   Kleisli texturesToMap >>> Kleisli (\tm -> do stdGen <- getStdGen; return (tm, stdGen)) >>> Kleisli (\(textureMap, stdGen) -> let game' = (getObject . f) stdGen & state . tMap .~ textureMap in play d c fps game' render (curry handleKeys) (curry updateWorld))

instance GlossInterface (ConstA (S.Set Texture)) (ConstA (S.Set Texture)) (Const (S.Set Texture )) where 
  playI _ _ _ f _ handleKeysConstA updateWorldConstA = ConstA $ getConst (f undefined) <> getConstA handleKeysConstA <> getConstA updateWorldConstA

-- ISSUE f GAME doesn't have the textures loaded yet. So we need to define a method a [Textures] (), that has a ConstA instance that gets all the textures from the game
-- the other instance needs to be a Kleisli IO instance that uses the textures to  

texturesToMap :: [Texture] -> IO (Map.Map Texture Picture)
texturesToMap ts = Map.fromList <$> mapM textureToPair ts


textureToPair :: Texture -> IO (Texture, Picture)
textureToPair asset = do
  pic <- loadAsset (getTexture asset)
  return (asset, pic)

instance Animations (Animation GameState Identity)

