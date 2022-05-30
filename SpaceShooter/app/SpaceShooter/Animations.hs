{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE RankNTypes #-}

module SpaceShooter.Animations (moveForwardAnim, moveBulletsAnim, (>>-), (>->)) where

import Data.Functor.Const
import Lens.Micro (Lens', ix, lens, (^.), _1, _2)
import Game
import SpaceShooter.Function
    ( bounce, getSurfaceType, nextBouncePos, nextPos, outOfBounds )
import SpaceShooter.Data
    ( bCoords,
      bHeading,
      bTexture,
      pBullets,
      Bullet,
      GameState,
      Heading )

moveForwardAnim :: (Parallel f, Applicative f, LinearTo GameState f) => (Float, Float) -> Heading -> Lens' GameState (Float, Float) -> f ()
moveForwardAnim coords heading lens =
  let (toX, toY) = nextPos coords heading
   in linearTo (lens . _1) (For 0.3) (To toX) >>- linearTo (lens . _2) (For 0.3) (To toY)

{-

moveBulletsAnim :: (LinearTo GameState f, Parallel f, Applicative f, Set GameState f, SetTexture GameState f, Delay f, IfThenElse f) => GameState -> [Bullet] -> f ()
moveBulletsAnim gs bullets = moveBulletsRec gs bullets 0

moveBulletsRec :: (LinearTo GameState f, Parallel f, Applicative f, Set GameState f, SetTexture GameState f, Delay f, IfThenElse f) => GameState -> [Bullet] -> Int -> f ()
moveBulletsRec gs (bullet : bullets) index =
  let pos = bullet ^. bCoords
      heading = bullet ^. bHeading
      pos' = nextPos pos heading
      isOutOfBounds = pure $ outOfBounds pos' gs
   in ifThenElse isOutOfBounds (bounceBullet gs bullet index >>- moveBulletsRec gs bullets (index + 1)) (moveBulletForward bullet index >>- moveBulletsRec gs bullets (index + 1))
moveBulletsRec _ _ _ = pure ()

-}

-- | Animation that maps the moveBullet animation over the entire list keeping track of bullet ids and combines all the animations with the parrallell combinator
moveBulletsAnim :: (MapAnim f, Parallel f, LinearTo GameState f, IfThenElse f, Applicative f, Set GameState f, SetTexture GameState f, Delay f) => GameState -> [Bullet] -> f ()
moveBulletsAnim gs bs =
  let indexedBullets = zip bs ([0 ..] :: [Int])
   in mapAnimParr (uncurry (moveBullet gs)) indexedBullets

-- | Animation that takes a bullet and an int and makes the bullet bounce or go forward depending on position
moveBullet :: (Parallel f, LinearTo GameState f, IfThenElse f, Applicative f, Set GameState f, SetTexture GameState f, Delay f) => GameState -> Bullet -> Int -> f ()
moveBullet gs bullet bId =
  let pos = bullet ^. bCoords
      heading = bullet ^. bHeading
      pos' = nextPos pos heading
      isOutOfBounds = pure $ outOfBounds pos' gs
   in ifThenElse isOutOfBounds (bounceBullet gs bullet bId) (moveBulletForward bullet bId)

moveBulletForward :: (Parallel f, LinearTo GameState f) => Bullet -> Int -> f ()
moveBulletForward bullet index =
  let coords = bullet ^. bCoords
      heading = bullet ^. bHeading
      (toX, toY) = nextPos coords heading
   in linearTo (pBullets . ix index . bCoords . _1) (For 0.3) (To toX) >>- linearTo (pBullets . ix index . bCoords . _2) (For 0.3) (To toY)

bounceBullet :: (Parallel f, LinearTo GameState f, Applicative f, Set GameState f, SetTexture GameState f, Delay f) => GameState -> Bullet -> Int -> f ()
bounceBullet gs bullet index =
  let (x, y) = bullet ^. bCoords
      heading = bullet ^. bHeading
      (x', y') = nextPos (x, y) heading
      (midX, midY) = ((x' + x) / 2, (y' + y) / 2)
      (finalX, finalY) = nextBouncePos (x, y) heading (getSurfaceType gs (x, y) heading)
      l = ["Assets/Bullets/red_bullet.png", "Assets/Bullets/red_bullet_1.png", "Assets/Bullets/red_bullet_2.png", "Assets/Bullets/red_bullet_3.png", "Assets/Bullets/red_bullet_2.png", "Assets/Bullets/red_bullet_1.png", "Assets/Bullets/red_bullet.png"]
   in linearTo (pBullets . ix index . bCoords . _1) (For 0.15) (To midX)
        >>- linearTo (pBullets . ix index . bCoords . _2) (For 0.15) (To midY)
        >-> set (pBullets . ix' index . bHeading) (bounce heading (getSurfaceType gs (x, y) heading))
        >>- frameByFrame (pBullets . ix' index . bTexture) (0.15 / 6) l
        >>- linearTo (pBullets . ix index . bCoords . _1) (For 0.15) (To finalX)
        >>- linearTo (pBullets . ix index . bCoords . _2) (For 0.15) (To finalY)

-- HELPER FUNCTIONS

(>>-) :: (Parallel f) => f () -> f () -> f ()
(>>-) = parallel

(>->) :: (Applicative f) => f () -> f () -> f ()
(>->) = sequential

ix' :: Int -> Lens' [a] a
ix' i =
  lens
    (!! i) -- getter
    (\s b -> take i s ++ b : drop (i + 1) s) -- setter

