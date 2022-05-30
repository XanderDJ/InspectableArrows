{-# LANGUAGE TupleSections #-}

module SpaceShooter.Collision where

import Lens.Micro
import SpaceShooter.Circle
import SpaceShooter.Objects
import SpaceShooter.Utility

handleCollisions :: World -> World
handleCollisions world =
  ( handleBulletCollisions bulletCollisions
      . handleShipCollisions shipCollisions
      . handlePlayerCollisions playerCollisions
  )
    world
  where
    player' = world ^. player
    bs = world ^. bullets
    ships' = world ^. ships
    playerCollisions = filterZip touches bs player'
    bulletCollisions = concatMap (filterZip touches bs) bs
    shipCollisions = concatMap (filterZip touches bs) ships'

handlePlayerCollisions :: [(Player, Bullet)] -> World -> World
handlePlayerCollisions [] world = world
handlePlayerCollisions ((pl, bullet) : collisions) world =
  let player' = pl & pHealth -~ bullet ^. bPower
      world' = world & bullets %~ removeFirst bullet & player .~ player'
   in handlePlayerCollisions collisions world'

handleBulletCollisions :: [(Bullet, Bullet)] -> World -> World
handleBulletCollisions [] world = world
handleBulletCollisions ((bullet, collidingBullet) : collisions) world =
  let world' = world & bullets %~ removeFirst bullet & bullets %~ removeFirst collidingBullet
   in if bullet /= collidingBullet then handleBulletCollisions collisions world' else handleBulletCollisions collisions world

handleShipCollisions :: [(Ship, Bullet)] -> World -> World
handleShipCollisions [] world = world
handleShipCollisions ((s, bullet) : collisions) world =
  let s' = s & sHealth -~ bullet ^. bPower
      destroyed = s' ^. sHealth <= 0
      world' = world & bullets %~ removeFirst bullet & if destroyed then (totalScore +~ s ^. sValue) . (ships %~ removeFirst s) else ships %~ replace s s'
   in handleShipCollisions collisions world'

filterZip :: (a -> b -> Bool) -> [b] -> a -> [(a, b)]
filterZip f bs a = map (a,) (filter (f a) bs)
