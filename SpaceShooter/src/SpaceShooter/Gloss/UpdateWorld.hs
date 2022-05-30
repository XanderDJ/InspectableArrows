module SpaceShooter.Gloss.UpdateWorld (updateWorld) where

import Control.Arrow
import Lens.Micro
import ResourceManagement.InspectableArrow
import SpaceShooter.AddObjects
import SpaceShooter.Collision
import SpaceShooter.Functions
import SpaceShooter.Movement
import SpaceShooter.Objects
import SpaceShooter.Point
import SpaceShooter.Utility

updateWorld :: MapA a => a (Float, World) World
updateWorld =
  arr (uncurry updateSpawnOptions)
    >>> arr (uncurry updateShipShootDecisions)
    >>> arr (uncurry updateMovement)
    >>> arr handleCollisions
    >>> arr (\w -> (w, w ^. ships)) `mapA` ifA shipCanShootA shipShootsA returnA
    >>> whenA (shipsLessThan 4 `andA` canSpawn) addRandomShip
    >>> arr updateGameState

updateSpawnOptions :: Float -> World -> (Float, World)
updateSpawnOptions time w
  | not (w ^. canSpawnShip) && (w ^. canSpawnShipTimeLeft) < 0 = (,) time (w & canSpawnShipTimeLeft .~ (w ^. canSpawnShipTime) & canSpawnShip .~ True)
  | not (w ^. canSpawnShip) = (,) time (w & canSpawnShipTimeLeft -~ time)
  | otherwise = (time, w)

updateShipShootDecisions :: Float -> World -> (Float, World)
updateShipShootDecisions time w = (,) time $ w & ships . each . sShootDecision %~ f time
  where
    f time shootDecision@(SD timeLeft totalTime canShoot)
      | canShoot = shootDecision
      | time < timeLeft = SD (timeLeft - time) totalTime canShoot
      | time >= timeLeft = SD totalTime totalTime True
      | otherwise = shootDecision

updateMovement :: Float -> World -> World
updateMovement time w = case w ^. gameState of
  Playing ->
    w & player %~ (accelerate time >>> decelerate time >>> turnLeft time >>> turnRight time >>> moveForward time)
      & bullets . each %~ moveForward time
      & ships . each %~ updateShipOrientation time
      & ships . each %~ moveForward time
      & bullets %~ clearOutOfBoundsBullets w
      & player %~ keepInBoundaries w
      & ships %~ keepShipsInBoundaries w
  _ -> w

updateGameState :: World -> World
updateGameState world = world''
 where world' = if world ^. player . pHealth <= 0 then world & gameState .~ Defeat else world
       world'' = if world ^. totalScore > 200 then world & gameState .~ Victory else world'

clearOutOfBoundsBullets :: World -> [Bullet] -> [Bullet]
clearOutOfBoundsBullets world [] = []
clearOutOfBoundsBullets world (b : bs) =
  let Point x y = b ^. bPosition
      Border xL xH yL yH = world ^. borders
      outOfBounds = x < xL || x > xH || y < yL || y > yH
   in if outOfBounds then clearOutOfBoundsBullets world bs else b : clearOutOfBoundsBullets world bs

keepInBoundaries :: World -> Player -> Player
keepInBoundaries world player = player & pPosition . xCoord .~ x & pPosition . yCoord .~ y
  where
    Border xL xH yL yH = world ^. borders
    Point currentX currentY = player ^. pPosition
    x' = if currentX < xL then currentX + (abs xL + xH) else currentX
    x = if x' > xH then currentX - (abs xL + xH) else x'
    y' = if currentY < yL then currentY + (abs yL + yH) else currentY
    y = if y' > yH then y' - (abs yL + yH) else y'

keepShipsInBoundaries :: World -> [Ship] -> [Ship]
keepShipsInBoundaries world = map (keepShipInBoundaries world)

keepShipInBoundaries :: World -> Ship -> Ship
keepShipInBoundaries world ship = ship & sPosition .~ inBoundariesPosition
  where
    Border xL xH yL yH = world ^. borders
    Point currentX currentY = ship ^. sPosition
    x' = if currentX < xL then currentX + (abs xL + xH) else currentX
    x = if x' > xH then currentX - (abs xL + xH) else x'
    y' = if currentY < yL then currentY + (abs yL + yH) else currentY
    y = if y' > yH then y' - (abs yL + yH) else y'
    inBoundariesPosition = Point x y

noEntities :: TexturedArrow a => a World (Either World World)
noEntities = arr f
  where
    f world = if null (world ^. ships) then Right world else Left world

shipsLessThan :: Arrow a => Int -> a World (Either World World)
shipsLessThan n = arr f
  where
    f world = if length (world ^. ships) < n then Right world else Left world

canSpawn :: Arrow a => a World (Either World World)
canSpawn = arr (\world -> if world ^. canSpawnShip then Right world else Left world)

addNRandomShip :: TexturedArrow a => Int -> a World World
addNRandomShip 0 = arr id
addNRandomShip n = addRandomShip >>> addNRandomShip (n - 1)

addRandomShip :: TexturedArrow a => a World World
addRandomShip = randInt (1, 4) >>> addShip >>> arr (\world -> world & canSpawnShip .~ False)

addShip :: TexturedArrow a => a (World, Int) World
addShip =
  match (matchNumber 1) (useTexture fighterResources addShipAtRandomPos)
    <-> (matchNumber 2, useTexture cruiserResources addShipAtRandomPos)
    <-> (matchNumber 3, useTexture destroyerResources addShipAtRandomPos)
    <-> (matchNumber 4, useTexture bossResources addShipAtRandomPos)
    <-|-> arr fst

matchNumber :: TexturedArrow a => Int -> a (World, Int) (Either (World, Int) World)
matchNumber number = arr (\(world, n) -> if n == number then Right world else Left (world, n))

fighterResources :: ShipResources
fighterResources = SR 20 20 5 400 90 90 "Assets/Ships/fighter.png" Single 10 (SD 0.75 0.75 False)

cruiserResources :: ShipResources
cruiserResources = SR 50 30 10 300 90 90 "Assets/Ships/cruiser.png" (Burst 3) 5 (SD 1.25 1.25 False)

destroyerResources :: ShipResources
destroyerResources = SR 10 30 20 500 90 90 "Assets/Ships/destroyer.png" (Spread 2) 25 (SD 1 1 False)

bossResources :: ShipResources
bossResources = SR 100 30 30 200 90 90 "Assets/Ships/boss.png" (Spread 4) 30 (SD 2 2 False)

matchSingle :: TexturedArrow a => a (World, Ship) (Either (World, Ship) (World, Ship))
matchSingle = arr matchSingle'

matchSingle' :: (World, Ship) -> Either (World, Ship) (World, Ship)
matchSingle' (w, s@Ship {_sShootStyle = Single}) = Right (w, s)
matchSingle' other = Left other

matchSpread :: TexturedArrow a => a (World, Ship) (Either (World, Ship) (World, Ship))
matchSpread = arr matchSpread'

matchSpread' :: (World, Ship) -> Either (World, Ship) (World, Ship)
matchSpread' (w, s@Ship {_sShootStyle = Spread _}) = Right (w, s)
matchSpread' other = Left other

matchBurst :: TexturedArrow a => a (World, Ship) (Either (World, Ship) (World, Ship))
matchBurst = arr matchBurst'

matchBurst' :: (World, Ship) -> Either (World, Ship) (World, Ship)
matchBurst' (w, s@Ship {_sShootStyle = Burst _}) = Right (w, s)
matchBurst' other = Left other

shipShootsA :: TexturedArrow a => a (World, Ship) World
shipShootsA =
  match matchSingle (useTexture (BR "Assets/Bullets/red_bullet.png" 0 10) shipShoots)
    <-> (matchBurst, useTexture (BR "Assets/Bullets/green_bullet.png" 0 5) shipShoots)
    <-> (matchSpread, useTexture (BR "Assets/Bullets/blue_bullet.png" 0 20) shipShoots)
    <-|-> arr fst

shipCanShootA :: TexturedArrow a => a (World, Ship) (Either World (World, Ship))
shipCanShootA = arr (uncurry shipCanShoot)

shipCanShoot :: World -> Ship -> Either World (World, Ship)
shipCanShoot w s = if s ^. sShootDecision . canShoot then Right (w, s) else Left w