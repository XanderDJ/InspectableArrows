module SpaceShooter.Movement where

import Lens.Micro
import qualified SpaceShooter.Angle as A
import SpaceShooter.Objects
import SpaceShooter.Angle
import SpaceShooter.Point


maxSpeedPlayer = 500
accelerationPlayer = 300
decelerationPlayer = accelerationPlayer / 1.3
playerRotationSpeed = 235

class Acceleration a where
  accelerate :: Float -> a -> a
  decelerate :: Float -> a -> a

class RotationalMovement a where
  turnLeft :: Float -> a -> a
  absoluteLeftTurn :: Float -> a -> a
  turnRight :: Float -> a -> a
  absoluteRightTurn :: Float -> a -> a

class TranslationalMovement a where
  moveForward :: Float -> a -> a
  absoluteLeft :: Float -> a -> a
  absoluteRight :: Float -> a -> a
  absoluteForward :: Float -> a -> a
  absoluteBackward :: Float -> a -> a

absoluteForward' :: Float -> A.Angle -> Position -> Position
absoluteForward' delta angle (Point x y) = Point (x + dx) (y + dy)
  where
    dx = A.cosA angle * delta
    dy = A.sinA angle * delta

absoluteBackward' :: Float -> A.Angle -> Position -> Position
absoluteBackward' delta angle (Point x y) = Point (x - dx) (y - dy)
  where
    dx = A.cosA angle * delta
    dy = A.sinA angle * delta

absoluteRight' :: Float -> A.Angle -> Position -> Position
absoluteRight' delta angle (Point x y) = Point (x + dx) (y + dy)
  where
    rightAngle = A.turnRight angle 90
    dx = A.cosA rightAngle * delta
    dy = A.sinA rightAngle * delta

absoluteLeft' :: Float -> A.Angle -> Position -> Position
absoluteLeft' delta angle (Point x y) = Point (x + dx) (y + dy)
  where
    leftAngle = A.turnLeft angle 90
    dx = A.cosA angle * delta
    dy = A.sinA angle * delta

forwardT :: Float -> A.Angle -> Float -> Position -> Position
forwardT time heading vel (Point x y) = Point (x + dx * time) (y + dy * time)
 where 
   dx = A.cosA heading * vel
   dy = A.sinA heading * vel

instance TranslationalMovement Player where
  moveForward time p = p & pPosition .~ newPos
    where
      newPos = forwardT time (p ^. pAngle) (p ^. pVelocity) (p ^. pPosition)
  absoluteLeft delta p = p & pPosition .~ newPos
    where
      newPos = absoluteLeft' delta (p ^. pAngle) (p ^. pPosition)
  absoluteRight delta p = p & pPosition .~ newPos
    where
      newPos = absoluteRight' delta (p ^. pAngle) (p ^. pPosition)
  absoluteForward delta p = p & pPosition .~ newPos
    where
      newPos = absoluteForward' delta (p ^. pAngle) (p ^. pPosition)
  absoluteBackward delta p = p & pPosition .~ newPos
    where
      newPos = absoluteBackward' delta (p ^. pAngle) (p ^. pPosition)

instance TranslationalMovement Ship where
  moveForward time s = s & sPosition .~ newPos
    where
      newPos = forwardT time (s ^. sAngle) (s ^. sVelocity) (s ^. sPosition)
  absoluteLeft delta s = s & sPosition .~ newPos
    where
      newPos = absoluteLeft' delta (s ^. sAngle) (s ^. sPosition)
  absoluteRight delta s = s & sPosition .~ newPos
    where
      newPos = absoluteRight' delta (s ^. sAngle) (s ^. sPosition)
  absoluteForward delta s = s & sPosition .~ newPos
    where
      newPos = absoluteForward' delta (s ^. sAngle) (s ^. sPosition)
  absoluteBackward delta s = s & sPosition .~ newPos
    where
      newPos = absoluteBackward' delta (s ^. sAngle) (s ^. sPosition)

instance TranslationalMovement Bullet where
  moveForward time b = b & bPosition .~ newPos
    where
      newPos = forwardT time (b ^. bAngle) (b ^. bVelocity) (b ^. bPosition)
  absoluteLeft delta b = b & bPosition .~ newPos
    where
      newPos = absoluteLeft' delta (b ^. bAngle) (b ^. bPosition)
  absoluteRight delta b = b & bPosition .~ newPos
    where
      newPos = absoluteRight' delta (b ^. bAngle) (b ^. bPosition)
  absoluteForward delta b = b & bPosition .~ newPos
    where
      newPos = absoluteForward' delta (b ^. bAngle) (b ^. bPosition)
  absoluteBackward delta b = b & bPosition .~ newPos
    where
      newPos = absoluteBackward' delta (b ^. bAngle) (b ^. bPosition)

instance RotationalMovement Player where
  turnLeft time p = p & pAngle %~ A.turnLeft delta
    where
      delta = if p ^. pTurning == LEFT then time * playerRotationSpeed else 0
  turnRight time p = p & pAngle %~ A.turnRight delta
    where
      delta = if p ^. pTurning == RIGHT then time * playerRotationSpeed else 0
  absoluteLeftTurn delta p = p & pAngle %~ A.turnLeft delta
  absoluteRightTurn delta p = p & pAngle %~ A.turnRight delta

instance RotationalMovement Ship where
  turnLeft time s = s & sAngle %~ A.turnLeft (time * 90)
  absoluteLeftTurn delta s = s & sAngle %~ A.turnLeft delta
  turnRight time s = s & sAngle %~ A.turnRight (time * 90)
  absoluteRightTurn delta s = s & sAngle %~ A.turnRight delta

instance Acceleration Player where
  accelerate time p = if p ^. pAccelerating then p & pVelocity .~ newVelocity else p
    where
      newVel = p ^. pVelocity + (accelerationPlayer * time)
      exceedsMaxVel = p ^. pVelocity > maxSpeedPlayer
      newVelocity = if not exceedsMaxVel then newVel else maxSpeedPlayer
  decelerate time p = if not (p ^. pAccelerating) then p & pVelocity .~newVelocity else p
    where
      newVel = p ^. pVelocity - decelerationPlayer * time
      newVelocity = max 0 newVel

totalVelocity :: Velocity -> Float
totalVelocity (Vel vx vy) = sqrt (vx ** 2 + vy ** 2)

getTotalVelocity :: Angle -> Float -> Velocity
getTotalVelocity heading total = Vel (cosA heading * total) (sinA heading * total)