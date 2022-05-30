module SpaceShooter.Angle where

import Data.Fixed

type Angle = Float


turnRight :: Angle -> Angle -> Angle
turnRight delta currentAngle = (currentAngle - delta) `mod'` 360

turnLeft :: Angle -> Angle -> Angle
turnLeft delta currentAngle = mod' (currentAngle + delta) 360

oneEighty :: Angle -> Angle
oneEighty = turnLeft 180

cosA :: Angle -> Float
cosA angle = if abs ans < 0.0001 then 0 else ans
 where radians = toRadians angle
       ans = cos radians

sinA :: Angle -> Float
sinA angle = if abs ans < 0.0001 then 0 else ans
 where radians = toRadians angle
       ans = sin radians
toRadians :: Angle -> Float
toRadians angle = pi * angle / 180