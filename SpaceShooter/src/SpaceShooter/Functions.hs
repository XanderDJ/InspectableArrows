module SpaceShooter.Functions  where

import Lens.Micro
import SpaceShooter.Angle
import SpaceShooter.Objects

updateShipOrientation :: Float -> Ship -> Ship
updateShipOrientation time ship = case ship ^. sMovement of
  (StraightLine timeLeft timeTotal) ->
    if time >= timeLeft
      then ship & sMovement .~ StraightLine timeTotal timeTotal & sAngle %~ oneEighty
      else ship & sMovement .~ StraightLine (timeLeft - time) timeTotal
  (RightSquare timeLeft timeTotal) ->
    if time >= timeLeft
      then ship & sMovement .~ RightSquare timeTotal timeTotal & sAngle %~ turnRight 90
      else ship & sMovement .~RightSquare (timeLeft - time) timeTotal
  (LeftSquare timeLeft timeTotal) ->
    if time >= timeLeft
      then ship & sMovement .~ LeftSquare timeTotal timeTotal & sAngle %~ turnLeft 90
      else ship & sMovement .~LeftSquare (timeLeft - time) timeTotal
  (Circle turnSpeed turnDirection) -> ship & sAngle %~ turnWithDirection turnDirection (turnSpeed * time)

turnWithDirection :: Turning -> Angle -> Angle -> Angle
turnWithDirection RIGHT = turnRight
turnWithDirection LEFT = turnLeft
turnWithDirection NO_TURNING = \a b -> b