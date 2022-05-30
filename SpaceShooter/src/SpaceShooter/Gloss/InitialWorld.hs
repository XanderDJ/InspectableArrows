module SpaceShooter.Gloss.InitialWorld (initialWorld, initialPlayer) where

import qualified Data.Map as M
import ResourceManagement.InspectableConstructors
import SpaceShooter.Objects
import SpaceShooter.Point
import System.Random

initialPlayer :: InitialiseObject f => f Player
initialPlayer =
  pure (Player 100 50 (Point 0 0) 0 0 90)
    `addFilePath` "Assets/Ships/player.png"
      <*> pure Nothing
      <*> pure False
      <*> pure NO_TURNING
      <*> pure Single
      <*> pure 10

initialBackground :: InitialiseObject f => f Background
initialBackground = pure (Bg (Point 0 0)) `addFilePath` "Assets/Background/fullspace.jpg"

initialWorld :: InitialiseObject f => StdGen -> f World
initialWorld randomGen = World randomGen Playing 0 (Border (-950) 950 (-500) 500) False 1 1 <$> initialPlayer <*> pure [] <*> pure [] <*> pure M.empty <*> initialBackground