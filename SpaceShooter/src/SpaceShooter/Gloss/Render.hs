{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE RecordWildCards #-}

module SpaceShooter.Gloss.Render (renderWorld) where

import qualified Data.Map as M
import Data.Maybe
import qualified Data.Text as T
import Graphics.Gloss
import Lens.Micro
import SpaceShooter.Angle
import SpaceShooter.Point
import SpaceShooter.Movement
import SpaceShooter.Objects

class Renderable a where
  render :: M.Map T.Text Picture -> a -> Maybe Picture

instance Renderable Player where
  render pictureMap player = Just $ pictures (catMaybes [playerPicture, boosterPicture])
    where
      playerPicture = M.lookup (player ^. pTexture) pictureMap >>= Just . makePicture player pAngle pOffset pPosition
      boosterPicture = player ^. pBoosterTexture >>= (`M.lookup` pictureMap) >>= Just . makeBoosterPicture player
      Point x y = absoluteBackward (fromIntegral (player ^. pRadius)) player ^. pPosition
      makeBoosterPicture player = translate x y . rotate ((- oneEighty (player ^. pAngle)) + 90) . scale 0.05 0.05

instance Renderable Ship where
  render pictureMap ship = M.lookup (ship ^. sTexture) pictureMap >>= Just . makePicture ship sAngle sOffset sPosition

instance Renderable Bullet where
  render pictureMap bullet = M.lookup (bullet ^. bTexture) pictureMap >>= Just . makePicture bullet bAngle bOffset bPosition

instance Renderable Background where
  render pictureMap bg =
    let (Point x y) = bg ^. bgPosition
     in M.lookup (bg ^. bgTexture) pictureMap >>= Just . translate x y

makePicture :: a -> Lens' a Float -> Lens' a Float -> Lens' a Position -> Picture -> Picture
makePicture a viewAngle viewOffset viewPos = translate x y . rotate (angle + offset)
  where
    angle = a ^. viewAngle * (-1)
    offset = a ^. viewOffset
    (Point x y) = a ^. viewPos

renderWorld :: World -> Picture
renderWorld World {_gameState = Playing, _pictureMap = pMap, ..} = pictures ps
  where
    score = "Score: " ++ show _totalScore
    health = "Health: " ++ show (_player ^. pHealth)
    angle = "Player Angle: " ++ show (_player ^. pAngle)
    bulletPs = mapMaybe (render pMap) _bullets
    shipsPs = mapMaybe (render pMap) _ships
    otherPs =
      catMaybes
        [ render pMap _bg,
          render pMap _player,
          Just $ renderString (-950, 450) score,
          Just $ renderString (-950, 400) health,
          Just $ renderString (-950, 350) angle
        ]
    ps = otherPs ++ bulletPs ++ shipsPs
renderWorld World {_gameState = Defeat} =
  pictures
    [ renderString' (-400, 0) 0.5 red "You have Lost!!",
      renderString' (-400, -60) 0.5 red "Try better next time!!"
    ]
renderWorld World {_gameState = Victory} =
  pictures
    [ renderString' (-400, 0) 0.5 green "You have won!!",
      renderString' (-400, -60) 0.5 green "Congratulations!!"
    ]

renderString :: (Float, Float) -> String -> Picture
renderString (x, y) = translate x y . scale 0.15 0.15 . color white . Text

renderString' :: (Float, Float) -> Float -> Color -> String -> Picture
renderString' (x, y) scale' colour = translate x y . scale scale' scale' . color colour . Text