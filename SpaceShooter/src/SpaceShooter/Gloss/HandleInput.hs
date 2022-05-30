module SpaceShooter.Gloss.HandleInput (handleInput) where

import Control.Arrow
import Graphics.Gloss.Interface.Pure.Game
import Lens.Micro
import ResourceManagement.InspectableArrow
import SpaceShooter.Movement
import SpaceShooter.Objects
import SpaceShooter.AddObjects
import SpaceShooter.Point

matchLeftKeyDown :: Arrow a => a (Event, World) (Either (Event, World) World)
matchLeftKeyDown = arr matchLeftKeyDown'

matchLeftKeyDown' :: (Event, World) -> Either (Event, World) World
matchLeftKeyDown' (EventKey (SpecialKey KeyLeft) Down _ _, game) = Right game
matchLeftKeyDown' (EventKey (Char 'q') Down _ _, game) = Right game
matchLeftKeyDown' other = Left other

matchLeftKeyUp :: Arrow a => a (Event, World) (Either (Event, World) World)
matchLeftKeyUp = arr matchLeftKeyUp'

matchLeftKeyUp' :: (Event, World) -> Either (Event, World) World
matchLeftKeyUp' (EventKey (SpecialKey KeyLeft) Up _ _, game) = Right game
matchLeftKeyUp' (EventKey (Char 'q') Up _ _, game) = Right game
matchLeftKeyUp' other = Left other

matchRightKeyDown :: Arrow a => a (Event, World) (Either (Event, World) World)
matchRightKeyDown = arr matchRightKeyDown'

matchRightKeyDown' :: (Event, World) -> Either (Event, World) World
matchRightKeyDown' (EventKey (SpecialKey KeyRight) Down _ _, game) = Right game
matchRightKeyDown' (EventKey (Char 'd') Down _ _, game) = Right game
matchRightKeyDown' other = Left other

matchRightKeyUp :: Arrow a => a (Event, World) (Either (Event, World) World)
matchRightKeyUp = arr matchRightKeyUp'

matchRightKeyUp' :: (Event, World) -> Either (Event, World) World
matchRightKeyUp' (EventKey (SpecialKey KeyRight) Up _ _, game) = Right game
matchRightKeyUp' (EventKey (Char 'd') Up _ _, game) = Right game
matchRightKeyUp' other = Left other

matchForwardKeyDown :: Arrow a => a (Event, World) (Either (Event, World) World)
matchForwardKeyDown = arr matchForwardKeyDown'

matchForwardKeyDown' :: (Event, World) -> Either (Event, World) World
matchForwardKeyDown' (EventKey (SpecialKey KeyUp) Down _ _, game) = Right game
matchForwardKeyDown' (EventKey (Char 'z') Down _ _, game) = Right game
matchForwardKeyDown' other = Left other

matchRKey :: Arrow a => a (Event, World) (Either (Event, World) World)
matchRKey = arr matchRKey'

matchRKey' :: (Event, World) -> Either (Event, World) World
matchRKey' (EventKey (Char 'r') Down _ _, world) = Right world
matchRKey' other = Left other

matchVKey :: Arrow a => a (Event, World) (Either (Event, World) World)
matchVKey = arr matchVKey'

matchVKey' :: (Event, World) -> Either (Event, World) World
matchVKey' (EventKey (Char 'v') Down _ _, world) = Right world
matchVKey' other = Left other

matchForwardKeyUp :: Arrow a => a (Event, World) (Either (Event, World) World)
matchForwardKeyUp = arr matchForwardKeyUp'

matchForwardKeyUp' :: (Event, World) -> Either (Event, World) World
matchForwardKeyUp' (EventKey (SpecialKey KeyUp) Up _ _, game) = Right game
matchForwardKeyUp' (EventKey (Char 'z') Up _ _, game) = Right game
matchForwardKeyUp' other = Left other

matchCKey :: TexturedArrow a => a (Event, World) (Either (Event, World) World)
matchCKey = arr matchCKey'

matchCKey' :: (Event, b) -> Either (Event, b) b
matchCKey' (EventKey (Char 'c') Down _ _, world) = Right world
matchCKey' other = Left other

matchSpace :: Arrow a => a (Event, World) (Either (Event, World) World)
matchSpace = arr matchSpace'

matchSpace' :: (Event, World) -> Either (Event, World) World
matchSpace' (EventKey (SpecialKey KeySpace) Down _ _, game) = Right game
matchSpace' other = Left other

handleInput :: TexturedArrow a => a (Event, World) World
handleInput =
  match matchForwardKeyDown (useTexture "Assets/Ships/blueflame.png" playerMoves  >>> arr (player . pAccelerating .~ True))
    <-> (matchForwardKeyUp, arr (player . pAccelerating .~ False >>> player . pBoosterTexture .~ Nothing))
    <-> (matchLeftKeyUp, arr (player . pTurning .~ NO_TURNING))
    <-> (matchLeftKeyDown, arr (player . pTurning .~ LEFT))
    <-> (matchRightKeyDown, arr (player . pTurning .~ RIGHT))
    <-> (matchRightKeyUp, arr (player . pTurning .~ NO_TURNING))
    <-> (matchSpace, useTexture (BR "Assets/Bullets/red_bullet.png" 0 10) playerShoots)
    <-> (matchRKey, arr resetWorld)
    <-> (matchCKey, arr (ships .~ []))
    <-|-> arr snd

resetWorld :: World -> World
resetWorld = (player . pPosition .~ Point 0 0) . (ships .~ []) . (bullets .~ []) . (totalScore .~ 0) . (player . pHealth .~ 100) . (player . pVelocity .~ 0) 