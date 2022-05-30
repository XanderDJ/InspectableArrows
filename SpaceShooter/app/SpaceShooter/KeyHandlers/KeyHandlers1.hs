{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE FunctionalDependencies #-}
{-# LANGUAGE TypeFamilies #-}

module SpaceShooter.KeyHandlers.KeyHandlers1 where

import Control.Arrow hiding ((<+>))
import Control.Monad.Identity
import Data.Functor.Const
import qualified Data.Set as S
import Game
import Graphics.Gloss.Interface.Pure.Game
import Lens.Micro
import PaSe (Animation, ConstA (ConstA), Texture (Texture))
import SpaceShooter.Animations
import SpaceShooter.Data
import SpaceShooter.Function
import SpaceShooter.KeyHandlers.Classes
import SpaceShooter.Types

instance KeyHandlers (->) where
  addBulletA t = addBullet t
  turnLeftA = turnLeft
  turnRightA = turnRight
  moveForwardA = movePlayer

instance KeyHandlers (ConstA (S.Set Texture)) where
  addBulletA t = ConstA (S.singleton t)
  turnLeftA = ConstA S.empty
  turnRightA = ConstA S.empty
  moveForwardA = ConstA S.empty

instance Indexable Event where
  index (EventKey (SpecialKey KeyLeft) Down _ _) = 0
  index (EventKey (Char 'q') Down _ _) = 1
  index (EventKey (SpecialKey KeyRight) Down _ _) = 2
  index (EventKey (Char 'd') Down _ _) = 3
  index (EventKey (SpecialKey KeyUp) Down _ _) = 4
  index (EventKey (Char 'z') Down _ _) = 5
  index (EventKey (SpecialKey KeySpace) Down _ _) = 6
  index _ = 7

-- animation animatie = getConst animatie

{-
handleKeys :: Event -> Game -> Game
handleKeys (EventKey (SpecialKey KeyLeft) Down _ _) = whenAvailable turnLeftA
handleKeys (EventKey (Char 'q') Down _ _) = whenAvailable turnLeftA
handleKeys (EventKey (SpecialKey KeyRight) Down _ _) = whenAvailable turnRightA
handleKeys (EventKey (Char 'd') Down _ _) = whenAvailable turnRightA
handleKeys (EventKey (SpecialKey KeyUp) Down _ _) = whenAvailable moveForwardA
handleKeys (EventKey (Char 'z') Down _ _) = whenAvailable moveForwardA
handleKeys (EventKey (SpecialKey KeySpace) Down _ _) = whenAvailable addRedBullet
handleKeys _ = id
-}

patternList :: (IfA a, KeyHandlers a) => PatternList (Key, KeyState) a Game Game
patternList =
  PL
    [ ((SpecialKey KeyLeft, Down), whenAvailable turnLeftA),
      ((Char 'q', Down), whenAvailable turnLeftA),
      ((SpecialKey KeyRight, Down), whenAvailable turnRightA),
      ((Char 'd', Down), whenAvailable turnRightA),
      ((SpecialKey KeyUp, Down), whenAvailable moveForwardA),
      ((Char 'z', Down), whenAvailable moveForwardA),
      ((SpecialKey KeySpace, Down), whenAvailable (addBulletA (Texture "Assets/Bullets/red_bullet.png")))
    ]
    (arr id)

branches :: (Arrow a, IfA a, KeyHandlers a) => List8 (a Game Game)
branches =
  L8
    (whenAvailable turnLeftA)
    (whenAvailable turnLeftA)
    (whenAvailable turnRightA)
    (whenAvailable turnRightA)
    (whenAvailable moveForwardA)
    (whenAvailable moveForwardA)
    (whenAvailable (addBulletA (Texture "Assets/Bullets/red_bullet.png")))
    (arr id)

handleKeys8 :: (Case8 a, IfA a, KeyHandlers a) => Event -> a Game Game
handleKeys8 event = case8 event branches

handleKeys :: (CaseList a, IfA a, KeyHandlers a) => Event -> a Game Game
handleKeys event = caseList (branchEvent event) patternList

branchEvent :: Event -> (Key, KeyState)
branchEvent (EventKey key keyState _ _) = (key, keyState)
branchEvent _ = (SpecialKey KeyAltR, Down)

whenAvailable :: IfA a => a Game Game -> a Game Game
whenAvailable = ifA availableA (arr id)

availableA :: Arrow a => a Game (Either Game Game)
availableA = arr (\game -> if isAvailable game then Right game else Left game)

-- | If player can shoot (i.e. bullet doesn't isntantly go out of bounds then return Right game else return Left game)
canShootA :: Arrow a => a Game (Either Game Game)
canShootA = arr canShoot

canShoot :: Game -> Either Game Game
canShoot game = bool
  where
    gs = game ^. state
    pos = gs ^. pCoords
    heading = gs ^. player . pHeading
    bool = if outOfBounds (nextPos pos heading) gs then Left game else Right game

{-

handleKeys event = caseOf event case1 case2 case3 case4
case1 (add a bullet to the game state) = if bool then addBulletA (Texture 'texture') else addBulletA (Texture "yellow")

-}

isAvailable :: Game -> Bool
isAvailable (Game (Right _) _  _) = True
isAvailable (Game (Left _) _ _) = False

turn :: (Heading -> Heading) -> Game -> Game
turn f game = game'
  where
    currentHeading = game ^. state . player . pHeading
    nextHeading = f currentHeading
    game' = game & state . player . pHeading .~ nextHeading

turnLeft :: Game -> Game
turnLeft = turn counterClockWise

turnRight :: Game -> Game
turnRight = turn clockWise

movePlayer :: Game -> Game
movePlayer game = if bool then game & currentAnimation .~ Left (moveBulletsAnim gs bullets >>- moveForwardAnim coords heading pCoords) else game
  where
    gs = game ^. state
    bullets = gs ^. pBullets
    coords = gs ^. pCoords
    heading = gs ^. player . pHeading
    nextCoords = nextPos coords heading
    bool = not $ outOfBounds nextCoords gs

-- pure
addBullet :: Texture -> Game -> Game
addBullet t game = game'
  where
    state' = game ^. state -- Arrow f => f Game GameState
    coords = state' ^. pCoords
    heading = state' ^. player . pHeading
    newBullet = Bullet 10 coords heading 0 t -- create beschrijving
    oldBullets = state' ^. pBullets
    newBullets = newBullet : oldBullets
    game' = game & state . pBullets .~ newBullets

matchLeftKey :: Arrow a => a (Event, Game) (Either (Event, Game) Game)
matchLeftKey = arr matchLeftKey'

matchLeftKey' :: (Event, Game) -> Either (Event, Game) Game
matchLeftKey' (EventKey (SpecialKey KeyLeft) Down _ _, game) = Right game
matchLeftKey' (EventKey (Char 'q') Down _ _, game) = Right game
matchLeftKey' other = Left other

matchRightKey :: Arrow a => a (Event, Game) (Either (Event, Game) Game)
matchRightKey = arr matchRightKey'

matchRightKey' :: (Event, Game) -> Either (Event, Game) Game
matchRightKey' (EventKey (SpecialKey KeyRight) Down _ _, game) = Right game
matchRightKey' (EventKey (Char 'd') Down _ _, game) = Right game
matchRightKey' other = Left other

matchForwardKey :: Arrow a => a (Event, Game) (Either (Event, Game) Game)
matchForwardKey = arr matchForwardKey'

matchForwardKey' :: (Event, Game) -> Either (Event, Game) Game
matchForwardKey' (EventKey (SpecialKey KeyUp) Down _ _, game) = Right game
matchForwardKey' (EventKey (Char 'z') Down _ _, game) = Right game
matchForwardKey' other = Left other

matchSpace :: Arrow a => a (Event, Game) (Either (Event, Game) Game)
matchSpace = arr matchSpace'

matchSpace' :: (Event, Game) -> Either (Event, Game) Game
matchSpace' (EventKey (SpecialKey KeySpace) Down _ _, game) = Right game
matchSpace' other = Left other

matchCKey :: Arrow a => a (Event, Game) (Either (Event, Game) Game)
matchCKey = arr matchCKey'

matchCKey' :: (Event, Game) -> Either (Event, Game) Game
matchCKey' (EventKey (Char 'c') Down _ _, game) = Right game
matchCKey' other = Left other

defaultAction :: Arrow a => a (Event, Game) Game
defaultAction = arr snd

handleKeysIf :: (Arrow a, IfA a, KeyHandlers a) => a (Event, Game) Game
handleKeysIf =
  ifA (matchLeftKey >>> notA) (whenAvailable turnLeftA) $
    ifA (matchRightKey >>> notA) (whenAvailable turnRightA) $
      ifA (matchForwardKey >>> notA) (whenAvailable moveForwardA) $
        ifA (matchSpace >>> notA) (whenAvailable (addBulletA (Texture "Assets/Bullets/red_bullet.png"))) defaultAction

-- Problem can't define interface
class Arrow a => AnimationInterface f a | a -> f where
  updateAnimation :: (b -> f ()) -> (f () -> Either (f ()) ()) -> a (Game, b) Game

instance Ord a => AnimationInterface (Const [a]) (ConstA (S.Set a)) where
  updateAnimation f _ = ConstA $ S.fromList (getConst (f undefined))

instance AnimationInterface (Animation GameState Identity) (->) where
  updateAnimation getAnimation f (state, input) = state & currentAnimation .~ f (getAnimation input)

handleKeysCase ::
  ( Arrow a,
    TexturedArrows a,
    IfA a,
    Case a,
    AnimationInterface f a,
    MapAnim f,
    Parallel f,
    LinearTo GameState f,
    IfThenElse f,
    Applicative f,
    Set GameState f,
    SetTexture GameState f,
    Delay f
  ) =>
  a (Event, Game) Game
handleKeysCase =
  match matchLeftKey (whenAvailable (arr turnLeft))
    <--> (matchRightKey, whenAvailable (arr turnRight))
    <--> (matchForwardKey, whenAvailable (arr movePlayer))
    <--> ( matchSpace,
           whenAvailable $
             ifThenElseA
               canShootA
               (arr id)
               ( textureA (Texture "Assets/Bullets/red_bullet.png") addBullet
                   >>> arr (\game -> (game, (game ^. state, game ^. state . pBullets)))
                   >>> updateAnimation (uncurry moveBulletsAnim) Left
               )
         )
--   <--> (matchCKey, whenAvailable (arr clearEntities))
    <-|-> defaultAction


clearEntities :: Game -> Game
clearEntities = state . entities .~ []

updateScore :: Arrow a => Int -> a Game Game
updateScore x = arr (\game -> game & state . score .~ (game ^. state . score) + x)