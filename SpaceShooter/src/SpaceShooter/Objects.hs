{-# LANGUAGE TemplateHaskell #-}

module SpaceShooter.Objects where

import qualified Data.Map as M
import qualified Data.Text as T
import Graphics.Gloss (Picture)
import Lens.Micro
import Lens.Micro.TH (makeLenses)
import SpaceShooter.Angle (Angle)
import SpaceShooter.Circle
import SpaceShooter.Point
import System.Random
import System.Random.Stateful

type Position = Point Float

data Velocity = Vel {_vx :: Float, _vy :: Float} deriving (Show, Eq, Ord)

makeLenses ''Velocity

data ShootStyle = Single | Spread Int | Burst Int deriving (Show, Eq, Ord)

data Decision = Forward | TurnLeft Angle | TurnRight Angle | Shoot | Pass deriving (Show, Eq, Ord)
data Turning = NO_TURNING | RIGHT | LEFT deriving (Eq, Show, Ord, Enum)

data Movement = StraightLine Float Float | RightSquare Float Float | LeftSquare Float Float | Circle Float Turning deriving (Show, Eq, Ord)

instance Uniform Movement where
  uniformM g = do
    i <- uniformRM (0, 3) g
    case i :: Int of
      0 -> do
        time <- uniformRM (0.5, 2) g
        return $ StraightLine time time
      1 -> do
        time <- uniformRM (0.5, 2) g
        return $ RightSquare time time
      2 -> do
        time <- uniformRM (0.5, 2) g
        return $ LeftSquare time time
      3 -> Circle <$> uniformRM (90, 160) g <*> uniformM g
      _ -> error "Impossible situation"

data ShootDecision = SD
  { _timeLeft :: Float,
    _reloadTime :: Float,
    _canShoot :: Bool
  }
  deriving (Show, Eq, Ord)

makeLenses ''ShootDecision


instance Uniform Turning where
  uniformM g = do
    i <- uniformRM (0, 2) g
    return $ [NO_TURNING, RIGHT, LEFT] !! i

type Power = Int

type Health = Int

data Background = Bg {_bgPosition :: Position, _bgTexture :: T.Text} deriving (Show, Eq, Ord)

makeLenses ''Background

data Player = Player
  { _pHealth :: Int,
    _pRadius :: Int,
    _pPosition :: Position,
    _pVelocity :: Float,
    _pAngle :: Angle,
    _pOffset :: Angle,
    _pTexture :: T.Text,
    _pBoosterTexture :: Maybe T.Text,
    _pAccelerating :: Bool,
    _pTurning :: Turning,
    _pShootStyle :: ShootStyle,
    _pPower :: Int
  }
  deriving (Show, Eq, Ord)

makeLenses ''Player

data Ship = Ship
  { _sHealth :: Int,
    _sRadius :: Int,
    _sValue :: Int,
    _sPosition :: Position,
    _sVelocity :: Float,
    _sAngle :: Angle,
    _sOffset :: Angle,
    _sTexture :: T.Text,
    _sShootStyle :: ShootStyle,
    _sPower :: Power,
    _sMovement :: Movement,
    _sShootDecision :: ShootDecision
  }
  deriving (Show, Eq, Ord)

makeLenses ''Ship

data Bullet = Bullet
  { _bRadius :: Int,
    _bPower :: Int,
    _bPosition :: Position,
    _bVelocity :: Float,
    _bAngle :: Angle,
    _bOffset :: Angle,
    _bTexture :: T.Text
  }
  deriving (Show, Eq, Ord)

makeLenses ''Bullet

data Border = Border
  { _xLow :: Float,
    _xHigh :: Float,
    _yLow :: Float,
    _yHigh :: Float
  }
  deriving (Show, Eq, Ord)

makeLenses ''Border

data GameState = Playing | Defeat | Victory deriving (Show, Eq, Ord)

data World = World
  { _stdGen :: StdGen,
    _gameState :: GameState,
    _totalScore :: Int,
    _borders :: Border,
    _canSpawnShip :: Bool,
    _canSpawnShipTime :: Float,
    _canSpawnShipTimeLeft :: Float,
    _player :: Player,
    _bullets :: [Bullet],
    _ships :: [Ship],
    _pictureMap :: M.Map T.Text Picture,
    _bg :: Background
  }

makeLenses ''World

instance Circle Player where
  getCenter p = p ^. pPosition
  getRadius p = fromIntegral $ p ^. pRadius

instance Circle Ship where
  getCenter s = s ^. sPosition
  getRadius s = fromIntegral $ s ^. sRadius

instance Circle Bullet where
  getCenter b = b ^. bPosition
  getRadius = fromIntegral . _bRadius


class Shootable a where
  getAngle :: a -> Angle
  getVelocity :: a -> Float
  setAngle :: a -> Float -> a

instance Shootable Player where
  getAngle = (^. pAngle) 
  getVelocity = (^. pVelocity)
  setAngle p a = p & pAngle .~ a

instance Shootable Ship where
  getAngle = (^. sAngle)
  getVelocity = (^. sVelocity)
  setAngle s a = s & sAngle .~ a
  
  