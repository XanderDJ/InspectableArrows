{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE FlexibleContexts #-}

module SpaceShooter.Data where

import Lens.Micro.TH ( makeLenses )
import Data.Map as Map ( Map )
import Graphics.Gloss ( Picture )
import Game ( Animation, Texture )
import Control.Monad.Identity ( Identity )
import System.Random ( StdGen )

type Position = (Float, Float)

data Background = Bg {
  _bgPosition :: Position,
  _backgroundTexture :: Texture
} deriving Show

makeLenses ''Background

data Border = Border
  { _borderTexture :: Texture,
    _bPos :: Position
  }
  deriving (Show)

makeLenses ''Border

data Decision = Forward | Turn Heading | Shoot | Pass deriving (Show, Eq)

data ShootStyle = Single | Spread | Multi deriving (Show, Eq)

type Power = Int

type Health = Int 

data Heading = North | NorthEast | East | SouthEast | South | SouthWest | West | NorthWest deriving (Show, Eq)

data Ship = Ship
  { _sHealth :: Int,
    _sPos :: Position,
    _sHeading :: Heading,
    _sOffset :: Float,
    _sTexture :: Texture,
    _sShootStyle :: ShootStyle,
    _sPower :: Power,
    _sDecision :: Decision
  }
  deriving (Show)

makeLenses ''Ship

data Player = Player
  { _pHealth :: Int,
    _pHeading :: Heading,
    _pOffset :: Float,
    _pTexture :: Texture
  }
  deriving (Show)

makeLenses ''Player

data Bullet = Bullet
  { _bPower :: Int,
    _bCoords :: Position,
    _bHeading :: Heading,
    _bOffset :: Float,
    _bTexture :: Texture
  }
  deriving (Show)

makeLenses ''Bullet

data Entity = EntityShip Ship | EntityBullet Bullet | EntityBorder Border deriving (Show)

data GameState = GameState
  { _tMap :: Map.Map Texture Picture,
    _entities :: [Entity],
    _lengthBorder :: Position, -- (minRow, maxRow)
    _widhtBorder :: Position, -- (minCol, maxCol)
    _gameBg :: [Background],
    _player :: Player,
    _pBullets :: [Bullet],
    _pCoords :: Position,
    _stateUpdated :: Bool,
    _canMove :: Bool,
    _score :: Int
  }
  deriving (Show)

makeLenses ''GameState

data Game = Game {
  _currentAnimation :: Either (Animation GameState Identity ()) (),
  _randGen :: StdGen,
  _state :: GameState
}

instance Show Game where
  show (Game _ g s) = show s ++ "\n" ++ show g

makeLenses ''Game