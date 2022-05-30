{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE FlexibleInstances #-}
module SpaceShooter.KeyHandlers.Classes where


import Control.Arrow
import Game
import SpaceShooter.Data
import Lens.Micro
import Data.Either
import Data.Functor.Const
import qualified Data.Set as S

class Arrow a => KeyHandlers a where
  addBulletA :: Texture -> a Game Game
  turnLeftA :: a Game Game
  turnRightA :: a Game Game
  moveForwardA :: a Game Game
  -- animatie :: Animation -> a Game Game 

