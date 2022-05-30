{-# LANGUAGE TemplateHaskell #-}

module SpaceShooter.Point where

import Lens.Micro.TH
import System.Random
import System.Random.Stateful

data Point a = Point {_xCoord :: a, _yCoord :: a} deriving (Show, Eq, Ord)

makeLenses ''Point

instance Uniform a => Uniform (Point a) where
  uniformM g = Point <$> uniformM g <*> uniformM g

instance UniformRange a => UniformRange (Point a) where
  uniformRM (Point xl yl, Point xh yh) g = Point <$> uniformRM (xl, xh) g <*> uniformRM (yl, yh) g

-- | Not optimal since unnecessary sqrt, could remove sqrt but then we need to keep track everywhere that this is squared.
distanceBetween :: (Num a, Floating a) => Point a -> Point a -> a
distanceBetween (Point x y) (Point x' y') = sqrt $ (x - x') ** 2 + (y - y') ** 2

add :: Num a => Point a -> Point a -> Point a
add (Point x y) (Point x' y') = Point (x+x') (y+y')

sub :: Num a => Point a -> Point a -> Point a
sub (Point x y) (Point x' y') = Point (x-x') (y-y')

pow :: Floating a => Int -> Point a -> Point a
pow power (Point x y) = Point (x ** fromIntegral power) (y ** fromIntegral power)

fromIntegralPoint :: (Integral a, Num b) => Point a -> Point b
fromIntegralPoint (Point x y) = Point (fromIntegral x) (fromIntegral y)