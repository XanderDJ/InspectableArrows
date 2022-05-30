module SpaceShooter.Function where

import SpaceShooter.Data
import Lens.Micro
import PaSe

getAngle :: Heading -> Float
getAngle North = -90
getAngle NorthEast = -45
getAngle East = 0
getAngle SouthEast = 45
getAngle South = 90
getAngle SouthWest = 135
getAngle West = 180
getAngle NorthWest = -135

tileToCoords :: (Float, Float) -> (Float, Float)
tileToCoords = both %~ (* 100)

nextPos :: (Float, Float) -> Heading -> (Float, Float)
nextPos (x, y) North = (x, y + 1)
nextPos (x, y) NorthEast = (x + 1, y + 1)
nextPos (x, y) East = (x + 1, y)
nextPos (x, y) SouthEast = (x + 1, y -1)
nextPos (x, y) South = (x, y - 1)
nextPos (x, y) SouthWest = (x - 1, y - 1)
nextPos (x, y) West = (x - 1, y)
nextPos (x, y) NorthWest = (x - 1, y + 1)

nextBouncePos :: (Float, Float) -> Heading -> Surface -> (Float, Float)
nextBouncePos (x, y) NorthEast VerticalWall = (x, y + 1)
nextBouncePos (x, y) NorthEast HorizontalWall = (x + 1, y)
nextBouncePos (x, y) SouthEast VerticalWall = (x, y - 1)
nextBouncePos (x, y) SouthEast HorizontalWall = (x + 1, y)
nextBouncePos (x, y) SouthWest VerticalWall = (x , y -1)
nextBouncePos (x, y) SouthWest HorizontalWall = (x -1, y)
nextBouncePos (x, y) NorthWest VerticalWall = (x, y + 1)
nextBouncePos (x, y) NorthWest HorizontalWall = (x - 1, y)
nextBouncePos coords _ Corner = coords
nextBouncePos coords _ _ = coords

isShip :: Entity -> Bool
isShip (EntityShip _) = True
isShip _ = False

getShip :: Entity -> Maybe Ship
getShip (EntityShip s) = Just s
getShip _ = Nothing

outOfBounds :: (Float, Float) -> GameState -> Bool
outOfBounds (x, y) game = xCheck || yCheck
  where
    (minRow, maxRow) = game ^. lengthBorder
    (minCol, maxCol) = game ^. widhtBorder
    xCheck = x < minCol || x > maxCol
    yCheck = y < minRow || y > maxRow

clockWise :: Heading -> Heading
clockWise North = NorthEast
clockWise NorthEast = East
clockWise East = SouthEast
clockWise SouthEast = South
clockWise South = SouthWest
clockWise SouthWest = West
clockWise West = NorthWest
clockWise NorthWest = North

counterClockWise :: Heading -> Heading
counterClockWise NorthEast = North
counterClockWise North = NorthWest
counterClockWise NorthWest = West
counterClockWise West = SouthWest
counterClockWise SouthWest = South
counterClockWise South = SouthEast
counterClockWise SouthEast = East
counterClockWise East = NorthEast

data Surface = VerticalWall | HorizontalWall | Corner deriving (Show, Eq)

getSurfaceType :: GameState -> (Float, Float) -> Heading -> Surface
getSurfaceType gs coords heading = wallType
  where
    (x', y') = coords & both %~ abs
    maxRow = gs ^. lengthBorder . _2
    maxCol = gs ^. widhtBorder . _2
    wallType
      | (maxCol - x') < (maxRow - y') = VerticalWall
      | (maxCol - x') > (maxRow - y') = HorizontalWall
      | otherwise = Corner

bounce :: Heading -> Surface -> Heading
bounce North _ = South
bounce NorthEast s
  | s == HorizontalWall = SouthEast
  | s == VerticalWall = NorthWest
  | otherwise = SouthWest
bounce East _ = West
bounce SouthEast s
  | s == HorizontalWall = NorthEast
  | s == VerticalWall = SouthWest
  | otherwise = NorthWest
bounce South _ = North
bounce SouthWest s
  | s == HorizontalWall = NorthWest
  | s == VerticalWall = SouthEast
  | otherwise = NorthEast
bounce West _ = East
bounce NorthWest s
  | s == HorizontalWall = SouthWest
  | s == VerticalWall = NorthEast
  | otherwise = SouthEast

getCorners :: GameState -> [(Float, Float)]
getCorners gs = corners
  where
    (minRow, maxRow) = gs ^. lengthBorder
    (minCol, maxCol) = gs ^. widhtBorder
    corners = map (both %~ extend 1) [(minRow, minCol), (minRow, maxCol), (maxRow, minCol), (maxRow, maxCol)]


getEntityPos :: Entity -> Position 
getEntityPos (EntityShip s) = _sPos s
getEntityPos (EntityBullet b) = _bCoords b
getEntityPos (EntityBorder border) = _bPos border

getEntityTexture :: Entity -> Texture
getEntityTexture (EntityShip s) = s ^. sTexture
getEntityTexture (EntityBullet b) = b ^. bTexture
getEntityTexture (EntityBorder b) = b ^. borderTexture


extend :: (Ord a, Num a) => a -> a -> a
extend increment number = if number < 0 then number - increment else number + increment


curry3 :: ((a , b, c) -> d) -> a -> b -> c -> d
curry3 f a b c = f (a, b , c)

uncurry3 :: (a -> b -> c -> d) -> (a, b, c) -> d
uncurry3 f (a, b, c) = f a b c