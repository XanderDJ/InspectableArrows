module SpaceShooter.AddObjects where

import qualified Data.Text as T
import Lens.Micro
import ResourceManagement.ResourceIdentifier
import SpaceShooter.Angle
import SpaceShooter.Circle
import SpaceShooter.Movement (TranslationalMovement (absoluteForward), getTotalVelocity, totalVelocity)
import SpaceShooter.Objects
import SpaceShooter.Point
import SpaceShooter.Utility
import System.Random

data BulletResources = BR
  { brFp :: T.Text,
    brOffset :: Float,
    brPower :: Int
  }
  deriving (Show, Eq, Ord)

instance HasResource BulletResources where
  getResourceFilePath = brFp
  getResourceType _ = PNG

data ShipResources = SR
  { srHealth :: Int,
    srRadius :: Int,
    srValue :: Int,
    srVelocity :: Float,
    srAngle :: Angle,
    srOffset :: Angle,
    srTexture :: T.Text,
    srShootStyle :: ShootStyle,
    srPower :: Int,
    srShootDecision :: ShootDecision
  }
  deriving (Show, Eq, Ord)

instance HasResource ShipResources where
  getResourceFilePath SR {srTexture = t} = t
  getResourceType _ = PNG

playerShoots :: BulletResources -> World -> World
playerShoots br world = world & bullets %~ (++) newBullets
  where
    p = world ^. player
    newBullets = makeBullets (p ^. pShootStyle) p br

playerMoves :: T.Text -> World -> World
playerMoves moveTexture world = world & player . pBoosterTexture ?~ moveTexture

addShipAtRandomPos :: ShipResources -> World -> World
addShipAtRandomPos (SR health radius value velocity angle offset texture shootstyle power shootdecision) world = world & ships .~ s : (world ^. ships) & stdGen .~ g'''
  where
    g = world ^. stdGen
    Border xL xH yL yH = Border (-500) 500 (-200) 200
    (xPos, g') = randomR (xL, xH) g
    (yPos, g'') = randomR (yL, yH) g'
    (movement, g''') = uniform g''
    s = Ship health radius value (Point xPos yPos) velocity 0 offset texture shootstyle power movement shootdecision

shipShoots :: BulletResources -> (World, Ship) -> World
shipShoots br (w, s) =
  w
    & bullets %~ (++) bulletsToAdd
    & ships %~ replace s s'
  where
    shootStyle = s ^. sShootStyle
    s' = s & sShootDecision %~ canShoot .~ False
    bulletsToAdd = makeBullets shootStyle s br

makeBullets :: (Circle o, Shootable o, TranslationalMovement o) => ShootStyle -> o -> BulletResources -> [Bullet]
makeBullets ss o (BR fp offset power) = case ss of
  Single ->
    let angle = getAngle o
        pos = getCenter $ absoluteForward (getRadius o + 17) o
     in [Bullet 8 power pos velocity angle offset fp]
  Spread n ->
    let angleDelta = div 360 n
        newAngle o = turnLeft (getAngle o)
        angles = [newAngle o (fromIntegral (angleDelta * x)) | x <- [0 .. n - 1]]
        positions = map (getCenter . absoluteForward (getRadius o + 17) . setAngle o) angles
        posAndAngles = zip positions angles
        f r p v o fp (pos, a) = Bullet r p pos v a o fp
     in map (f 8 power velocity offset fp) posAndAngles
  Burst n ->
    let angle = getAngle o
        f r p v a o fp pos = Bullet r p pos v a o fp
        getPositions 0 o = [getCenter $ absoluteForward (getRadius o) o]
        getPositions n o = getCenter (absoluteForward (getRadius o + ((getRadius o + 17) * n)) o) : getPositions (n -1) o
        positions = getPositions (fromIntegral n) o
     in map (f 8 power velocity angle offset fp) positions
  where
    velocity = max 150 (getVelocity o + 300)
