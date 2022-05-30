module Tests.TestingInspectableArrows where

import Control.Arrow
import qualified Data.Text as T
import ResourceManagement.InspectableArrow
import ResourceManagement.ResourceIdentifier
import qualified Data.Set as S

data ObjectWithTexture = OWT
  { objectHealth :: Int,
    objectTexture :: T.Text
  } deriving Show

newtype GameWorld = GW
  { objects :: [ObjectWithTexture]
  } deriving Show

data MyOwnResourceContainer = MORC Int T.Text deriving (Show, Eq, Ord)

instance HasResource MyOwnResourceContainer where
  getResourceFilePath (MORC _ fp) = fp
  getResourceType _ = PNG

initialWorld :: GameWorld
initialWorld = GW []

inspectableFunction :: (TexturedArrow a) => Int -> T.Text -> a GameWorld GameWorld
inspectableFunction hp t = useTexture (MORC 10 t) addObject

addObject :: MyOwnResourceContainer -> GameWorld -> GameWorld
addObject (MORC hp texturePath) (GW objects) = GW (OWT hp texturePath : objects)

addObjectWithTexture :: TexturedArrow a => a GameWorld GameWorld
addObjectWithTexture = inspectableFunction 10 "assets/object_texture"

complexInteraction :: TexturedArrow a => a GameWorld GameWorld
complexInteraction = addObjectWithTexture >>> inspectableFunction 20 "assets/big_object_texture.png" >>> arr add10Health

resourcesUsed :: S.Set ResourceIdentifier 
resourcesUsed = getConstA addObjectWithTexture

resourcesUsedComplex :: S.Set ResourceIdentifier
resourcesUsedComplex = getConstA complexInteraction

newWorld :: GameWorld
newWorld = addObjectWithTexture initialWorld

newWorldAfterInteraction :: GameWorld
newWorldAfterInteraction = complexInteraction initialWorld

add10Health :: GameWorld -> GameWorld
add10Health (GW os) = GW (map f os)
 where f (OWT oHp oTs) = OWT (oHp + 10) oTs