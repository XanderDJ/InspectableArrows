module Level.FireHydrant
    (FireHydrant(..),
     initFireHydrantA,
     initFireHydrant,
     updateFireHydrant,
     drawFireHydrant) where

import qualified Data.Map as M
import Graphics.Rendering.OpenGL hiding (rect)
import Nxt.Graphics
import Nxt.Types
import Resource.Resource
import ResourceManagement.InspectableArrow
import Settings.WorldSettings as WorldSettings
import Settings.Path

data FireHydrant = FireHydrant
    {
        fireHydrantDisabled     :: Bool,
        fireHydrantDir          :: Direction,
        fireHydrantRect         :: Nxt.Types.Rect,
        fireHydrantTexture      :: [Nxt.Types.Texture],
        fireHydrantDisTexture   :: Nxt.Types.Texture
    }

initFireHydrantA :: RaincatArrow a => a (M.Map ResourceConfig [Nxt.Types.Texture], Vector2d, Direction) FireHydrant
initFireHydrantA = useResource (CycleTexture "/data/level-misc/fire-hydrant-left" 8 WorldSettings.fireHydrantFrameTime) initFireHydrant'

initFireHydrant' :: ResourceConfig -> (M.Map ResourceConfig [Nxt.Types.Texture], Vector2d, Direction) -> FireHydrant
initFireHydrant' rc (rcMap, (posX, posY), dir) = FireHydrant False dir rect textures (head textures)
 where
     textures = rcMap M.! rc
     rect = Nxt.Types.Rect posX posY ((fromIntegral . textureWidth . head) textures) ((fromIntegral . textureHeight . head) textures)

-- initFireHydrant
initFireHydrant :: Vector2d -> Direction -> IO FireHydrant
initFireHydrant (posX, posY) dir = do
    dataPath <- getDataDir
    textures <- cycleTextures (dataPath ++ "/data/level-misc/fire-hydrant-left") 8 WorldSettings.fireHydrantFrameTime

    let rect = Nxt.Types.Rect posX posY (fromIntegral $ textureWidth $ head textures) (fromIntegral $ textureHeight $ head textures)

    return (FireHydrant False dir rect textures (head textures))

-- updateFireHydrant
updateFireHydrant :: FireHydrant -> FireHydrant
updateFireHydrant fireHydrant =
    fireHydrant {fireHydrantTexture = tail (fireHydrantTexture fireHydrant)}

-- drawFireHydrant
drawFireHydrant :: FireHydrant -> IO ()
drawFireHydrant (FireHydrant disa dir rect texList texDis) =
    Nxt.Graphics.drawTextureFlip posX posY tex (1.0::GLdouble) fliped
    where (posX, posY) = (rectX rect, rectY rect)
          tex = if disa then texDis else head texList
          fliped = case dir of
                      DirLeft -> False
                      DirRight -> True

