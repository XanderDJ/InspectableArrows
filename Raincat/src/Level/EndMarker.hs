module Level.EndMarker
    (EndMarker(EndMarker),
     endMarkerRect,
     endMarkerTexture,
     initEndMarker,
     initEndMarkerA) where

import Nxt.Types
import Nxt.Graphics
import Settings.Path
import Resource.Resource
import ResourceManagement.InspectableArrow
import qualified Data.Map as M

data EndMarker = EndMarker
    {
        endMarkerRect       :: Nxt.Types.Rect,
        endMarkerTexture    :: Nxt.Types.Texture
    }

initEndMarkerA :: RaincatArrow a => a (M.Map ResourceConfig [Nxt.Types.Texture], Vector2d) EndMarker
initEndMarkerA = useResource (LoadTexture "/data/level-misc/level-end-marker.png") initEndMarker'

initEndMarker' :: ResourceConfig -> (M.Map ResourceConfig [Nxt.Types.Texture], Vector2d) -> EndMarker
initEndMarker' rc (rcMap, (posX, posY)) = EndMarker markerRect markerTex
 where markerTex = head (rcMap M.! rc)
       markerRect = Nxt.Types.Rect posX posY
                                   (fromIntegral $ textureWidth markerTex :: Double)
                                   (fromIntegral $ textureHeight markerTex :: Double)

initEndMarker :: Vector2d -> IO EndMarker
initEndMarker (posX, posY) = do
    dataPath  <- getDataDir
    markerTex <- loadTexture (dataPath ++ "/data/level-misc/level-end-marker.png")

    let markerRect = Nxt.Types.Rect posX posY
                                   (fromIntegral $ textureWidth markerTex :: Double)
                                   (fromIntegral $ textureHeight markerTex :: Double)

    return (EndMarker markerRect markerTex)

