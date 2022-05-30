module Panels.ItemPanel
    (GoStopState(..),
     GoStopButton(..),
     initGoStopButtonA,
     initGoStopButton,
     updateGoStopButton,
     setGoStopButton,
     toggleGoStopButton,
     drawGoStopButton,
     ItemPanel(ItemPanel),
     itemButtonList,
     goStopButton) where

import qualified Data.Map as M
import Graphics.Rendering.OpenGL as GL
import Nxt.Types
import Nxt.Graphics
import Resource.Resource
import ResourceManagement.InspectableArrow
import Items.Items
import Settings.Path

data GoStopState = GoState | StopState

data GoStopButton = GoStopButton
    {
        goStopState         :: GoStopState,
        goStopButtonRect    :: Nxt.Types.Rect,
        goStopCooldown      :: Int,
        goButtonTexture     :: Nxt.Types.Texture,
        stopButtonTexture   :: Nxt.Types.Texture
    }

data GoStopResourceConfigs = GSRC {
    goResourceConfig :: ResourceConfig,
    stopResourceConfig :: ResourceConfig
}

instance HasResourceConfigs GoStopResourceConfigs where
    getResourceConfigs gsrc = [goResourceConfig gsrc, stopResourceConfig gsrc]


initGoStopButtonA :: RaincatArrow a => a (M.Map ResourceConfig [Nxt.Types.Texture]) GoStopButton
initGoStopButtonA = useResources (GSRC (LoadTexture "/data/item-buttons/cat-go-button.png") (LoadTexture "/data/item-buttons/cat-stop-button.png")) initGoStopButton'

initGoStopButton' :: GoStopResourceConfigs -> M.Map ResourceConfig [Nxt.Types.Texture] -> GoStopButton
initGoStopButton' (GSRC goRc stopRc) rcMap = GoStopButton GoState gsRect 0 goTexture stopTexture
 where goTexture = head $ rcMap M.! goRc
       stopTexture = head $ rcMap M.! stopRc
       gsRect = Nxt.Types.Rect 870.0 0.0 128.0 90.0

-- initGoStopButton
initGoStopButton :: IO GoStopButton
initGoStopButton = do
    dataPath <- getDataDir
    goTex    <- Nxt.Graphics.loadTexture (dataPath ++ "/data/item-buttons/cat-go-button.png")
    stopTex  <- Nxt.Graphics.loadTexture (dataPath ++ "/data/item-buttons/cat-stop-button.png")

    let gsrect = Nxt.Types.Rect 870.0 0.0 128.0 90.0

    return (GoStopButton GoState gsrect 0 goTex stopTex)

-- updateGoStopButton
updateGoStopButton :: GoStopButton -> GoStopButton
updateGoStopButton btn@(GoStopButton _ _ cooldown _ _) =
    if cooldown > 0
       then btn {goStopCooldown = cooldown - 1}
       else btn

-- setGoStopButton
setGoStopButton :: GoStopState -> GoStopButton -> GoStopButton
setGoStopButton state btn =
    btn {goStopState = state}

-- toggleGoStopButton - toggles only if cooldown is done
toggleGoStopButton :: GoStopButton -> GoStopButton
toggleGoStopButton btn@(GoStopButton _ _ 0 _ _) =
    case goStopState btn of
         GoState    -> btn {goStopState = StopState, goStopCooldown = 30}
         StopState  -> btn {goStopState = GoState, goStopCooldown = 30}
toggleGoStopButton btn = btn

-- drawGoStopButton
drawGoStopButton :: GoStopButton -> IO ()
drawGoStopButton (GoStopButton state gsrect _ goTex stopTex) =
    case state of
         GoState    -> Nxt.Graphics.drawTexture (rectX gsrect) (rectY gsrect) goTex (1.0::GLdouble)
         StopState  -> Nxt.Graphics.drawTexture (rectX gsrect) (rectY gsrect) stopTex (1.0::GLdouble)

data ItemPanel = ItemPanel
    {
        itemButtonList  :: [ItemButton],
        goStopButton    :: GoStopButton
    }

