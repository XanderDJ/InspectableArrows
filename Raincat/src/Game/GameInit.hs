module Game.GameInit
    (gameInit, allInitResources) where

import Data.IORef
import World.World hiding (itemPanel, mainPanel, messagePanel)
import qualified Nxt.Graphics
import Nxt.Types
import Input.InputState as InputState
import Items.Items
import Panels.MainPanel hiding (cat)
import Panels.ItemPanel
import Panels.MessagePanel
import Nxt.Audio
import Cat.Cat
import Level.Level
import Settings.CatSettings as CatSettings
import Items.ItemEffects
import Level.EndMarker
import Level.FireHydrant
import Game.GameState
import Settings.Path
import Resource.Resource
import ResourceManagement.InspectableArrow
import qualified Data.Map as M
import qualified Data.Set as S

allInitResources :: S.Set ResourceConfig
allInitResources = getConstA (initCatA undefined) <> getConstA initItemPanelA <> getConstA initEndMarkerA <> getConstA initFireHydrantA <> getConstA initGoStopButtonA

-- gameInit
gameInit :: IO WorldState
gameInit = do
    rcMap <- Nxt.Graphics.loadResources (S.toList allInitResources)
    dataPath <- getDataDir
    keys     <- newIORef (InputState.KeysState False False False False False False False False False)
    mousePos <- newIORef (InputState.MousePos 0 0)

    initAudio
    m <- loadMusic (dataPath ++ "/data/music/project_raincat.ogg")
    playMusic m

    lvl <- openLevel (dataPath ++ "/data/levels/pinball/pinball.lvl")
    let lvlData = levelData lvl
        lvlRect = levelRects lvlData
        lvlPoly = levelPolys lvlData
        lvlPuddles = levelPuddles lvlData
        lvlEndRect = levelEnd lvlData
        lvlFireHydrantsL = levelFireHydrantsL lvlData
        lvlFireHydrantsR = levelFireHydrantsR lvlData
        catStartRect = levelCat lvlData
        catStart = (rectX catStartRect, rectY catStartRect)
        cat = initCatA catStart rcMap
        lvlFireHydrants = map (\fhRect -> initFireHydrantA (rcMap, (rectX fhRect, rectY fhRect), DirLeft)) lvlFireHydrantsL
         ++ map (\fhRect -> initFireHydrantA (rcMap, (rectX fhRect, rectY fhRect), DirRight)) lvlFireHydrantsR
        levelend = initEndMarkerA (rcMap, (rectX lvlEndRect, rectY lvlEndRect))

    -- main panel
    let initCameraPos = (0, 0)
    backgroundTexObj <- Nxt.Graphics.loadTexture (dataPath ++ "/data/backgrounds/clouds_background.png")

    -- item panel
    let itemPanel = initItemPanelA rcMap
        item = itemButItem $ last $ itemButtonList itemPanel

    -- main panel
    let mainPanel = MainPanel initCameraPos [] backgroundTexObj cat lvlRect lvlPoly lvlPuddles lvlFireHydrants levelend [] [] [] item Nothing m

    -- message panel
    let messagePanel = MessagePanel "I am the message panel! Hover the cursor over the items on the panel at right to see their description!"

    -- menu textures
    menuTex <- Nxt.Graphics.loadTexture (dataPath ++ "/data/menu/level-select.png")
    howtoTex <- Nxt.Graphics.loadTexture (dataPath ++ "/data/menu/how-to.png")

    return (WorldState MainMenuState keys mousePos lvl mainPanel itemPanel messagePanel (menuTex, howtoTex) rcMap)


data ItemPanelResourceConfigs = IPRC {
    umbrellaRC :: ResourceConfig,
    upsideDownUmbrellaRC :: ResourceConfig,
    ponchoRC :: ResourceConfig,
    hairdryerRC :: ResourceConfig,
    springbootsRC :: ResourceConfig,
    skateboardRC :: ResourceConfig,
    wrenchRC :: ResourceConfig,
    corkRC :: ResourceConfig,
    pogostickRC :: ResourceConfig,
    speedbootsRC :: ResourceConfig,
    tarpRC :: ResourceConfig,
    shieldRC :: ResourceConfig,
    rainbootsRC :: ResourceConfig,
    eraserRC :: ResourceConfig,
    corkItemRC :: ResourceConfig,
    tarpItemRC :: ResourceConfig
}

instance HasResourceConfigs ItemPanelResourceConfigs where
    getResourceConfigs(IPRC umbrella upsidedown poncho hairdryer springboots skateboard wrench cork pogostick speedboots tarp shield rainboots eraser corkItem tarpItem) 
     = [umbrella, upsidedown, poncho, hairdryer, springboots, skateboard, wrench, cork, pogostick, speedboots, tarp, shield, rainboots, eraser, corkItem, tarpItem]


initItemPanelA :: RaincatArrow a => a (M.Map ResourceConfig [Nxt.Types.Texture]) ItemPanel
initItemPanelA = useResources itemPanelResourceConfigs initItemPanel'

initItemPanel' :: ItemPanelResourceConfigs -> M.Map ResourceConfig [Nxt.Types.Texture] -> ItemPanel
initItemPanel' (IPRC umbrella ud poncho hairdryer springboots skate wrench cork pogostick speedboots tarp shield rainboots eraser corkItemRC tarpItemRC) rcMap 
 = ItemPanel itemButtons goStopButton
  where
    umbrellaTex = head $ rcMap M.! umbrella
    upsidedownUmbrellaTex = head $ rcMap M.! ud
    ponchoTex = head $ rcMap M.! poncho
    hairdryerTex = head $ rcMap M.! hairdryer
    springbootsTex = head $ rcMap M.! springboots
    skateboardTex = head $ rcMap M.! skate
    wrenchTex = head $ rcMap M.! wrench
    corkTex = head $ rcMap M.! cork
    pogostickTex = head $ rcMap M.! pogostick
    speedbootsTex = head $ rcMap M.! speedboots
    tarpTex = head $ rcMap M.! tarp
    shieldTex = head $ rcMap M.! shield
    rainbootsTex = head $ rcMap M.! rainboots
    eraserTex = head $ rcMap M.! eraser
    corkItemTex = head $ rcMap M.! corkItemRC
    tarpItemTex = head $ rcMap M.! tarpItemRC
    umbrellaItem            = Item (0, 0) umbrellaTex umbrellaEffect "Umbrella"
    upsidedownUmbrellaItem  = Item (0, 0) upsidedownUmbrellaTex upsUmbrellaEffect "Inverted Umbrella"
    ponchoItem              = Item (0, 0) ponchoTex ponchoEffect "Poncho"
    hairdryerItem           = Item (0, 0) hairdryerTex hairDryerEffect "Hairdryer"
    springbootsItem         = Item (0, 0) springbootsTex springBootsEffect "Spring Boots"
    skateboardItem          = Item (0, 0) skateboardTex skateboardEffect "Skateboard"
    wrenchItem              = Item (0, 0) wrenchTex wrenchEffect "Wrench"
    corkItem                = Item (0, 0) corkItemTex noEffect "Cork"
    pogostickItem           = Item (0, 0) pogostickTex pogostickEffect "Pogostick"
    speedbootsItem          = Item (0, 0) speedbootsTex speedBootsEffect "Speed Boots"
    tarpItem                = Item (0, 0) tarpItemTex noEffect "Tarp"
    shieldItem              = Item (0, 0) shieldTex shieldEffect "Shield"
    rainbootsItem           = Item (0, 0) rainbootsTex rainBootsEffect "Rain Boots"
    eraserItem              = Item (0, 0) eraserTex noEffect "Eraser"
    umbrellaDesc            = "Umbrella - Normal umbrella on the ground (lasts " ++ show (CatSettings.catUmbrellaDuration `div` 60) ++ " seconds), Mary Poppins umbrella in the air. (one use)"
    upsidedownUmbrellaDesc  = "Upsidedown Umbrella - A rather crude, makeshift boat. This can actually float perfectly fine on puddles. (one use)"
    ponchoDesc              = "Poncho - This will protect your fuzzy head and fuzzy back, but not your fuzzy feet or fuzzy face. (lasts " ++ show (CatSettings.catPonchoDuration `div` 60) ++  " seconds)"
    hairdryerDesc           = "Hairdryer - You'd think this would have something to do with drying, but it actually turns the cat around."
    springbootsDesc         = "Springboots - Insta-*BOING* (one use)"
    skateboardDesc          = "Skateboard - Ride swiftly over hazardous terrain. (lasts " ++ show (CatSettings.catSkateDuration `div` 60) ++ " seconds)"
    wrenchDesc              = "Wrench - Turn off a fire hydrant, much to the disappointment of the neighborhood children. (one use)"
    corkDesc                = "Cork - It's a floating platform. Go figure."
    pogostickDesc           = "Pogostick - Spring back up after falling a distance. Only works when grabbed in the air! (one use)"
    speedbootsDesc          = "Speedboots - Move disturbingly quickly. (lasts " ++ show (CatSettings.catSpeedDuration `div` 60) ++ " second)"
    tarpDesc                = "Tarp - This impressive material blocks rain. It is not strong enough to walk on though."
    rainbootsDesc           = "Rainboots - Walk through puddles with brazen impunity (lasts " ++ show (CatSettings.catRainDuration `div` 60) ++ " seconds)"
    shieldDesc              = "Shield - Complete protection from water. (lasts " ++ show (CatSettings.catShieldDuration `div` 60) ++ " seconds)"
    eraserDesc              = ""
    itemButtons = [ItemButton (860, 700.0) umbrellaTex umbrellaDesc umbrellaItem 0,
                       ItemButton (950, 700.0) upsidedownUmbrellaTex upsidedownUmbrellaDesc upsidedownUmbrellaItem 0,
                       ItemButton (860, 600.0) ponchoTex ponchoDesc ponchoItem 0,
                       ItemButton (950, 600.0) hairdryerTex hairdryerDesc hairdryerItem 0,
                       ItemButton (860, 500.0) springbootsTex springbootsDesc springbootsItem 0,
                       ItemButton (950, 500.0) skateboardTex skateboardDesc skateboardItem 0,
                       ItemButton (860, 400.0) wrenchTex wrenchDesc wrenchItem 0,
                       ItemButton (950, 400.0) corkTex corkDesc corkItem 0,
                       ItemButton (860, 300.0) pogostickTex pogostickDesc pogostickItem 0,
                       ItemButton (950, 300.0) speedbootsTex speedbootsDesc speedbootsItem 0,
                       ItemButton (860, 200.0) tarpTex tarpDesc tarpItem 0,
                       ItemButton (950, 200.0) rainbootsTex rainbootsDesc rainbootsItem 0,
                       ItemButton (860, 100.0) shieldTex shieldDesc shieldItem 0,
                       ItemButton (950, 100.0) eraserTex eraserDesc eraserItem (-1)]
    goStopButton = initGoStopButtonA rcMap



itemPanelResourceConfigs :: ItemPanelResourceConfigs
itemPanelResourceConfigs = IPRC
 (LoadTexture "/data/item-buttons/item-umbrella.png")
 (LoadTexture "/data/item-buttons/item-upsidedown-umbrella.png")
 (LoadTexture "/data/item-buttons/item-poncho.png")
 (LoadTexture "/data/item-buttons/item-hairdryer.png")
 (LoadTexture "/data/item-buttons/item-springboots.png")
 (LoadTexture "/data/item-buttons/item-skateboard.png")
 (LoadTexture "/data/item-buttons/item-wrench.png")
 (LoadTexture "/data/item-buttons/item-cork.png")
 (LoadTexture "/data/item-buttons/item-pogostick.png")
 (LoadTexture "/data/item-buttons/item-speedboots.png")
 (LoadTexture "/data/item-buttons/item-tarp.png")
 (LoadTexture "/data/item-buttons/item-happy-shield.png")
 (LoadTexture "/data/item-buttons/item-rainboots.png")
 (LoadTexture "/data/item-buttons/item-eraser.png")
 (LoadTexture "/data/items/cork.png")
 (LoadTexture "/data/items/tarp.png")

-- initItemPanel
initItemPanel :: IO ItemPanel
initItemPanel = do
    dataPath                <- getDataDir
    umbrellaTex             <- Nxt.Graphics.loadTexture (dataPath ++ "/data/item-buttons/item-umbrella.png")
    upsidedownUmbrellaTex   <- Nxt.Graphics.loadTexture (dataPath ++ "/data/item-buttons/item-upsidedown-umbrella.png")
    ponchoTex               <- Nxt.Graphics.loadTexture (dataPath ++ "/data/item-buttons/item-poncho.png")
    hairdryerTex            <- Nxt.Graphics.loadTexture (dataPath ++ "/data/item-buttons/item-hairdryer.png")
    springbootsTex          <- Nxt.Graphics.loadTexture (dataPath ++ "/data/item-buttons/item-springboots.png")
    skateboardTex           <- Nxt.Graphics.loadTexture (dataPath ++ "/data/item-buttons/item-skateboard.png")
    wrenchTex               <- Nxt.Graphics.loadTexture (dataPath ++ "/data/item-buttons/item-wrench.png")
    corkTex                 <- Nxt.Graphics.loadTexture (dataPath ++ "/data/item-buttons/item-cork.png")
    pogostickTex            <- Nxt.Graphics.loadTexture (dataPath ++ "/data/item-buttons/item-pogostick.png")
    speedbootsTex           <- Nxt.Graphics.loadTexture (dataPath ++ "/data/item-buttons/item-speedboots.png")
    tarpTex                 <- Nxt.Graphics.loadTexture (dataPath ++ "/data/item-buttons/item-tarp.png")
    shieldTex               <- Nxt.Graphics.loadTexture (dataPath ++ "/data/item-buttons/item-happy-shield.png")
    rainbootsTex            <- Nxt.Graphics.loadTexture (dataPath ++ "/data/item-buttons/item-rainboots.png")
    eraserTex               <- Nxt.Graphics.loadTexture (dataPath ++ "/data/item-buttons/item-eraser.png")

    corkItemTex <- Nxt.Graphics.loadTexture (dataPath ++ "/data/items/cork.png")
    tarpItemTex <- Nxt.Graphics.loadTexture (dataPath ++ "/data/items/tarp.png")

    let umbrellaItem            = Item (0, 0) umbrellaTex umbrellaEffect "Umbrella"
        upsidedownUmbrellaItem  = Item (0, 0) upsidedownUmbrellaTex upsUmbrellaEffect "Inverted Umbrella"
        ponchoItem              = Item (0, 0) ponchoTex ponchoEffect "Poncho"
        hairdryerItem           = Item (0, 0) hairdryerTex hairDryerEffect "Hairdryer"
        springbootsItem         = Item (0, 0) springbootsTex springBootsEffect "Spring Boots"
        skateboardItem          = Item (0, 0) skateboardTex skateboardEffect "Skateboard"
        wrenchItem              = Item (0, 0) wrenchTex wrenchEffect "Wrench"
        corkItem                = Item (0, 0) corkItemTex noEffect "Cork"
        pogostickItem           = Item (0, 0) pogostickTex pogostickEffect "Pogostick"
        speedbootsItem          = Item (0, 0) speedbootsTex speedBootsEffect "Speed Boots"
        tarpItem                = Item (0, 0) tarpItemTex noEffect "Tarp"
        shieldItem              = Item (0, 0) shieldTex shieldEffect "Shield"
        rainbootsItem           = Item (0, 0) rainbootsTex rainBootsEffect "Rain Boots"
        eraserItem              = Item (0, 0) eraserTex noEffect "Eraser"

    let umbrellaDesc            = "Umbrella - Normal umbrella on the ground (lasts " ++ show (CatSettings.catUmbrellaDuration `div` 60) ++ " seconds), Mary Poppins umbrella in the air. (one use)"
        upsidedownUmbrellaDesc  = "Upsidedown Umbrella - A rather crude, makeshift boat. This can actually float perfectly fine on puddles. (one use)"
        ponchoDesc              = "Poncho - This will protect your fuzzy head and fuzzy back, but not your fuzzy feet or fuzzy face. (lasts " ++ show (CatSettings.catPonchoDuration `div` 60) ++  " seconds)"
        hairdryerDesc           = "Hairdryer - You'd think this would have something to do with drying, but it actually turns the cat around."
        springbootsDesc         = "Springboots - Insta-*BOING* (one use)"
        skateboardDesc          = "Skateboard - Ride swiftly over hazardous terrain. (lasts " ++ show (CatSettings.catSkateDuration `div` 60) ++ " seconds)"
        wrenchDesc              = "Wrench - Turn off a fire hydrant, much to the disappointment of the neighborhood children. (one use)"
        corkDesc                = "Cork - It's a floating platform. Go figure."
        pogostickDesc           = "Pogostick - Spring back up after falling a distance. Only works when grabbed in the air! (one use)"
        speedbootsDesc          = "Speedboots - Move disturbingly quickly. (lasts " ++ show (CatSettings.catSpeedDuration `div` 60) ++ " second)"
        tarpDesc                = "Tarp - This impressive material blocks rain. It is not strong enough to walk on though."
        rainbootsDesc           = "Rainboots - Walk through puddles with brazen impunity (lasts " ++ show (CatSettings.catRainDuration `div` 60) ++ " seconds)"
        shieldDesc              = "Shield - Complete protection from water. (lasts " ++ show (CatSettings.catShieldDuration `div` 60) ++ " seconds)"
        eraserDesc              = ""

    let itemButtons = [ItemButton (860, 700.0) umbrellaTex umbrellaDesc umbrellaItem 0,
                       ItemButton (950, 700.0) upsidedownUmbrellaTex upsidedownUmbrellaDesc upsidedownUmbrellaItem 0,
                       ItemButton (860, 600.0) ponchoTex ponchoDesc ponchoItem 0,
                       ItemButton (950, 600.0) hairdryerTex hairdryerDesc hairdryerItem 0,
                       ItemButton (860, 500.0) springbootsTex springbootsDesc springbootsItem 0,
                       ItemButton (950, 500.0) skateboardTex skateboardDesc skateboardItem 0,
                       ItemButton (860, 400.0) wrenchTex wrenchDesc wrenchItem 0,
                       ItemButton (950, 400.0) corkTex corkDesc corkItem 0,
                       ItemButton (860, 300.0) pogostickTex pogostickDesc pogostickItem 0,
                       ItemButton (950, 300.0) speedbootsTex speedbootsDesc speedbootsItem 0,
                       ItemButton (860, 200.0) tarpTex tarpDesc tarpItem 0,
                       ItemButton (950, 200.0) rainbootsTex rainbootsDesc rainbootsItem 0,
                       ItemButton (860, 100.0) shieldTex shieldDesc shieldItem 0,
                       ItemButton (950, 100.0) eraserTex eraserDesc eraserItem (-1)]

    goStopBtn <- initGoStopButton

    return (ItemPanel itemButtons goStopBtn)

