{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}

module Main where

import Control.Arrow
import Control.Monad.Identity (Identity (runIdentity))
import Data.Functor.Const
import qualified Data.Map.Strict as Map
import Data.Maybe (catMaybes, fromJust)
import qualified Data.Set as S
import qualified Data.Text as T
import Game
import Graphics.Gloss hiding (Point)
import Lens.Micro ((&), (.~), (^.))
import ResourceLoader (loadAsset, loadAssets)
import qualified ResourceManagement.InspectableArrow as RIA
import qualified ResourceManagement.InspectableConstructors as RIC
import ResourceManagement.ResourceIdentifier
import SpaceShooter
import SpaceShooter.Gloss
import SpaceShooter.Objects (pictureMap)
import System.Random

main :: IO ()
main = do
  let resources =
        S.elems $
          RIA.getConstA handleInput
            `S.union` getConst (initialWorld undefined)
            `S.union` RIA.getConstA SpaceShooter.Gloss.updateWorld ::
          [ResourceIdentifier]
      filepaths = map resourceFilepath resources
  pictures <- mapM (loadAsset . T.unpack) filepaths
  randomGen <- getStdGen
  let iWorld = RIC.getObject (initialWorld randomGen) & pictureMap .~ Map.fromList (zip filepaths pictures)
  play window background 60 iWorld SpaceShooter.Gloss.renderWorld (curry handleInput) (curry SpaceShooter.Gloss.updateWorld)

mainSelfInitialized :: IO ()
mainSelfInitialized = runKleisli gameLoop allTextures

allTextures :: [Texture]
allTextures = S.elems $ getConstA gameLoop

gameLoop ::
  ( GlossInterface a1 a2 f,
    InitialiseList f,
    TexturedArrows a1,
    IfA a1,
    Case a1,
    AnimationInterface f1 a1,
    LinearTo GameState f1,
    Set GameState f1,
    SetTexture GameState f1,
    Animations f1
  ) =>
  a2 [Texture] ()
gameLoop = playI window background 144 initialGame Main.renderWorld handleKeysCase Main.updateWorld

window :: Display
window = InWindow "Space rogue like" (1900, 1000) (10, 10)

background :: Color
background = black

initialPlayer :: InitialiseObject f => f Player
initialPlayer = pure (Player 200 North 90) `initialiseTexture` Texture "Assets/Ships/player.png"

initialGame :: (InitialiseObject f, InitialiseList f) => StdGen -> f Game
initialGame stdGen = Game (Right ()) stdGen <$> initialState

initialState :: (InitialiseObject f, InitialiseList f) => f GameState
initialState = GameState Map.empty [] (-4, 4) (-9, 9) <$> iBackground <*> Main.initialPlayer <*> pure [] <*> pure (0, 0) <*> pure False <*> pure True <*> pure 0

iBackground :: InitialiseList f => f [Background]
iBackground =
  initialList
    `addTexturedObject` (Texture "Assets/Background/fullspace.jpg", Bg (0, 0))

renderWorld :: Game -> Picture
renderWorld game = picture
  where
    gameState = game ^. state
    player' = gameState ^. player
    ptexture = gameState ^. (player . pTexture)
    orientedPicture = fromJust $ render player' (Map.lookup ptexture (gameState ^. tMap))
    bullets =
      let f = findBulletPicture gameState
          bulletList = gameState ^. pBullets
       in pictures $ catMaybes $ zipWith render bulletList $ map f bulletList
    (x, y) = tileToCoords (gameState ^. pCoords)
    playerPicture = translate x y orientedPicture
    bgPicture =
      let f = findBackgroundPicture gameState
          bgs = gameState ^. gameBg
       in pictures $ catMaybes $ zipWith render bgs $ map f bgs
    enemiesPicture = renderEnemiesPicture game
    picture = pictures [bgPicture, bullets, enemiesPicture, playerPicture, (renderString (-950, 450) . ("Score: " ++) . show) (game ^. state . score)]

renderEnemiesPicture :: Game -> Picture
renderEnemiesPicture game = pictures . catMaybes $ zipWith render es ps
  where
    es = game ^. state . entities
    em = game ^. state . tMap
    ts = map getEntityTexture es
    ps = map (`Map.lookup` em) ts

-- Explosions, kogel raakt schip

updateWorld :: (Case a, TexturedArrows a, IfA a) => a (Float, Game) Game
updateWorld = ifA canUpdate execAnimation validateGame

-- Make update world and execAnimation with arrows

canUpdate :: Arrow a => a (Float, Game) (Either (Animation GameState Identity (), Float, Game) Game)
canUpdate = arr canUpdate'

canUpdate' :: (Float, Game) -> Either (Animation GameState Identity (), Float, Game) Game
canUpdate' (t, game) =
  case game ^. currentAnimation of
    Right _ -> Right game
    Left anim -> Left (anim, t, game)

execAnimation :: Arrow a => a (Animation GameState Identity (), Float, Game) Game
execAnimation = arr (uncurry3 execAnimation')

execAnimation' :: Animation GameState Identity () -> Float -> Game -> Game
execAnimation' anim t game =
  let (appState', animResult, _) = runIdentity $ runAnimation anim (game ^. state) t
   in game & state .~ appState' & currentAnimation .~ animResult

validateGame :: (IfA a, Case a, TexturedArrows a) => a Game Game
validateGame = ifA noEntities (arr validateGame') (arr validateGame' >>> add5RandomShip)

validateGame' :: Game -> Game
validateGame' game = game'
  where
    state' = filterOutOfBoundsEntities (game ^. state)
    game' = game & state .~ state'

filterOutOfBoundsEntities :: GameState -> GameState
filterOutOfBoundsEntities gs = gs'
  where
    bullets = gs ^. pBullets
    bullets' = filter (\bullet -> not $ outOfBounds (bullet ^. bCoords) gs) bullets
    gs' = gs & pBullets .~ bullets'

noEntities :: Arrow a => a Game (Either Game Game)
noEntities = arr noEntities'

noEntities' :: Game -> Either Game Game
noEntities' game = if null (game ^. state . entities) then Right game else Left game

findBulletPicture :: GameState -> Bullet -> Maybe Picture
findBulletPicture game bullet =
  Map.lookup key textureMap
  where
    key = bullet ^. bTexture
    textureMap = game ^. tMap

findBackgroundPicture :: GameState -> Background -> Maybe Picture
findBackgroundPicture GameState {_tMap = textureMap} (Bg pos t) = Map.lookup t textureMap

renderString :: (Float, Float) -> String -> Picture
renderString (x, y) = translate x y . scale 0.15 0.15 . color white . Text

addRandomShip :: (TexturedArrows a, Case a) => a Game Game
addRandomShip = randInt (1, 4) >>> addShip

addNRandomShip :: (TexturedArrows a, Case a) => Int -> a Game Game
addNRandomShip 0 = arr id
addNRandomShip x = addRandomShip >>> addNRandomShip (x - 1)

add5RandomShip :: (Case a, TexturedArrows a) => a Game Game
add5RandomShip = addNRandomShip 5

makeShip :: Texture -> (Position, Health, ShootStyle, Power) -> Ship
makeShip t (pos, health, ss, p) = Ship health pos West 90 t ss p Pass

randInt :: Arrow a => (Int, Int) -> a Game (Game, Int)
randInt range =
  arr
    ( \game ->
        let stdGen = game ^. randGen
            (i, gen') = randomR range stdGen
         in (game & randGen .~ gen', i)
    )

addShip :: (TexturedArrows a, Case a) => a (Game, Int) Game
addShip =
  match
    matchFighter
    ( arr posForShip
        >>> second (textureA (Texture "Assets/Ships/fighter.png") makeShip)
        >>> arr addShipToEntities
    )
    <--> ( matchCruiser,
           arr posForShip
             >>> second (textureA (Texture "Assets/Ships/cruiser.png") makeShip)
             >>> arr addShipToEntities
         )
    <--> ( matchDestroyer,
           arr posForShip
             >>> second (textureA (Texture "Assets/Ships/destroyer.png") makeShip)
             >>> arr addShipToEntities
         )
    <--> ( matchBoss,
           arr posForShip
             >>> second (textureA (Texture "Assets/Ships/boss.png") makeShip)
             >>> arr addShipToEntities
         )
    <-|-> arr fst

addShipToEntities :: (Game, Ship) -> Game
addShipToEntities (game, ship) =
  let es = game ^. state . entities
   in game & state . entities .~ EntityShip ship : es

posForShip :: (Game, (Health, Power, ShootStyle)) -> (Game, (Position, Health, ShootStyle, Power))
posForShip (game, (health, pow, style)) =
  let (game', pos) = getRandomPosition game
   in (game', (pos, health, style, pow))

getRandomPosition :: Game -> (Game, Position)
getRandomPosition game = (game', pos)
  where
    allPos = allPositions game
    occupiedPos = occupiedPositions game
    freePos = S.filter (`notElem` occupiedPos) allPos
    game' = game & randGen .~ newGen
    (pos, newGen) = randElem freePos (game ^. randGen)

allPositions :: Game -> S.Set Position
allPositions game = S.fromList [(x, y) | x <- [minCol .. maxCol], y <- [minRow .. maxRow]]
  where
    (minRow, maxRow) = game ^. state . lengthBorder
    (minCol, maxCol) = game ^. state . widhtBorder

occupiedPositions :: Game -> S.Set Position
occupiedPositions game = S.fromList (playerPos : bulletPositions ++ entityPositions)
  where
    playerPos = game ^. state . pCoords
    bulletPositions = map _bCoords (game ^. state . pBullets)
    entityPositions = map getEntityPos (game ^. state . entities)

randElem :: (RandomGen g) => S.Set a -> g -> (a, g)
randElem s g = (S.elemAt n s, g')
  where
    (n, g') = randomR (0, S.size s - 1) g