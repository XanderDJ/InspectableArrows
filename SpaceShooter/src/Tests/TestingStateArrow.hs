{-# LANGUAGE FlexibleContexts #-}
module Tests.TestingStateArrow where


import Control.Monad.State
import qualified Data.Text as T
import ResourceManagement.ResourceIdentifier
import ResourceManagement.InspectableConstructors
import ResourceManagement.InspectableArrow
import Control.Monad.Identity (Identity (runIdentity))



data World = World { action :: Int, textures :: [T.Text]} deriving Show


data Action = Action Int T.Text

instance HasResource Action where
  getResourceFilePath (Action _ fp) = fp
  getResourceType _ = PNG


abstractFunction :: TexturedArrow a => a World World
abstractFunction = undefined

updateAction :: (MonadState World m) => Action -> m ()
updateAction (Action action fp) = do
    currentState <- get
    let newState = World action (fp : textures currentState)
    put newState

run :: World
run = execState (updateAction (Action 2 "test.png")) (World 1 [])