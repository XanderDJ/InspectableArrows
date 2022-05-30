module Resource.Resource where

type FrameTime = Int
type TotalFrames = Int
type LastFrame = Int
type StartRepeat = Int
type EndRepeat = Int
type NumberOfRepeats = Int

data ResourceConfig = 
    LoadTexture String 
    | CycleTexture String TotalFrames FrameTime
    | CycleLastFrame String TotalFrames LastFrame FrameTime
    | RepeatTextures String TotalFrames StartRepeat EndRepeat NumberOfRepeats LastFrame FrameTime
     deriving (Show, Eq, Ord)

class HasResourceConfig a where
  getResourceConfig :: a -> ResourceConfig

instance HasResourceConfig ResourceConfig where
    getResourceConfig = id

class HasResourceConfigs a where
  getResourceConfigs :: a -> [ResourceConfig]

instance HasResourceConfigs ResourceConfig where
    getResourceConfigs rc = [rc]