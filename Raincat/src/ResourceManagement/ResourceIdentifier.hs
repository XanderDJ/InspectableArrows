module ResourceManagement.ResourceIdentifier where

import qualified Data.Text as T


class HasResource a where
    getResourceFilePath :: a -> T.Text
    getResourceType :: a -> ResourceType

class HasResources a where
    getResourceIdentifiers :: a -> [ResourceIdentifier]


data ResourceType = PNG | JPG | MP3 | MP4 | WAV | JSON | Other deriving (Show, Eq, Ord)

data ResourceIdentifier = RI {
    resourceFilepath :: T.Text,
    resouceType ::  ResourceType
 } deriving (Show, Eq, Ord)

instance HasResource ResourceIdentifier where
    getResourceFilePath (RI filepath _) = filepath
    getResourceType (RI _ resourceType) = resourceType

instance HasResource T.Text where
  getResourceFilePath fp = fp
  getResourceType _ = Other
    