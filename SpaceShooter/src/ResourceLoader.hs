{-# LANGUAGE OverloadedStrings #-}

module ResourceLoader
  ( loadAsset,
    loadAssets,
  )
where

import Codec.Picture (convertRGBA8, readImage)
import qualified Data.Text as T
import Graphics.Gloss.Data.Picture (Picture)
import Graphics.Gloss.Juicy (fromImageRGBA8)
import PaSe (Texture (..))
import System.Directory (listDirectory)

loadAsset :: FilePath -> IO Picture
loadAsset path = do
  (Right img) <- readImage path
  let rgba = convertRGBA8 img
  return $ fromImageRGBA8 rgba

loadAssets :: FilePath -> IO [(Texture, Picture)]
loadAssets base = do
  fps <- listDirectory base
  let pictureFiles = filter isPictureFile fps
      completePictureFiles = map ((base ++ "/") ++) pictureFiles
      directories = filter isDirectory fps
      fullDirectories = map ((base ++ "/") ++) directories
  pictures' <- mapM loadAsset completePictureFiles
  let textures = zip (map Texture completePictureFiles) pictures' :: [(Texture, Picture)]
  otherPictureFiles <- mapM loadAssets fullDirectories
  let otherTextures = concat otherPictureFiles
  return $ textures ++ otherTextures

isPictureFile :: String -> Bool
isPictureFile file = bool
  where
    fileT = T.pack file
    bool = T.isInfixOf ".png" fileT || T.isInfixOf ".jpeg" fileT

isDirectory :: String -> Bool
isDirectory file = bool
  where
    fileT = T.pack file
    bool = not $ T.isInfixOf "." fileT
