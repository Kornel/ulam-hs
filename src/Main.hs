{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE TypeOperators   #-}

module Main (main) where

import Codec.Picture
import Control.Monad
import Control.Monad.ST
import System.Environment (getArgs)
import System.FilePath (replaceExtension)
import qualified Codec.Picture.Types as M

main :: IO ()
main = do
  [path] <- getArgs
  imageCreator path

imageCreator path = writePng path $ generateImage pixelRenderer 1200 1200
  where pixelRenderer x y = PixelRGB8 (fromIntegral x) (fromIntegral y) 128
