{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE TypeOperators   #-}

module Main (main) where

import Codec.Picture
import Control.Monad
import Control.Monad.ST
import System.Environment (getArgs)
import System.FilePath (replaceExtension)
import qualified Codec.Picture.Types as M
import Data.Array
import Data.Numbers.Primes
import Debug.Trace


xTerm :: Int -> Int
xTerm n =
  (-1)^((n+1) `mod` 2)


yTerm :: Int -> Int
yTerm n =
  (-1)^(n `mod` 2)

dxs :: Int -> [Int]
dxs n =
  take (n^2 - 1) $ concat $ map (\x -> replicate x (xTerm x) ++ replicate x 0) [1..n]


dys :: Int -> [Int]
dys n =
  take (n^2 - 1) $ concat $ map (\x -> replicate x 0 ++ replicate x (yTerm x)) [1..n]


computeCoords :: Int -> [Int] -> [Int]
computeCoords shift ds =
	map (\x -> x + shift) $ scanl (+) 0 ds

coords :: Int -> Int -> (Int -> [Int]) -> [Int]
coords n shift f =
	computeCoords shift (f n)


xCoords n shift = coords n shift dxs
yCoords n shift = coords n shift dys


coordsArray :: Int -> (Array (Int, Int) Bool)
coordsArray size =
	let
		n = size
		shift = quot size 2
		xs = xCoords n shift
		ys = yCoords n shift
		ix = zip xs ys
	in
		array ((0, 0), (size - 1, size - 1)) $ zip ix (map isPrime [1..n^2])

main :: IO ()
main =
	let
		path = "out.png"
		size = 601
		coords = coordsArray size
  in
		imageCreator size path coords

imageCreator :: Int -> String -> (Array (Int, Int) Bool) -> IO ()
imageCreator size path coords = writePng path $ generateImage pixelRenderer size size
  where pixelRenderer x y = PixelRGB8 (fromIntegral $ (if (coords!(x,y)) then 0 else 255)) 255 255
