module Main (main) where

import Codec.Picture
import System.Environment (getArgs)
import System.FilePath (replaceExtension)
import Data.Array
import Data.Numbers.Primes
import Debug.Trace
import Data.Word (Word8)


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


xCoords :: Int -> Int -> [Int]
xCoords n shift = coords n shift dxs


yCoords :: Int -> Int -> [Int]
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


pixelColor :: Bool -> Word8
pixelColor setPixel
  | setPixel == True = 0
  | otherwise        = 255


renderPixel :: (Array (Int, Int) Bool) -> Int -> Int -> PixelRGB8
renderPixel coords x y =
  let
    setPixel = coords!(x,y)
    color = pixelColor setPixel
  in
    PixelRGB8 color color color


createImage :: Int -> String -> (Array (Int, Int) Bool) -> IO ()
createImage size path coords =
  let
    renderer = renderPixel coords
    image = generateImage renderer size size
  in
    writePng path image


outputPath :: [String] -> String
outputPath [] = "out.png"
outputPath (x:_) = x


main :: IO ()
main = do
  args <- getArgs
  let
    path = outputPath args
    size = 2001
    coords = coordsArray size
  putStrLn $ "Writing spiral of width " ++ (show size) ++ " at " ++ path
  createImage size path coords
