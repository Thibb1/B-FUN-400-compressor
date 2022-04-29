module Compressor where

import System.Random ( StdGen, next, mkStdGen )
import Data.List ( sortOn )
import Lib
import Types

imageCompressor :: Options -> IO ()
imageCompressor (Options n l f) = do
  file <- fileMaybe f
  case file of
    Nothing -> throw "File not found"
    Just file -> case readPixels file of
      Nothing -> throw "File not supported"
      Just pixels -> printList $ showClusters (compresspixels pixels n l)

-- select a pixel in a list a pixel with a random index if the color is already
-- in the cluster take a new random pixel and reduce the
-- initCluster :: [Cluster] -> [Pixel] -> Int -> StdGen -> [Cluster]
initCluster :: [Cluster] -> [Pixel] -> Int -> [Cluster]
initCluster cluster pixels 0 = cluster
initCluster _ [] _ = []
initCluster cluster (pixel@(Pixel _ color):pixels) n
  | colorExists cluster color = initCluster cluster pixels n
  | otherwise = initCluster (Cluster color [pixel]:cluster) pixels (n-1)
-- initCluster cluster pixels n gen = do
--   [index, newGen] <- randomR (0, length pixels - 1) gen
--   pixel <- pixels !! index
--   case colorExists cluster (color pixel) of
--     True -> initCluster cluster pixels n newGen
--     False -> initCluster (Cluster color [pixel]:cluster) pixels (n-1) newGen
-- initCluster cluster (pixel@(Pixel _ color):pixels) n
--   | colorExists cluster color = initCluster cluster pixels n
--   | otherwise = initCluster (Cluster color [pixel]:cluster) pixels (n-1)



colorExists :: [Cluster] -> Color -> Bool
colorExists [] _ = False
colorExists (Cluster c1 _:cluster) c2
  | c1 == c2 = True
  | otherwise = colorExists cluster c2

compresspixels :: [Pixel] -> Int -> Float -> [Cluster]
-- compresspixels p n = do
--   seed <- mkStdGen 0
--   compressCluster (initCluster [] p n seed) [] p
compresspixels p n = compressCluster (initCluster [] p n) [] p

compressCluster :: [Cluster] -> [Cluster] -> [Pixel] -> Float -> [Cluster]
compressCluster c1 [] pixels l =
  compressCluster c1 (createCluster c1 pixels) pixels l
compressCluster c1 c2 pixels l
 | converged l c2 = c1
 | otherwise = compressCluster c2 [] pixels l

createCluster :: [Cluster] -> [Pixel] -> [Cluster]
createCluster cluster = foldl assignCluster (meanCluster cluster)

assignCluster :: [Cluster] -> Pixel -> [Cluster]
assignCluster cluster pixel =
  assignCluster' (sortOn (distance pixel) cluster) pixel
    where
      assignCluster' [] _ = []
      assignCluster' (Cluster c1 p1:cluster) pixel =
        Cluster c1 (pixel:p1):cluster

meanCluster :: [Cluster] -> [Cluster]
meanCluster = map meanCluster'
  where
    meanCluster' (Cluster color pixels) = Cluster (meanPixels pixels) []

distanceColor :: Color -> Color -> Float
distanceColor (Color r1 g1 b1) (Color r2 g2 b2) =
  sqrt ((r1 - r2)^2 + (g1 - g2)^2 + (b1 - b2)^2)

distance :: Pixel -> Cluster -> Float
distance (Pixel _ c1) (Cluster c2 _) = distanceColor c1 c2

-- closestPixel :: [Pixel] -> Pixel -> Pixel
-- closestPixel pixels p = minimumBy (compare `on` distance p) pixels
-- closestPixel pixels p = snd $ minimum $ zip (map (distance p) pixels) pixels

meanPixels :: [Pixel] -> Color
meanPixels pixels = Color (mean r) (mean g) (mean b)
  where
    r = sum $ map (\(Pixel _ (Color r _ _)) -> r) pixels
    g = sum $ map (\(Pixel _ (Color _ g _)) -> g) pixels
    b = sum $ map (\(Pixel _ (Color _ _ b)) -> b) pixels
    mean x = x / fromIntegral (length pixels)

converged :: Float -> [Cluster] -> Bool
converged _ [] = True
converged l (Cluster color pixels:cluster)
  | distanceColor color (meanPixels pixels) > l = False
  | otherwise = converged l cluster