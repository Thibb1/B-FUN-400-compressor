module Types where

data Options = Options {
  colorNb :: Int, convergence :: Float, path :: String
}

data Coords = Coords {
  x :: Int, y :: Int
}

instance Show Coords where
  show (Coords x y) = "(" ++ show x ++ "," ++ show y ++ ")"

newCoords :: Int -> Int -> Coords
newCoords = Coords

data Color = Color {
  r :: Int, g :: Int, b :: Int
}

instance Show Color where
  show (Color r g b) = "(" ++ show r ++ "," ++ show g ++ "," ++ show b ++ ")"

newColor :: Int -> Int -> Int -> Color
newColor = Color

data Pixel = Pixel {
  pCoords :: Coords,
  pColor :: Color
}

instance Show Pixel where
  show (Pixel coords color) = show coords ++ " " ++ show color

newPixel :: Coords -> Color -> Pixel
newPixel = Pixel

showPixels :: [Pixel] -> [String]
showPixels = map show

data Cluster = Cluster {
  cColor :: Color,
  cPixels :: [Pixel]
}

instance Show Cluster where
  show (Cluster color pixels) = "--\n" ++ show color ++ "\n-" ++ unlines (showPixels pixels)

showClusters :: [Cluster] -> [String]
showClusters = map show