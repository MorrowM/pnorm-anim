module Main (main) where

import Codec.Picture
import Data.Bifunctor
import Data.Foldable
import Data.List
import Text.Printf

main :: IO ()
main = gif $ frameList (-20) 100
 where
  width = 500
  height = 500
  scaleX = 200
  scaleY = 200
  fromInt = bimap fromIntegral fromIntegral
  scale = bimap (/ scaleX) (/ scaleY)
  shift (x, y) = (x - width `div` 2, y - height `div` 2)
  pToImage genPixelFun p = generateImage (curry $ genPixelFun p . scale . fromInt . shift) width height

  -- Generate PNGs
  _pngs ps = for_ ps $ \p -> writePng (printf "%.2f-circle.png" p) $ pToImage genPixelGrey p

  -- Generate a GIF
  gif ps =
    either putStrLn id $
      writeGifImages
        "circle.gif"
        LoopingForever
        [ (greyPaletteCompressed, 1, pToImage genPixelGrey p) | p <- ps
        -- , let (img, palette) = palettize paletteOpts (pToImage genPixel p)
        ]

  -- Color palette stuff, unused
  _paletteOpts =
    PaletteOptions
      { paletteCreationMethod = MedianMeanCut
      , enableImageDithering = True
      , paletteColorCount = 256
      }

frameList :: (Ord a, Floating a) => a -> a -> [a]
frameList lower upper = unfoldr (\acc -> if acc > upper then Nothing else Just (acc, acc + log ((abs acc + 2) ** (1 / 5)))) lower

pnorm :: Floating a => a -> a -> a -> a
pnorm p x y = (abs x ** p + abs y ** p) ** (1 / p)

sigmoid :: Floating a => a -> a
sigmoid x = 2 / (exp (-x) + 1) - 1

-- Tried doing color stuff, wasn't very interesting
_genPixel :: Double -> (Double, Double) -> PixelRGB8
_genPixel p (x, y) = PixelRGB8 red green blue
 where
  red = toWord8 $ strength 2 / 2
  blue = toWord8 $ strength p
  green = toWord8 $ strength 2 / 2

  toWord8 z = floor $ 127 * z
  strength p0 = sigmoid $ pnorm p0 x y

genPixelGrey :: Double -> (Double, Double) -> Pixel8
genPixelGrey p (x, y) = floor $ 127 * strength
 where
  strength = sigmoid $ pnorm p x y

greyPaletteCompressed :: Palette
greyPaletteCompressed = generateImage toGrey 256 1
 where
  toGrey x _ = PixelRGB8 ix ix ix
   where
    ix = fromIntegral $ (x `div` 16) * 16