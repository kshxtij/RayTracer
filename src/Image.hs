{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE FlexibleContexts #-}

module Image (writeImage) where

import Camera (camera, rayTowards)
import Colors (Color, ColorVec, SampledColor (SampledColor), toColor)
import Control.Exception (evaluate)
import Data.Maybe (fromMaybe)
import Hittable (HitRecord (HitRecord), Hittable (hit), Material (Material), scatter)
import Numeric.Limits (maxValue)
import Ray (Ray (Ray))
import Samplings (sampleFraction)
import System.Directory (createDirectoryIfMissing, renameFile)
import System.IO (IOMode (WriteMode), hPutStrLn, withFile)
import System.Random (RandomGen, mkStdGen)
import Text.Printf (printf)
import Utils (getSecondsNow, loop)
import Vec (getY, len, one, unit, vec, zero, (.*.))

data Image = Image
  { imageWidth :: Int,
    imageHeight :: Int,
    imageColors :: [Color]
  }
  deriving (Show)

aspectRatio :: Double
aspectRatio = 16.0 / 9.0

writeImage :: Hittable a => Int -> Int -> Int -> a -> IO ()
writeImage width samplesPerPixel raysPerSample world = do
  createDirectoryIfMissing True "images"
  let height = floor $ fromIntegral width / aspectRatio
  let image = createImage width height samplesPerPixel raysPerSample world
  start <- getSecondsNow
  let mkFilename secs rate =
        printf "images/%dx%d.%d-%d.%dm-%ds.%d.ppm" width height samplesPerPixel raysPerSample (secs `div` 60) (secs `rem` 60) (floor start :: Int)
  colors <- evaluate $ imageColors image
  end <- getSecondsNow
  let elapsed = end - start
  let pixelRate = fromIntegral (height * width) / elapsed
  let filename = mkFilename (floor elapsed :: Int) (floor pixelRate :: Int)
  withFile filename WriteMode $ \h -> do
    hPutStrLn h "P3"
    hPutStrLn h $ show width ++ " " ++ show height
    hPutStrLn h $ show 255
    hPutStrLn h $ unlines $ fmap show colors
  putStrLn $ "time elapsed      : " ++ (printf "%.3f" elapsed ++ " seconds")
  putStrLn $ "pixels per second : " ++ printf "%.3f" pixelRate
  putStrLn $ "Image produced    : " ++ filename

createImage :: Hittable a => Int -> Int -> Int -> Int -> a -> Image
createImage width height samplesPerPixel raysPerSample world = Image width height colors
  where
    lookFrom = vec 13 2 13
    lookAt = vec 0 0 0
    viewUp = vec 0 1 0
    focusDistance = len $ lookFrom - lookAt
    aperture = 0.1
    cam = camera lookFrom lookAt viewUp 20.0 aspectRatio aperture focusDistance
    coordinates = (,) <$> reverse [0 .. height -1] <*> [0 .. width -1]
    colors = map computeColor coordinates
    computeColor (j, i) = color
      where
        g = mkStdGen (i * width + j)
        (sampledColor, _) = loop sampledRayColor (SampledColor (samplesPerPixel, toColor zero), g) samplesPerPixel
        color = toColor sampledColor
        sampledRayColor (acc, g'') _ = (c + acc, g4)
          where
            (r1, g1) = sampleFraction g''
            (r2, g2) = sampleFraction g1
            u = (fromIntegral i + r1) / fromIntegral (width - 1)
            v = (fromIntegral j + r2) / fromIntegral (height - 1)
            (ray, g3) = rayTowards cam u v g2
            (colorVec, g4) = rayColor ray world g3 raysPerSample (toColor one)
            c = SampledColor (samplesPerPixel, colorVec)

rayColor :: (Hittable a, RandomGen g) => Ray -> a -> g -> Int -> ColorVec -> (ColorVec, g)
rayColor ray@(Ray _origin direction) world g raysPerSample !acc = if raysPerSample <= 0 then (toColor zero, g) else computeColor
  where
    h = hit world ray 0.001 maxValue
    t = 0.5 * (getY (unit direction) + 1.0)
    default_color = toColor $ one .*. (1.0 - t) + vec 0.5 0.7 1.0 .*. t
    computeColor = fromMaybe (acc * default_color, g) $ do
      record@(HitRecord _ _ (Material m) _ _) <- h
      let (maybeScattered, g1) = scatter m ray record g
      return $ fromMaybe (toColor zero, g1) $ do
        (scatteredRay, attenuation) <- maybeScattered
        return $ rayColor scatteredRay world g1 (raysPerSample - 1) (acc * attenuation)
