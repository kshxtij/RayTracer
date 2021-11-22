module Main (main) where

import Image (writeImage)
import Scenes (genRandomScene)
import System.Environment (getArgs)
import Utils (getSecondsNow, printCurrentTime)

main :: IO ()
main = do
  printCurrentTime $ \now -> "Program started at " ++ now
  args <- getArgs
  seed <- getSecondsNow
  let [width, samplesPerPixel, raysPerSample] = fmap read args
  let scene = genRandomScene (floor seed)
  writeImage width samplesPerPixel raysPerSample scene
  printCurrentTime $ \now -> "Program stopping at " ++ now
