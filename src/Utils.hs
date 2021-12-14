module Utils
  ( loop,
    getSecondsNow,
    getSecondsElapsed,
    getCurrentTimeString,
    printCurrentTime,
    clamp
  )
where

import Data.Time.Clock (getCurrentTime)
import Data.Time.Clock.System (getSystemTime, systemNanoseconds, systemSeconds)
import Data.Time.Format (defaultTimeLocale, formatTime)
import Data.Time.LocalTime (getCurrentTimeZone, utcToZonedTime)

{-# INLINE loop #-}
loop :: (a -> Int -> a) -> a -> Int -> a
loop f v n = go 0 v
  where
    go i arg
      | i == n = arg
      | otherwise = go (i + 1) (f arg i)

getSecondsNow :: IO Double
getSecondsNow = do
  t <- getSystemTime
  let s1 = fromIntegral (systemSeconds t)
  let s2 = fromIntegral (systemNanoseconds t) / 1000000000
  return $ s1 + s2

getSecondsElapsed :: IO a -> IO (a, Double)
getSecondsElapsed action = do
  start <- getSecondsNow
  result <- action
  end <- getSecondsNow
  return (result, end - start)

getCurrentTimeString :: IO String
getCurrentTimeString = do
  startTime <- getCurrentTime
  tz <- getCurrentTimeZone
  let now = utcToZonedTime tz startTime
  return $ formatTime defaultTimeLocale "%c" now

printCurrentTime :: (String -> String) -> IO ()
printCurrentTime f = do
  now <- getCurrentTimeString
  putStrLn $ f now

clamp :: Ord a => a -> a -> a -> a
clamp lo hi val = min hi (max lo val)