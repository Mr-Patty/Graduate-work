module Main where
{-# LANGUAGE BangPatterns #-}
import Lib
-- import Data.Time.Clock.System
import Criterion.Measurement
import Control.DeepSeq

main :: IO ()
main = do
  initializeTime
  res <- averageTime (myMul) 49094 40912
  print res


averageTime :: (Int -> Int -> Int) -> Int -> Int -> IO Double
averageTime f a b = do
   a <- fmap sum $ sequence $ fmap (\x -> x `deepseq` applyFunction f a b) [1..numberOfRepetitions]
   -- print a
   return $ a / fromInteger numberOfRepetitions

-- количество повторений
numberOfRepetitions :: Integer
numberOfRepetitions = 10000

applyFunction :: (Int -> Int -> Int) -> Int -> Int -> IO Double
applyFunction f a b = do
  start <- getCPUTime
  let !res = f a b
  stop  <- res `deepseq` getCPUTime
  -- print $ (stop - start)
  return $ (stop - start) * 100000000
