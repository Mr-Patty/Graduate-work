module Main where
{-# LANGUAGE BangPatterns #-}
import Lib
import Mul_Lib
import qualified AltMul as Alt
-- import Data.Time.Clock.System
import Criterion.Measurement
import Control.DeepSeq
import System.IO
import System.Environment

type Function = (BinaryVector -> BinaryVector -> BinaryVector)

type FunctionAlt = (Alt.BinaryList -> Alt.BinaryList -> Alt.BinaryList)
-- количество повторений
numberOfRepetitions :: Integer
numberOfRepetitions = 10

main :: IO ()
main = do
  (a:b:list) <- getArgs
  let start = (read a) :: Int
      stop = (read b) :: Int
  -- print start
  -- print stop
  -- res <- averageTime (myMul) 49094 40912
  -- initializeTime
  mainIOAlt start stop
  -- resAlt <- startFunctionAlt Alt.naiveMul start
  -- print resAlt
  -- res <- startFunction naiveMul start
  -- print res

startFunctionAlt :: FunctionAlt -> Int -> IO Double
startFunctionAlt f1 k = do
  vecA <- Alt.randomVector k
  vecB <- Alt.randomVector k
  applyFunctionAlt f1 vecA vecB

applyFunctionAlt :: FunctionAlt -> Alt.BinaryList -> Alt.BinaryList  -> IO Double
applyFunctionAlt f a b = do
  start <- getCPUTime
  let !res = f a b
  stop  <- res `deepseq` getCPUTime
  return $ (stop - start) * 100000

mainIOAlt :: Int -> Int -> IO ()
mainIOAlt start stop = do
  handle <- openFile "test-alt.txt" WriteMode
  mapM (\x -> do
              res <- averageTimeAlt (Alt.naiveMul) numberOfRepetitions x
              print (x, res)
              hPutStrLn handle $ outputFormat x res) [start,(start + 100)..stop]
  hClose handle


averageTimeAlt :: FunctionAlt -> Integer -> Int -> IO Double
averageTimeAlt f1 n k = do
  a <- fmap sum $ sequence $ totalTimeAlt f1 numberOfRepetitions k
  return $ a / fromInteger n

totalTimeAlt :: FunctionAlt -> Integer -> Int -> [IO Double]
totalTimeAlt f1 0 _ = []
totalTimeAlt f1 n k = do
  (startFunctionAlt f1 k) : (totalTimeAlt f1 (n - 1) k)


------------------------   ------------------------

mainIO :: Int -> Int -> IO ()
mainIO start stop = do
  handle <- openFile "test.txt" WriteMode
  mapM (\x -> do
              res <- averageTime (naiveMul) numberOfRepetitions x
              print res
              hPutStrLn handle $ outputFormat x res) [start,(start + 10)..stop]
  hClose handle

outputFormat :: Int -> Double -> String
outputFormat x y =
  "(" ++ (show x) ++ ";" ++ (show y) ++ ")"

averageTime :: Function -> Integer -> Int -> IO Double
averageTime f1 n k = do
  a <- fmap sum $ sequence $ totalTime f1 numberOfRepetitions k
  return $ a / fromInteger n

totalTime :: Function -> Integer -> Int -> [IO Double]
totalTime f1 0 _ = []
totalTime f1 n k = do
  (startFunction f1 k) : (totalTime f1 (n - 1) k)

startFunction :: Function -> Int -> IO Double
startFunction f1 k = do
  vecA <- randomVector k
  vecB <- randomVector k
  applyFunction f1 vecA vecB

applyFunction :: Function -> BinaryVector -> BinaryVector -> IO Double
applyFunction f a b = do
  start <- getCPUTime
  let !res = f a b
  stop  <- res `deepseq` getCPUTime
  return $ (stop - start) * 100000
