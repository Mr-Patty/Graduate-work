module Lib where
-- import Data.Time
-- import Data.Time.Clock.System
import Data.List
import Mul_Lib as Mul
import Data.Int
import Data.Word
import Criterion.Measurement

nameFile :: FilePath
nameFile = "out/test.txt"

someFunc :: IO ()
someFunc = do
  initializeTime
  t1 <- getTime
  let res = myMul 3 4
  t2 <- getTime
  print 0
  -- print $ t2
  -- writeFile nameFile $ show $ diffUTCTime t2 t1
  -- print res
  -- writeFile nameFile $ "lol\n"
  -- appendFile nameFile $ "kek"

-- funckaratsubaMul :: IO ()
-- funckaratsubaMul = do
--   -- initializeTime
--   -- fmap (\x -> (x / fromInteger numberOfRepetitions)) $
--   fmap sum $ sequence $ map func [1..numberOfRepetitions]
--   -- return (a / numberOfRepetitions)
--   where
--     func _ = do
--         -- start <- getCPUTime
--         res <- myMul 4214124 40000
--         -- stop  <- getCPUTime
--         -- test <- getCPUTime
--         -- print test
--         -- print $ stop - start
--         -- return $ stop - start
--         return res


-- getTime =
--   do
--     time <- getSystemTime
--     return $ getSec time

-- getSec :: SystemTime -> String
-- getSec (MkSystemTime a b) = show a?

-- random_vector :: Int -> Vector

-- naiveMul :: Vector Int -> Vector Int -> Vector Int




myMul :: Int -> Int -> Int
myMul a b = a * b
