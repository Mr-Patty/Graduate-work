
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

-- количество повторений
numberOfRepetitions :: Integer
numberOfRepetitions = 10000

someFunc :: IO ()
someFunc = do
  -- t1 <- getCurrentTime
  res <- mulNaive 3 4
  -- t2 <- getCurrentTime
  -- print $ t1
  -- print $ t2
  -- writeFile nameFile $ show $ diffUTCTime t2 t1
  print res
  -- writeFile nameFile $ "lol\n"
  -- appendFile nameFile $ "kek"

-- funckaratsubaMul :: IO NominalDiffTime
funckaratsubaMul = do
  initializeTime
  -- fmap (\x -> (x / fromInteger numberOfRepetitions)) $
  fmap sum $ sequence $ map func [1..numberOfRepetitions]
  -- return (a / numberOfRepetitions)
  where
    func _ = do
        start <- getCPUTime
        res <- mulNaive 4214124 40000
        stop  <- getCPUTime
        -- test <- getCPUTime
        -- print test
        -- print $ stop - start
        return $ stop - start


-- getTime =
--   do
--     time <- getSystemTime
--     return $ getSec time

-- getSec :: SystemTime -> String
-- getSec (MkSystemTime a b) = show a?

-- random_vector :: Int -> Vector


mulNaive :: Int -> Int -> IO Int
mulNaive a b = return (a * b)
