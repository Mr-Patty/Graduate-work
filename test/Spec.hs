import AltMul
import Data.List

main :: IO ()
main =  do
  let x = 100423553124124 :: Int
      y = 820124124124125125 :: Int
  let vecX = intToVec x
      vecY = intToVec y
      len = fromIntegral $ max (length vecX) (length vecY)
      resX = extendVec vecX len
      resY = extendVec vecY len
  print (length resX)
  print resX
  print resY
  let
      resV = finalizeVec $ naiveMul resX resY
      -- resV = naiveMul vecX vecY
      res = x * y
  print resV
  print res
  let a = vecToInt resV
  print a
  print $ a == res
