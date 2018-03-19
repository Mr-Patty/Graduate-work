import Mul_Lib

main :: IO ()
main =  do
  let x = 3 :: Int
      y = 9 :: Int
  let vecX = intToVec x
      vecY = intToVec y
  let resV = naiveMul vecY vecX
      res = x * y
  print resV
  print res
  let a = vecToInt resV
  print a
  print $ a == res
