module AltMul where

import Prelude
import System.Random
import Data.List


-- Двоичное число
type Binary = Int

type BinaryList = [Binary]
-- Число в представлении списка двоичных чисел
-- type Vector = [Binary]


-- lenght of the long number for which naive multiplication
-- will be called in the Karatsuba function
lenfNaive :: Int
lenfNaive = 1

nullMul :: BinaryList -> BinaryList -> BinaryList
nullMul _ _ = []

fftMul :: BinaryList -> BinaryList -> BinaryList
fftMul a b = a `add` b

-- Умножение Карацубы
karatsubaMul :: BinaryList -> BinaryList -> BinaryList
karatsubaMul [a] [b] = [a * b, 0]
karatsubaMul x y =
  let
    len = (length x)
    k = len `div` 2
    xl = take k x
    xr = drop k x
    yl = take k y
    yr = drop k y
    prod1 = karatsubaMul xl yl
    prod2 = karatsubaMul xr yr
    prod3 = karatsubaMul (xr `add` xl) (yl `add` yr)
    middle = minus  prod3 prod1 prod2
    restmp = prod1 ++ prod2
  in
    if len <= lenfNaive then
      naiveMul x y
    else
      (take k restmp) ++ (drop k restmp `add` middle) ++ (drop (len + k) restmp)
      -- offset restmp middle k (k + (length middle))


offset :: BinaryList -> BinaryList -> Int -> Int -> BinaryList
offset [] [] _ _ = []
offset [] y _ _ = y
offset x [] _ _ = x
offset x y 0 0 = x
offset (x:xs) (y:ys) 0 k = (x + y) : offset xs ys 0 (k - 1)
offset (x:xs) y n k = x : offset xs y (n - 1) k

minus :: BinaryList -> BinaryList -> BinaryList -> BinaryList
minus [] _ _ = []
minus (a:as) (b:bs) (c:cs) = (a - b - c) : minus as bs cs
-- minus a b = zipWith (-) a b

add :: BinaryList -> BinaryList -> BinaryList
add a b = zipWith (+) a b

naiveMul :: BinaryList -> BinaryList -> BinaryList
naiveMul vecA vecB =
  let
    lenA = length vecA
    lenB = length vecB
    lenMax = max (length vecA) (length vecB)
    resVec = replicate (2 * lenMax) 0
    listVec = mulVec vecA vecB 0
  in
    foldl' add resVec listVec
    -- finalizeVec $ foldl' foldFunc resVec listVec

foldFunc :: BinaryList -> BinaryList -> BinaryList
foldFunc a b =
  let
    lenA = length a
    lenB = length b
  in
    if lenB > lenA then
      sumVec b a
    else
      sumVec a b

sumVec :: BinaryList -> BinaryList -> BinaryList
sumVec vecA vecB =
  case null vecB of
    False ->
      let
        a = head vecA
        b = head vecB
        vecAtail = tail vecA
        vecBtail = tail vecB
      in
        (a + b) : (sumVec vecAtail vecBtail)
    _ -> vecA


mulVec :: BinaryList -> BinaryList -> Int -> [BinaryList]
mulVec vecA vecB n =
  case null vecA of
    False ->
      let
        a = head vecA
        vecAEnd = tail vecA
        resNul = replicate n 0
      in
        (resNul ++ (fmap (\x -> x * a) vecB)) : mulVec vecAEnd vecB (n + 1)
    _ -> []

mulVecNum :: BinaryList -> Binary -> BinaryList
mulVecNum vec a =
  fmap (\x -> x * a) vec


-- Генерация рандомного вектора
randomVector :: Int -> IO BinaryList
randomVector n = do
  randGen <- newStdGen
  return $ take n $ randomRs (0 :: Binary,1) randGen


-- Расширение вектора
extendVec :: BinaryList -> Float -> BinaryList
extendVec vec n =
  if m > len then
    vec ++ (replicate (m - len) 0)
  else
    vec
  where
    m = round $ 2 ** (fromIntegral $ ceiling $ (logBase 2 n))
    len = length vec

intToVec :: Int -> BinaryList
intToVec 0 = []
intToVec n =
  let
    d = n `div` 2
    m = n `mod` 2
  in
    m : (intToVec d)

-- перевод из вектора в обычное число
vecToInt :: BinaryList -> Int
vecToInt = (\vec -> vecToIntAcc vec 0)

vecToIntAcc :: BinaryList -> Int -> Int
vecToIntAcc vec n =
  case null vec of
    False ->
      let
        a = head vec
        vecR = tail vec
      in
        a * (2^(n)) + (vecToIntAcc vecR $ n + 1)
    True -> 0


-- Привести вектор к нормальному виду
finalizeVec :: BinaryList -> BinaryList
finalizeVec vec = finalizeVecAcc vec 0

finalizeVecAcc :: BinaryList -> Binary -> BinaryList
finalizeVecAcc [] acc = [acc]
finalizeVecAcc (x:newVec) acc =
  (a `mod` 2) : (finalizeVecAcc newVec (a `div` 2))
  where
    a = (x + acc)
