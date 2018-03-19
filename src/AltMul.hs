module AltMul where

import qualified Data.Vector as Vec
import System.Random
import qualified Data.List as List


-- Двоичное число
type Binary = Int

type BinaryList = [Binary]
-- Число в представлении списка двоичных чисел
-- type Vector = [Binary]


-- Умножение Карацубы
-- karatsubaMul :: Vector -> Vector -> Vector
-- karatsuba_mu

-- Из двоичного представление в десятичное
-- vectorToInt :: Vector -> Int

-- Расширение вектора
-- extendVec :: Vector -> Int -> Vector
-- extendVec a _ = a

naiveMul :: BinaryList -> BinaryList -> BinaryList
naiveMul vecA vecB =
  let
    lenA = List.length vecA
    lenB = List.length vecB
    lenMax = max (length vecA) (length vecB)
    resVec = List.replicate (2 * lenMax) 0
    listVec = mulVec vecA vecB 0
  in
    finalizeVec $ List.foldl' foldFunc resVec listVec

foldFunc :: BinaryList -> BinaryList -> BinaryList
foldFunc a b =
  let
    lenA = List.length a
    lenB = List.length b
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
        a = List.head vecA
        b = List.head vecB
        vecAtail = List.tail vecA
        vecBtail = List.tail vecB
      in
        (a + b) : (sumVec vecAtail vecBtail)
    _ -> vecA


mulVec :: BinaryList -> BinaryList -> Int -> [BinaryList]
mulVec vecA vecB n =
  case null vecA of
    False ->
      let
        a = List.head vecA
        vecAEnd = List.tail vecA
        resNul = List.replicate n 0
      in
        (resNul List.++ (fmap (\x -> x * a) vecB)) : mulVec vecAEnd vecB (n + 1)
    _ -> []

mulVecNum :: BinaryList -> Binary -> BinaryList
mulVecNum vec a =
  fmap (\x -> x * a) vec

--
-- func2
--
-- func :: acc -> BinaryList -> a -> BinaryList

-- Генерация рандомного вектора
randomVector :: Int -> IO BinaryList
randomVector n = do
  randGen <- newStdGen
  return $ List.take n $ randomRs (0 :: Binary,1) randGen


-- Расширение вектора
extendVec :: BinaryList -> Int -> BinaryList
extendVec vec n =
  if n > len then
    vec List.++ (List.replicate (n - len) 0)
  else
    vec
  where
    len = List.length vec

intToVec :: Int -> BinaryList
intToVec 0 = [0]
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
        a = List.head vec
        vecR = List.tail vec
      in
        a * (2^(n)) + (vecToIntAcc vecR $ n + 1)
    True -> 0


-- Привести вектор к нормальному виду
finalizeVec :: BinaryList -> BinaryList
finalizeVec vec = finalizeVecAcc vec 0

finalizeVecAcc :: BinaryList -> Binary -> BinaryList
finalizeVecAcc vec acc =
  case List.null vec of
    True -> [acc]
    False ->
      let
        a = (List.head vec) + acc
        newVec = List.tail vec
      in
        (a `mod` 2) : (finalizeVecAcc newVec (a `div` 2))
