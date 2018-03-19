module Mul_Lib where

import qualified Data.Vector as Vec
import System.Random
import qualified Data.List as List

  -- Двоичное число
type Binary = Int

type BinaryVector = Vec.Vector Binary
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

naiveMul :: BinaryVector -> BinaryVector -> BinaryVector
naiveMul vecA vecB =
  let
    lenA = Vec.length vecA
    lenB = Vec.length vecB
    lenMax = max (length vecA) (length vecB)
    resVec = Vec.replicate (2 * lenMax) 0
    listVec = mulVec vecA vecB 0
  in
    finalizeVec $ List.foldl' foldFunc resVec listVec

foldFunc :: BinaryVector -> BinaryVector -> BinaryVector
foldFunc a b =
  let
    lenA = Vec.length a
    lenB = Vec.length b
  in
    if lenB > lenA then
      sumVec b a
    else
      sumVec a b

sumVec :: BinaryVector -> BinaryVector -> BinaryVector
sumVec vecA vecB =
  case null vecB of
    False ->
      let
        a = Vec.head vecA
        b = Vec.head vecB
        vecAtail = Vec.tail vecA
        vecBtail = Vec.tail vecB
      in
        Vec.cons (a + b) $ sumVec vecAtail vecBtail
    _ -> vecA


mulVec :: BinaryVector -> BinaryVector -> Int -> [BinaryVector]
mulVec vecA vecB n =
  case null vecA of
    False ->
      let
        a = Vec.head vecA
        vecAEnd = Vec.tail vecA
        resNul = Vec.replicate n 0
      in
        (resNul Vec.++ (fmap (\x -> x * a) vecB)) : mulVec vecAEnd vecB (n + 1)
    _ -> []

mulVecNum :: BinaryVector -> Binary -> BinaryVector
mulVecNum vec a =
  fmap (\x -> x * a) vec

--
-- func2
--
-- func :: acc -> BinaryVector -> a -> BinaryVector

-- Генерация рандомного вектора
randomVector :: Int -> IO BinaryVector
randomVector n = do
  randGen <- newStdGen
  return $ Vec.fromList $ List.take n $ randomRs (0 :: Binary,1) randGen


-- Расширение вектора
extendVec :: BinaryVector -> Int -> BinaryVector
extendVec vec n =
  if n > len then
    vec Vec.++ (Vec.replicate (n - len) 0)
  else
    vec
  where
    len = Vec.length vec

intToVec :: Int -> BinaryVector
intToVec 0 = Vec.singleton 0
intToVec n =
  let
    d = n `div` 2
    m = n `mod` 2
  in
    Vec.singleton m Vec.++ (intToVec d)

-- перевод из вектора в обычное число
vecToInt :: BinaryVector -> Int
vecToInt = (\vec -> vecToIntAcc vec 0)

vecToIntAcc :: BinaryVector -> Int -> Int
vecToIntAcc vec n =
  case null vec of
    False ->
      let
        a = Vec.head vec
        vecR = Vec.tail vec
      in
        a * (2^(n)) + (vecToIntAcc vecR $ n + 1)
    True -> 0


-- Привести вектор к нормальному виду
finalizeVec :: BinaryVector -> BinaryVector
finalizeVec vec = finalizeVecAcc vec 0

finalizeVecAcc :: BinaryVector -> Binary -> BinaryVector
finalizeVecAcc vec acc =
  case Vec.null vec of
    True -> Vec.singleton acc
    False ->
      let
        a = (Vec.head vec) + acc
        newVec = Vec.tail vec
      in
        Vec.cons (a `mod` 2) $ finalizeVecAcc newVec (a `div` 2)
