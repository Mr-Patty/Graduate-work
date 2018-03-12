module Mul_Lib where

import Data.Vector as Vec

  -- Двоичное число
type Binary = Int

newtype BinaryVector = Vector Binary
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
--
-- naiveMul :: BinaryVector -> BinaryVector -> BinaryVector
-- naiveMul vecA vecB =
--   let
--     lenA = lenght vecA
--     lenB = lenght vecB
--     lenMax = max lenA lenB
--     vecRes = replicate lenMax 0
--     listVec =
--   in
--
--
-- func2
--
-- func :: acc -> BinaryVector -> a -> BinaryVector

-- Расширение вектора
extendVec :: Vector Binary -> Int -> Vector Binary
extendVec vec n =
  if len > n then
    vec Vec.++ (Vec.replicate (len - n) 0)
  else
    vec
  where
    len = Vec.length vec

-- Привести вектор к нормальному виду
finalizeVec :: Vector Binary -> Vector Binary
finalizeVec vec = finalizeVecAcc vec 0

finalizeVecAcc :: Vector Binary -> Binary -> Vector Binary
finalizeVecAcc vec acc =
  case Vec.null vec of
    True -> Vec.singleton acc
    False ->
      let
        a = (Vec.head vec) + acc
        newVec = Vec.tail vec
      in
        Vec.cons (a `mod` 2) $ finalizeVecAcc newVec (a `div` 2)
