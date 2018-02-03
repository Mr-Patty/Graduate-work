module Mul_Lib where

import Data.Vector

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

naive_mul :: BinaryVector -> BinaryVector -> BinaryVector
naive_mul _ 
