module OperandRegister (
   OperandRegister
  ,zero
  ,appendDigit
  ,forceFractional
  ,reduceDigit
  ,optimise
  ,OperandRegister'(..)
  ) where

import Data.Word(Word8)
import Data.Ratio(numerator,denominator)
import Data.Char (intToDigit)


type ORScale = Word8

scaleMax :: ORScale
scaleMax = 12

data OperandRegister = ORIntegral Integer
                     | ORFractional Integer ORScale
                     deriving (Show)

zero :: OperandRegister
zero = ORIntegral 0

-- [TODO] max digits
--             9
--  999999999999 ORIntegral   999999999999
-- 9.99999999999 ORFractional 999999999999 11
-- 99999999999.9 ORFractional 999999999999  1
appendDigit :: OperandRegister -> Word8 -> OperandRegister
appendDigit x@(ORIntegral v) n
  | (0 <= n && n <= 9) && (v < 10^(scaleMax-1)) = ORIntegral $ (v * 10) + (fromIntegral n)
  | otherwise = x
appendDigit x@(ORFractional v s) n
  | (0 <= n && n <= 9) && (v < 10^(scaleMax-1)) && (s < scaleMax-1) = ORFractional ((v * 10) + (fromIntegral n)) (s+1)
  | otherwise = x

forceFractional :: OperandRegister -> OperandRegister
forceFractional (ORIntegral v) = ORFractional v 0
forceFractional x = x

reduceDigit :: OperandRegister -> OperandRegister
reduceDigit (ORIntegral v) = ORIntegral $ v `div` 10
reduceDigit (ORFractional v 0) = ORIntegral v
reduceDigit (ORFractional v s) = ORFractional (v `div` 10) (s-1)

optimise :: OperandRegister -> OperandRegister
optimise x@(ORIntegral _) = x
optimise x = f x
  where
    f (ORFractional v 0) = ORIntegral v
    f x@(ORFractional v s) =
      let
        (d,m) = divMod v 10
      in if m == 0
         then f $ ORFractional d (s-1)
         else x


instance Num OperandRegister where
  (+) x@(ORFractional xv xs) y@(ORFractional yv ys) | xs >= ys  = ORFractional (xv+(yv*(10^(xs-ys)))) xs
                                                    | otherwise = (+) y x
  (+) x y = (+) (forceFractional x) (forceFractional y)

  (*) (ORFractional xv xs) (ORFractional yv ys) | (xs+ys) <= scaleMax = ORFractional (xv*yv) (xs+ys)
                                                | otherwise = ORFractional ((xv*yv) `div` (10^((xs+ys)-scaleMax))) scaleMax
  (*) x y = (*) (forceFractional x) (forceFractional y)
 
  abs (ORFractional v s) = ORFractional (abs v) s
  abs (ORIntegral v) = ORIntegral $ abs v
  
  signum (ORFractional v _) = ORIntegral $ signum v
  signum x = signum $ forceFractional x
  
  negate (ORFractional v s) = ORFractional (negate v) s
  negate (ORIntegral v) = ORIntegral $ negate v
  
  fromInteger x = ORIntegral x


divide :: OperandRegister -> OperandRegister -> OperandRegister
divide (ORFractional xv xs) (ORFractional yv ys) =
  let
    s = max xs ys
    xv' = xv * (10^(s-xs)) * (10^scaleMax)
    yv' = yv * (10^(s-ys))
  in ORFractional (xv' `div` yv') scaleMax
  
divide x y = divide (forceFractional x) (forceFractional y)

instance Fractional OperandRegister where
  (/) x y = x `divide` y
  fromRational x = (ORIntegral $ numerator x) `divide` (ORIntegral $ denominator x)

    

newtype OperandRegister' = OperandRegister' OperandRegister

-- [TODO] output text digits
instance Show OperandRegister' where
  show (OperandRegister' (ORIntegral v)) = f v []
    where
      f n xs = if n >= 0
               then f' n xs
               else '-': f' (abs n) xs

      f' 0 xs = if length xs == 0
               then "0"
               else xs

      f' n xs =
        let
          (d,m) = divMod n 10
        in f' d $ (intToDigit $ fromIntegral m) : xs

  show (OperandRegister' (ORFractional v s)) = mconcat $ f v []
    where
      f n xs = if n >= 0
               then f' n xs
               else "-": f' (abs n) xs

      f' 0 xs =
        let
          zs = fromIntegral s - length xs
        in if 0 <= zs
           then ("0." <> replicate zs '0') : xs
           else xs

      f' n xs =
        let
          (d,m) = divMod n 10
          m' = intToDigit $ fromIntegral m
          x = if length xs == fromIntegral s
              then [m','.']
              else [m']
        in f' d (x:xs)

