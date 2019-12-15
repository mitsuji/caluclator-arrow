module Main where

import qualified Data.Stream as Stream
import Control.Arrow
import Control.Arrow.Transformer.Automaton(Automaton(..),runAutomaton)
import Control.Arrow.Transformer.Stream(StreamArrow(..),runStream)
import Data.Word(Word8)
import Data.Ratio(numerator,denominator)
import Data.Char (intToDigit)
import Data.Maybe (isJust,fromJust)
import System.IO(stdin,stdout,BufferMode(..),hSetEcho,hSetBuffering,hGetContents,hFlush)
import Control.Monad(forM_)


type ORScale = Word8

scaleMax :: ORScale
scaleMax = 12

data OperandRegister = ORIntegral Integer
                     | ORFractional Integer ORScale
                     deriving (Show)

appendDigit :: OperandRegister -> Word8 -> OperandRegister
appendDigit x@(ORIntegral v) n
  | (0 <= n && n <= 9) = ORIntegral $ (v * 10) + (fromIntegral n)
  | otherwise = x
appendDigit x@(ORFractional v s) n
  | (s < scaleMax) && (0 <= n && n <= 9) = ORFractional ((v * 10) + (fromIntegral n)) (s+1)
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



data Operation = Add | Sub | Mul | Div deriving (Show)

calc :: Operation -> OperandRegister -> OperandRegister -> OperandRegister
calc Add = (+)
calc Sub = (-)
calc Mul = (*)
calc Div = (/)

calc' op x = optimise . (calc op x)


data NumKey = N0 | N1 | N2 | N3 | N4 | N5 | N6 | N7 | N8 | N9 | Dot | BS deriving (Show)

keyinput :: NumKey -> OperandRegister -> OperandRegister
keyinput N0  = flip appendDigit 0
keyinput N1  = flip appendDigit 1
keyinput N2  = flip appendDigit 2
keyinput N3  = flip appendDigit 3
keyinput N4  = flip appendDigit 4
keyinput N5  = flip appendDigit 5
keyinput N6  = flip appendDigit 6
keyinput N7  = flip appendDigit 7
keyinput N8  = flip appendDigit 8
keyinput N9  = flip appendDigit 9
keyinput Dot = forceFractional
keyinput BS  = reduceDigit



data Input = Number NumKey | Operator Operation | Equal | Clear | AllClear deriving (Show)
data State = Operand1 OperandRegister | Operand2 OperandRegister Operation OperandRegister deriving (Show)
type Output = OperandRegister

calculator :: (ArrowLoop a, ArrowApply a) => Automaton a Input Output
calculator = operand1 zero
  where
    zero = ORIntegral 0
    
    operand1 r = Automaton $ arr eval &&& arr (next eval (Operand1 r))
      where
        eval (Number x) = keyinput x r
        eval (Operator _) = zero
        eval Equal = r
        eval Clear = zero
        eval AllClear = zero

    operand2 r1 op r2 = Automaton $ arr eval &&& arr (next eval (Operand2 r1 op r2))
      where
        eval (Number x) = keyinput x r2
        eval (Operator _) = calc' op r1 r2
        eval Equal = calc' op r1 r2
        eval Clear = zero
        eval AllClear = zero

    next :: (ArrowLoop a, ArrowApply a) => (Input -> Output) -> State -> Input -> Automaton a Input Output
    next ev (Operand1 _) i@(Number _) = operand1 $ ev i
    next ev (Operand1 r) i@(Operator x) = operand2 r x (ev i)
    next ev (Operand1 _) i@Equal = operand1 $ ev i
    next ev (Operand1 _) i@Clear = operand1 $ ev i
    next ev (Operand1 _) i@AllClear = operand1 $ ev i
    
    next ev (Operand2 r1 op _) i@(Number _) = operand2 r1 op $ ev i
    next ev (Operand2 _  _  _) i@(Operator x) = operand2 (ev i) x zero
    next ev (Operand2 _  _  _) i@Equal = operand1 $ ev i
    next ev (Operand2 r1 op _) i@Clear = operand2 r1 op $ ev i
    next ev (Operand2 _  _  _) i@AllClear = operand1 $ ev i

    
calculate :: Stream.Stream Input -> Stream.Stream OperandRegister
calculate xs = (runAutomaton $ arr snd >>> calculator) ((),xs)

calculateLog :: Stream.Stream Input -> Stream.Stream (Input ,OperandRegister)
calculateLog xs = (runAutomaton $ arr snd >>> (returnA &&& calculator)) ((),xs)


toInput :: Stream.Stream Char -> Stream.Stream Input
toInput xs = (runStream $ arr snd >>> filter' >>> map') ((),xs)
  where
    filter' = StreamArrow $ arr $ Stream.filter $ isJust . f
    map' = arr $ fromJust . f

    f :: Char -> Maybe Input
    f '0' = Just $ Number N0
    f '1' = Just $ Number N1
    f '2' = Just $ Number N2
    f '3' = Just $ Number N3
    f '4' = Just $ Number N4
    f '5' = Just $ Number N5
    f '6' = Just $ Number N6
    f '7' = Just $ Number N7
    f '8' = Just $ Number N8
    f '9' = Just $ Number N9
    f '.' = Just $ Number Dot
    f 'd' = Just $ Number BS
    f '+' = Just $ Operator Add
    f '-' = Just $ Operator Sub
    f '*' = Just $ Operator Mul
    f '/' = Just $ Operator Div
    f '\n' = Just Equal
    f '=' = Just Equal
    f 'e' = Just Equal
    f 'a' = Just AllClear
    f 'c' = Just Clear
    f x | fromEnum x == 0x7f = Just $ Number BS
        | otherwise = Nothing


fromOutput :: Stream.Stream OperandRegister -> Stream.Stream String
fromOutput xs = (runStream $ arr snd >>> (arr $ show . OperandRegister')) ((),xs)

fromOutputLog :: Stream.Stream (Input,OperandRegister) -> Stream.Stream (Input,String)
fromOutputLog xs = (runStream $ arr snd >>> second (arr $ show . OperandRegister')) ((),xs)



main :: IO ()
main = main1


main1 :: IO ()
main1 = do
  hSetBuffering stdin NoBuffering
  hSetEcho stdin False
  xs <- Stream.toList . fromOutput . calculate . toInput . Stream.fromList <$> hGetContents stdin
--  forM_ xs $ \x -> putStr "\r " >> putStr x >> putStr "          \b\b\b\b\b\b\b\b\b\b" >> hFlush stdout
  forM_ xs putStrLn

main2 :: IO ()
main2 = do
  hSetBuffering stdin NoBuffering
  hSetEcho stdin False
  xs <- Stream.toList . fromOutputLog . calculateLog . toInput . Stream.fromList <$> hGetContents stdin
  forM_ xs print
