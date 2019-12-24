module Calculator (
   Operation(..)
  ,NumKey(..)
  ,Input(..)
  ,Output(..)
  ,calculate
  ,calculateLog
  ) where

import OperandRegister

import Control.Arrow
import Control.Arrow.Transformer.Automaton(Automaton(..),runAutomaton)
import qualified Data.Stream as Stream


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

-- [TODO] output text digits
