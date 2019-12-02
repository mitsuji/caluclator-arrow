module Main where

import qualified Data.Stream as Stream
import Control.Arrow
import Control.Arrow.Transformer.Automaton(Automaton(..),runAutomaton)


data Operation = Add | Sub | Mul | Div deriving (Show)
calc :: Operation -> Float -> Float -> Float
calc Add = (+)
calc Sub = (-)
calc Mul = (*)
calc Div = (/)

data Input = Clear | Number Float | Operator Operation deriving (Show)

data State = Init | Done Float | Operating Float Operation

calculator :: (ArrowLoop a, ArrowApply a) => Automaton a Input Float
calculator = init
  where
    init = Automaton $ arr r &&& arr (next Init)
      where
        r (Number x) = x -- new Number
        r _ = 0.0
    
    done n = Automaton $ arr (r n) &&& arr (next (Done n))
      where
        r _ Clear = 0.0
        r _ (Number x) = x -- overwrite Number
        r n (Operator _) = n -- keep Number
        
    operating n op = Automaton $ arr (r n op) &&& arr (next (Operating n op))
      where
        r _ _ Clear = 0.0
        r n op (Number x) = calc op n x
        r n _ (Operator _) = n -- keep Number
        
    
    next Init Clear = init
    next Init (Number x) = done x -- new Number
    next Init (Operator _) = init

    next (Done _) Clear = init
    next (Done _) (Number x) = done x -- overwrite Number
    next (Done n) (Operator op) = operating n op
    
    next (Operating _ _) Clear = init
    next (Operating n op) (Number x) = done $ calc op n x
    next (Operating n _) (Operator op) = operating n op -- overwrite Operation
    
    
runCalc :: Stream.Stream Input -> Stream.Stream Float
runCalc xs = (runAutomaton $ arr snd >>> calculator) ((),xs)

runCalcLog :: Stream.Stream Input -> Stream.Stream (Input ,Float)
runCalcLog xs = (runAutomaton $ arr snd >>> (returnA &&& calculator)) ((),xs)

             
test :: [Input] -> [Float]
test xs = Stream.toList $ runCalc $ Stream.fromList $ xs <> (repeat Clear)

testLog :: [Input] -> [(Input,Float)]
testLog xs = Stream.toList $ runCalcLog $ Stream.fromList $ xs <> (repeat Clear)


test1 = take 4 $ test [Number 100,Operator Add,Number 10]

testLog1 = take 4 $ testLog $ [Clear, Clear, Clear]
testLog2 = take 4 $ testLog $ [Clear, Clear, Number 100]
testLog3 = take 4 $ testLog $ [Clear, Clear, Operator Add]
testLog4 = take 4 $ testLog $ [Clear, Number 10, Clear]
testLog5 = take 4 $ testLog $ [Clear, Number 10, Number 100]
testLog6 = take 4 $ testLog $ [Clear, Number 10, Operator Add]
testLog7 = take 4 $ testLog $ [Clear, Operator Mul, Clear]
testLog8 = take 4 $ testLog $ [Clear, Operator Mul, Number 100]
testLog9 = take 4 $ testLog $ [Clear, Operator Mul, Operator Add]

testLog10 = take 4 $ testLog $ [Number 5, Clear, Clear]
testLog11 = take 4 $ testLog $ [Number 5, Clear, Number 100]
testLog12 = take 4 $ testLog $ [Number 5, Clear, Operator Add]
testLog13 = take 4 $ testLog $ [Number 5, Number 10, Clear]
testLog14 = take 4 $ testLog $ [Number 5, Number 10, Number 100]
testLog15 = take 4 $ testLog $ [Number 5, Number 10, Operator Add]
testLog16 = take 4 $ testLog $ [Number 5, Operator Mul, Clear]
testLog17 = take 4 $ testLog $ [Number 5, Operator Mul, Number 100]
testLog18 = take 4 $ testLog $ [Number 5, Operator Mul, Operator Add]

testLog19 = take 4 $ testLog $ [Operator Sub, Clear, Clear]
testLog20 = take 4 $ testLog $ [Operator Sub, Clear, Number 100]
testLog21 = take 4 $ testLog $ [Operator Sub, Clear, Operator Add]
testLog22 = take 4 $ testLog $ [Operator Sub, Number 10, Clear]
testLog23 = take 4 $ testLog $ [Operator Sub, Number 10, Number 100]
testLog24 = take 4 $ testLog $ [Operator Sub, Number 10, Operator Add]
testLog25 = take 4 $ testLog $ [Operator Sub, Operator Mul, Clear]
testLog26 = take 4 $ testLog $ [Operator Sub, Operator Mul, Number 100]
testLog27 = take 4 $ testLog $ [Operator Sub, Operator Mul, Operator Add]


testLog101 = take 10 $ testLog $ [Operator Add,Number 100,Operator Add,Number 10,Clear,Number 200,Operator Sub,Number 10,Number 300]

testLog102 = take 10 $ testLog $ [Operator Add,Number 100,Operator Add,Number 10,Clear,Number 200,Operator Sub,Number 10,Number 300]
testLog103 = take 10 $ testLog $ [Operator Add,Number 100,Operator Add,Number 10,Clear,Number 200,Operator Sub,Number 10,Number 300]






--main :: IO ()
--main = do
--  xs <- randomZDK
--  forM_ xs $ \x -> do
--    putStrLn x
--    threadDelay $ 500 * 1000

--main = putStrLn "main"
main = do
  print testLog1
  print testLog2
  print testLog3
  print testLog4
  print testLog5
  print testLog6
  print testLog7
  print testLog8
  print testLog9
  print testLog10
  print testLog11
  print testLog12
  print testLog13
  print testLog14
  print testLog15
  print testLog16
  print testLog17
  print testLog18
  print testLog19
  print testLog20
  print testLog21
  print testLog22
  print testLog23
  print testLog24
  print testLog25
  print testLog26
  print testLog27
