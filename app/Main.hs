module Main where

import qualified Data.Stream as Stream
import Control.Arrow
import Control.Arrow.Transformer.Stream(StreamArrow(..),runStream)
import OperandRegister
import Calculator
import Data.Maybe (isJust,fromJust)
import System.IO(stdin,stdout,BufferMode(..),hSetEcho,hSetBuffering,hGetContents,hFlush)
import Control.Monad(forM_)


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
  forM_ xs $ \x -> putStr "\r " >> putStr x  >> hFlush stdout

main2 :: IO ()
main2 = do
  hSetBuffering stdin NoBuffering
  hSetEcho stdin False
  xs <- Stream.toList . fromOutputLog . calculateLog . toInput . Stream.fromList <$> hGetContents stdin
  forM_ xs print
