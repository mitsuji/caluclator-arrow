module Main where

import qualified Data.Stream as Stream
import Control.Arrow
import Control.Arrow.Transformer.Stream(StreamArrow(..),runStream)
import OperandRegister
import Calculator
import Data.Maybe (isJust,fromJust)
import System.IO(stdin,stdout,BufferMode(..),hSetEcho,hSetBuffering)
import System.IO (hGetChar,hPutStrLn)
import Control.Monad(forM_)

import Control.Concurrent.MVar
import Control.Concurrent.Chan
import System.IO.Unsafe (unsafeInterleaveIO)
import Data.Stream ((<:>))
import Control.Concurrent (forkIO)
import Control.Monad (forever)
import Graphics.Gloss.Interface.IO.Interact


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
--main = main_cui
main = main_gui


getInStream :: Chan Char -> IO (Stream.Stream Char)
getInStream chan = f
  where
    f = unsafeInterleaveIO $ do
      c <- readChan chan
      cs <- f
      return $ c <:> cs

setOutStream :: MVar String -> Stream.Stream String -> IO ()
setOutStream ref str = f str
  where
    f (Stream.Cons xs str') = do
      putMVar ref xs
      f str'


main_cui :: IO ()
main_cui = do
  hSetBuffering stdin NoBuffering
  hSetEcho stdin False
  inChan <- newChan
  outVar <- newEmptyMVar
  forkIO $ getInStream inChan >>= (setOutStream outVar) . fromOutput . calculate . toInput
  forever $ do
    hGetChar stdin >>= \c -> writeChan inChan c
    takeMVar outVar >>= \xs -> hPutStrLn stdout xs


window :: Display
window = InWindow "Caluculator" (640, 480) (100, 100)

main_gui :: IO ()
main_gui = do
  inChan <- newChan
  outVar <- newEmptyMVar
  forkIO $ getInStream inChan >>= (setOutStream outVar) . fromOutput . calculate . toInput
  interactIO window white "0" update (event inChan outVar) (\_ -> return())
  where
    update :: String -> IO Picture
    update xs = do
      return (translate (-150) (-10) . scale 0.5 0.5 $ text xs)

    event :: (Chan Char) -> (MVar String) -> Event -> String -> IO String
    event inChan outVar (EventKey (Char c) Up _ _) xs = do
      writeChan inChan c
      nx <- tryTakeMVar outVar
      case nx of
        Nothing -> return xs
        Just x  -> return x
    event _ _ _ xs = return xs

