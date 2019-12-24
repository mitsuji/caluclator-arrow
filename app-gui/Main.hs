module Main where

import qualified Data.Stream as Stream
import OperandRegister
import Calculator

import Graphics.Gloss.Interface.IO.Interact

import Control.Concurrent.MVar
import Control.Concurrent.Chan
import System.IO.Unsafe (unsafeInterleaveIO)
import Data.Stream ((<:>))
import Control.Concurrent (forkIO)


main :: IO ()
main = main_gui

getInStream :: Chan Input -> IO (Stream.Stream Input)
getInStream chan = f
  where
    f = unsafeInterleaveIO $ do
      c <- readChan chan
      cs <- f
      return $ c <:> cs

setOutStream :: MVar Output -> Stream.Stream Output -> IO ()
setOutStream ref str = f str
  where
    f (Stream.Cons xs str') = do
      putMVar ref xs
      f str'

window :: Display
window = InWindow "Calculator" (640, 480) (100, 100)

main_gui :: IO ()
main_gui = do
  inChan <- newChan
  outVar <- newEmptyMVar
  forkIO $ getInStream inChan >>= (setOutStream outVar) . calculate 
  interactIO window white "0" update (event inChan outVar) (\_ -> return())
  where
    update :: String -> IO Picture
    update xs = do
      return (translate (-150) (-10) . scale 0.5 0.5 $ text xs)

    event :: (Chan Input) -> (MVar Output) -> Event -> String -> IO String
    event inChan outVar ev xs = do
      case eventToInput ev of
        Nothing  -> return xs
        Just inp -> writeChan inChan inp >> ((show . OperandRegister') <$> takeMVar outVar)


eventToInput :: Event -> Maybe Input
eventToInput (EventKey (Char c) Up _ _) = f c
  where
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

eventToInput _ = Nothing
