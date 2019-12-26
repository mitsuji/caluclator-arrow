module Main where

import qualified Data.Stream as Stream
import OperandRegister
import Calculator

import Graphics.Gloss.Interface.IO.Interact
import Control.Concurrent.MVar
import System.IO.Unsafe (unsafeInterleaveIO)
import Data.Stream ((<:>))
import Control.Concurrent (forkIO)


main :: IO ()
main = do
  inputVar <- newEmptyMVar
  outputVar <- newEmptyMVar
  forkIO $ getStream inputVar >>= (putStream outputVar) . calculate 
  interactIO window white "0" onUpdate (onEvent inputVar outputVar) (\_ -> return())
  where
    window = InWindow "Calculator" (640, 200) (100, 100)

    -- [TODO] create number images
    onUpdate xs = return $ (translate (-310) 0 ) . (scale 0.4 0.4) $ text xs

    onEvent inputVar outputVar event xs =
      case toInput event of
        Nothing  -> return xs
        Just input -> do
          putMVar inputVar input
          (show . OperandRegister') <$> takeMVar outputVar


getStream :: MVar Input -> IO (Stream.Stream Input)
getStream var = f
  where
    f = unsafeInterleaveIO $ do
      x  <- takeMVar var
      xs <- f
      return $ x <:> xs

putStream :: MVar Output -> Stream.Stream Output -> IO ()
putStream var xs = f xs
  where
    f (Stream.Cons x xs') = do
      putMVar var x
      f xs'

toInput :: Event -> Maybe Input
toInput (EventKey (Char c) Up _ _) = f c
  where
    f :: Char -> Maybe Input
    f '0'  = Just $ Number N0
    f '1'  = Just $ Number N1
    f '2'  = Just $ Number N2
    f '3'  = Just $ Number N3
    f '4'  = Just $ Number N4
    f '5'  = Just $ Number N5
    f '6'  = Just $ Number N6
    f '7'  = Just $ Number N7
    f '8'  = Just $ Number N8
    f '9'  = Just $ Number N9
    f '.'  = Just $ Number Dot
    f 'd'  = Just $ Number BS
    f '\b' = Just $ Number BS
    f '+'  = Just $ Operator Add
    f '-'  = Just $ Operator Sub
    f '*'  = Just $ Operator Mul
    f '/'  = Just $ Operator Div
    f '='  = Just Equal
    f 'e'  = Just Equal
    f 'a'  = Just AllClear
    f 'c'  = Just Clear
    f _    = Nothing
toInput (EventKey (SpecialKey KeyEnter) Up _ _) = Just $ Equal
toInput (EventKey (SpecialKey KeyBackspace) Up _ _) = Just $ Number BS
toInput (EventKey (SpecialKey KeyDelete) Up _ _) = Just $ Number BS
toInput _ = Nothing
