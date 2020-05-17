module Spinner (spinner) where

import System.Console.ANSI
import System.IO
import Control.Monad.Loops
import Control.Concurrent

spinner :: IO ()
spinner = iterateM_ (\_ -> spinner') ()

pause :: Int -> IO ()
pause n = do
    hFlush stdout
    threadDelay n

spinnerStep :: Int -> IO ()
spinnerStep n = do
  setSGR [SetColor Foreground Vivid Green]
  putStr "["
  putStr $ replicate n '-'
  setSGR [SetColor Foreground Vivid Red]
  putStr "o"
  setSGR [SetColor Foreground Vivid Green]
  putStr $ replicate (9-n) '-'
  putStr "]"
  hFlush stdout
  pause 200000
  cursorBackward 12

spinner' :: IO ()
spinner' = do
  mapM_ spinnerStep $ [0..9] ++ (reverse [1..8])
