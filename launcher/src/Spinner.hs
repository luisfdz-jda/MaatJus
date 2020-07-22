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

spinnerStep :: Char -> IO ()
spinnerStep c = do
  putChar c
  hFlush stdout
  pause 200000
  cursorBackward 1

spinner' :: IO ()
spinner' = do
  mapM_ spinnerStep $ ['|', '/', '-', '\\']
