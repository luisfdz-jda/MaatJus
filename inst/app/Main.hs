{-# LANGUAGE OverloadedStrings #-}

module Main where

import Turtle (shell, echo, empty)
import qualified Codec.Archive.Tar as Tar
import Network.HTTP.Simple (httpSource, getResponseBody)
import Conduit

type Filename = Text

installHax :: IO ()
installHax = do
  shell "intelhaxm-android.exe -f %TEMP%\\intel\\HAXM\\7.6.1\\silent -a /qn MEMSIZETYPE=1 CUSTOMMEMSIZE=0 NOTCHECKVTENABLE=1" empty
  pure ()

installQemu :: IO ()
installQemu = do
  pure ()

untar :: IO ()
untar = do
  Tar.extract "C:\\tmp\\foo1" "C:\\tmp\\acceso_rcja.tar"
  pure ()

downloads :: IO ()
  download "haxm1.tar.gz" "."

download :: Filename -> Filepath -> IO ()
download f p = do
  let url = "https://github.com/luisfdz-jda/arca/releases/download/arca1/" ++ f
  runConduitRes $ httpSource url getResponseBody .| sinkFile $ p ++ "/" ++ f

main :: IO ()
main = do
  downloads
  -- untar
  -- installHax
  -- installQemu

