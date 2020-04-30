{-# LANGUAGE OverloadedStrings #-}

module Main where

import Turtle (shell, echo, empty, need)
import qualified Codec.Archive.Tar as Tar
import Network.HTTP.Simple (httpSource, getResponseBody)
import Network.HTTP.Client (Request)
import Conduit
import qualified Data.Text as T
import Data.Maybe (fromMaybe)
import qualified Codec.Compression.GZip as GZip
import qualified Data.ByteString.Lazy as BS

type Filename = String
type Filepath = String

installHax :: IO ()
installHax = do
  shell "intelhaxm-android.exe -f %TEMP%\\intel\\HAXM\\7.6.1\\silent -a /qn MEMSIZETYPE=1 CUSTOMMEMSIZE=0 NOTCHECKVTENABLE=1" empty
  pure ()

installQemu :: IO ()
installQemu = do
  pure ()

untar :: Filepath -> IO ()
untar path = do
  -- Tar.extract (path ++ "\\haxm.1") (path ++ "\\haxm.1.tar.gz")
  Tar.unpack (path ++ "\\haxm.1") . Tar.read . GZip.decompress =<< BS.readFile (path ++ "\\haxm.1.tar.gz")
  pure ()

downloads :: Filepath -> IO ()
downloads path = do
  download "https://github.com/luisfdz-jda/arca/releases/download/arca1/haxm.1.tar.gz" $ path ++ "\\haxm.1.tar.gz"
  -- download "https://github.com/luisfdz-jda/arca/releases/download/arca1/qemu.1.tar.gz" $ tmpPath ++ "\\qemu.1.tar.gz"
  -- download "https://github.com/luisfdz-jda/arca/releases/download/arca1/slax.1.tar.gz" $ tmpPath ++ "\\slax.1.tar.gz"

download :: Request -> Filepath -> IO ()
download source destination = do
  runConduitRes $ httpSource source getResponseBody .| sinkFile destination

tmpPath :: IO String
tmpPath = do
  tempPath <- need "TMP"
  pure $ fromMaybe "." (fmap T.unpack tempPath)

main :: IO ()
main = do
  putStrLn "Descargando release..."
  path <- tmpPath
  downloads path
  putStrLn "Descomprimiendo..."
  untar path
  -- installHax
  -- installQemu

