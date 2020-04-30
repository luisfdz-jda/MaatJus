{-# LANGUAGE OverloadedStrings #-}

module Main where

import Turtle (shell, echo, empty, need, mv)
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
  shell "%TEMP%\\haxm.1\\haxm-windows_v7_6_1\\intelhaxm-android.exe -f %TEMP%\\intel\\HAXM\\7.6.1\\silent -a /qn MEMSIZETYPE=1 CUSTOMMEMSIZE=0 NOTCHECKVTENABLE=1" empty
  pure ()

installQemu :: Filepath -> Filepath -> IO ()
installQemu sourcePath destPath = do
  -- mv (sourcePath ++ "\\qemu.1") (destPath ++ "\\qemu.1") 
  mv sourcePath destPath 
  pure ()

untar :: Filepath -> IO ()
untar path = do
  -- Tar.extract (path ++ "\\haxm.1") (path ++ "\\haxm.1.tar.gz")
  Tar.unpack (path ++ "\\haxm.1") . Tar.read . GZip.decompress =<< BS.readFile (path ++ "\\haxm.1.tar.gz")
  Tar.unpack (path ++ "\\qemu.1") . Tar.read . GZip.decompress =<< BS.readFile (path ++ "\\qemu.1.tar.gz")
  pure ()

downloads :: Filepath -> IO ()
downloads path = do
  download "https://github.com/luisfdz-jda/arca/releases/download/arca1/haxm.1.tar.gz" $ path ++ "\\haxm.1.tar.gz"
  download "https://github.com/luisfdz-jda/arca/releases/download/arca1/qemu.1.tar.gz" $ path ++ "\\qemu.1.tar.gz"
  -- download "https://github.com/luisfdz-jda/arca/releases/download/arca1/slax.1.tar.gz" $ path ++ "\\slax.1.tar.gz"

download :: Request -> Filepath -> IO ()
download source destination = do
  runConduitRes $ httpSource source getResponseBody .| sinkFile destination

env :: String -> IO String
env variable = do
  tempPath <- need "TMP"
  pure $ fromMaybe "." (fmap T.unpack tempPath)

main :: IO ()
main = do
  tempPath <- env "TMP"
  appPath <- env "LOCALAPPDATA"
  putStrLn "Descargando release..."
  downloads tempPath
  putStrLn "Descomprimiendo..."
  untar tempPath
  putStrLn "Instalando..."
  installHax
  installQemu tempPath appPath
