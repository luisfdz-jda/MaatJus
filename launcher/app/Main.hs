{-# LANGUAGE OverloadedStrings #-}

module Main where

import Turtle (shell, echo, empty, need, mv, mktree, decodeString)
import qualified Codec.Archive.Tar as Tar
import Network.HTTP.Simple (httpSource, getResponseBody)
import Network.HTTP.Client (Request, parseRequest)
import Conduit
import qualified Data.Text as T
import Data.Maybe (fromMaybe)
import qualified Codec.Compression.GZip as GZip
import qualified Data.ByteString.Lazy as BS
import System.IO (readFile, writeFile)
import System.Directory (doesFileExist)

newtype Version = Version Int

instance Show Version where
  show (Version v) = show v

haxm :: FilePath -> FilePath -> IO ()
haxm tempPath appData = do
  putStrLn "Descargando e instalando haxm..."
  request  <- parseRequest $ "https://github.com/luisfdz-jda/MaatJus/releases/download/Maat_win32_x86_64_/haxm.tgz"
  download request (tempPath ++ "\\haxm.tgz")
  Tar.unpack tempPath . Tar.read . GZip.decompress =<< BS.readFile (tempPath ++ "\\haxm.tgz")
  shell (T.pack $ concat [tempPath, "\\haxm\\intelhaxm-android.exe -f ", appData, "\\haxm -a /qn MEMSIZETYPE=1 CUSTOMMEMSIZE=0 NOTCHECKVTENABLE=1"]) empty
  pure ()

qemu :: FilePath -> FilePath -> IO ()
qemu tempPath appData = do
  putStrLn "Descargando e instalando qemu..."
  request <- parseRequest $ "https://github.com/luisfdz-jda/MaatJus/releases/download/Maat_win32_x86_64_/qemu.tgz"
  download  request (tempPath ++ "\\qemu.tgz")
  Tar.unpack appData . Tar.read . GZip.decompress =<< BS.readFile (tempPath ++ "\\qemu.tgz")
  pure ()

slax :: FilePath -> IO ()
slax appData = do
  putStrLn "Descargando slax..."
  request <- parseRequest $ "https://github.com/luisfdz-jda/MaatJus/releases/download/Maat_win32_x86_64_/slax.iso"
  download request (appData ++ "\\slax.iso")
  pure ()

quemuLauncher :: FilePath -> IO ()
quemuLauncher appData = do
  putStrLn "Instalando qemu_launcher..."
  request <- parseRequest $ "https://github.com/luisfdz-jda/MaatJus/releases/download/Maat_win32_x86_64_/qemu_launcher.bat"
  download request (appData ++ "\\qemu\\qemu_launcher.bat")
  pure ()


-- https://stackoverflow.com/questions/40836795/haskell-streaming-download
download :: Request -> FilePath -> IO ()
download source destination = do
  runConduitRes $ httpSource source getResponseBody .| sinkFile destination

env :: T.Text -> IO String
env variable = do
  tempPath <- need variable
  pure $ fromMaybe "." (fmap T.unpack tempPath)

initialMsg :: IO ()
initialMsg = do
  putStrLn "Maat Jus - Mecanismo andaluz de acceso al teletrabajo"
  putStrLn "Consejería de Turismo, Regeneración, Justicia y Administración Local"

setInstalledVersion :: Version -> IO ()
setInstalledVersion (Version v) = do
  appData <- env "LOCALAPPDATA"
  let installedVersionFilename = appData ++ "\\maat_installed_version.txt"
  writeFile installedVersionFilename (show v)

install :: Version -> IO ()
install v = do
  tempPath <- env "TEMP"
  appData <- env "LOCALAPPDATA"
  haxm tempPath appData
  qemu tempPath appData
  slax appData
  quemuLauncher appData
  setInstalledVersion v

launch :: IO ()
launch = do
  appData <- fmap T.pack $ env "LOCALAPPDATA"
  shell (T.intercalate "" [ appData, "\\qemu\\qemu_launcher.bat "
                          , appData, "\\qemu\\qemu-system-x86_64w "
                          , appData, "\\slax.iso" ]
        )
        empty
  pure ()

currentVersion :: IO Int
currentVersion = do
  appData <- env "LOCALAPPDATA"
  let currentVersionFilename = appData ++ "\\maat_current_version.txt"
  runConduitRes $ httpSource "https://github.com/luisfdz-jda/MaatJus/releases/download/Maat_win32_x86_64/version.txt" getResponseBody .| sinkFile currentVersionFilename
  content <- readFile currentVersionFilename
  pure $ (read content :: Int)

installedVersion :: IO (Maybe Int)
installedVersion = do
  appData <- env "LOCALAPPDATA"
  let installedVersionFilename = appData ++ "\\maat_installed_version.txt"
  e <- doesFileExist installedVersionFilename
  if e then do
    content <- readFile installedVersionFilename
    pure $ Just $ (read content :: Int)
  else
    pure Nothing

newVersion :: IO (Bool, Version)
newVersion = do
  cv <- currentVersion
  iv <- installedVersion
  let iv1 = fromMaybe (-1) iv
  pure $ (cv /= iv1, Version cv)

main :: IO ()
main = do
  initialMsg
  (nv, cv) <- newVersion
  if nv then do
    putStrLn $ "Descargando e instalando release #" ++ (show cv)
    putStrLn "Puede tardar varios minutos (no interrumpa el proceso). Por favor, espere..."
    install cv
    launch
  else do
    launch
  pure ()

