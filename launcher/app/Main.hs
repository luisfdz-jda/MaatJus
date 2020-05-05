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
import System.Directory (doesFileExist, removeFile)
import Control.Monad (when)

newtype Version = Version Int

instance Show Version where
  show (Version v) = show v

haxm :: FilePath -> FilePath -> IO ()
haxm tempPath appData = do
  putStrLn "Descargando e instalando haxm..."
  request  <- parseRequest $ "https://github.com/luisfdz-jda/MaatJus/releases/download/maat_win32_x86_64/haxm.tgz"
  download request (tempPath ++ "\\haxm.tgz")
  Tar.unpack tempPath . Tar.read . GZip.decompress =<< BS.readFile (tempPath ++ "\\haxm.tgz")
  shell (T.pack $ concat [tempPath, "\\haxm\\intelhaxm-android.exe -f ", appData, "\\haxm -a /qn MEMSIZETYPE=1 CUSTOMMEMSIZE=0 NOTCHECKVTENABLE=1"]) empty
  pure ()

qemu :: FilePath -> FilePath -> IO ()
qemu tempPath appData = do
  putStrLn "Descargando e instalando qemu..."
  request <- parseRequest $ "https://github.com/luisfdz-jda/MaatJus/releases/download/maat_win32_x86_64/qemu.tgz"
  download  request (tempPath ++ "\\qemu.tgz")
  Tar.unpack appData . Tar.read . GZip.decompress =<< BS.readFile (tempPath ++ "\\qemu.tgz")
  pure ()

slax :: FilePath -> IO ()
slax appData = do
  putStrLn "Descargando imagen iso de maat..."
  request <- parseRequest $ "https://github.com/luisfdz-jda/MaatJus/releases/download/maat_win32_x86_64/maat.iso"
  download request (appData ++ "\\maat.iso")
  pure ()

quemuLauncher :: FilePath -> IO ()
quemuLauncher appData = do
  putStrLn "Instalando qemu_launcher..."
  request <- parseRequest $ "https://github.com/luisfdz-jda/MaatJus/releases/download/maat_win32_x86_64/qemu_launcher.bat"
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
  putStrLn "Maat.Jus - Mecanismo andaluz de acceso al teletrabajo"
  putStrLn "Consejería de Turismo, Regeneración, Justicia y Administración Local"

setInstalledVersion :: Version -> IO ()
setInstalledVersion (Version v) = do
  appData <- env "LOCALAPPDATA"
  let installedVersionFilename = appData ++ "\\maat_installed_version.txt"
  writeFile installedVersionFilename (show v)

maat :: FilePath -> FilePath -> IO ()
maat appData desktopPath = do
  let maatExeFilename = appData ++ "\\maat.exe"
  fileExists <- doesFileExist maatExeFilename
  let maatExeFilename1 = if fileExists then appData ++ "\\maat1.exe" else maatExeFilename
  putStrLn $ "Instalando " ++ maatExeFilename1 ++ "..."
  download "https://github.com/luisfdz-jda/MaatJus/releases/download/maat_win32_x86_64/maat.exe" maatExeFilename1
  download "https://github.com/luisfdz-jda/MaatJus/releases/download/maat_win32_x86_64/maat.ico" (appData ++ "\\maat.ico")
  download "https://github.com/luisfdz-jda/MaatJus/releases/download/maat_win32_x86_64/maat.bat" (appData ++ "\\maat.bat")
  download "https://github.com/luisfdz-jda/MaatJus/releases/download/maat_win32_x86_64/Maat.Jus.lnk" (desktopPath ++ "\\Maat.Jus.lnk")
  pure ()

install :: Version -> IO ()
install v = do
  tempPath <- env "TEMP"
  appData <- env "LOCALAPPDATA"
  profilePath <- env "USERPROFILE"
  let desktopPath = profilePath ++ "\\Desktop"
  maat appData desktopPath
  haxm tempPath appData
  qemu tempPath appData
  slax appData
  quemuLauncher appData
  setInstalledVersion v

launch :: Version -> IO ()
launch v = do
  putStrLn $ "Lanzando Maat.Jus v" ++ show v ++ "..."
  appData <- fmap T.pack $ env "LOCALAPPDATA"
  shell (T.intercalate "" [ appData, "\\qemu\\qemu_launcher.bat "
                          , appData, "\\qemu\\qemu-system-x86_64w "
                          , appData, "\\maat.iso" ]
        )
        empty
  pure ()

onlineVersion :: IO Int
onlineVersion = do
  appData <- env "LOCALAPPDATA"
  let currentVersionFilename = appData ++ "\\maat_online_version.txt"
  runConduitRes $ httpSource "https://github.com/luisfdz-jda/MaatJus/releases/download/maat_win32_x86_64/version.txt" getResponseBody .| sinkFile currentVersionFilename
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

newVersion :: IO (Bool, Version, Version)
newVersion = do
  ov <- onlineVersion
  iv <- installedVersion
  let iv1 = fromMaybe (-1) iv
  pure $ (ov /= iv1, Version ov, Version iv1)

main :: IO ()
main = do
  initialMsg
  (newRelease, onlineVersion, installedVersion) <- newVersion
  if newRelease then do
    putStrLn $ "Descargando e instalando Maat.Jus v" ++ (show onlineVersion)
    putStrLn "Puede tardar varios minutos (no interrumpa el proceso). Por favor, espere..."
    install onlineVersion
    launch onlineVersion
  else do
    launch installedVersion
  pure ()

