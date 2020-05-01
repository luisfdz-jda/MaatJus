{-# LANGUAGE OverloadedStrings #-}

module Main where

import Turtle (shell, echo, empty, need, mv, mktree, decodeString)
import qualified Codec.Archive.Tar as Tar
import Network.HTTP.Simple (httpSource, getResponseBody)
import Network.HTTP.Client (Request)
import Conduit
import qualified Data.Text as T
import Data.Maybe (fromMaybe)
import qualified Codec.Compression.GZip as GZip
import qualified Data.ByteString.Lazy as BS
import System.IO (readFile, writeFile)
import System.Directory (doesFileExist)

newtype Version = Version Int

haxm :: Version -> FilePath -> FilePath -> IO ()
haxm (Version v) tempPath appPath = do
  download ("https://github.com/luisfdz-jda/MaatJus/releases/download/Maat_win32_x86_64_" ++ show v ++ "/haxm.tgz") $ tempPath ++ "\\haxm.tgz"
  Tar.unpack tempPath . Tar.read . GZip.decompress =<< BS.readFile (tempPath ++ "\\haxm.tgz")
  shell (T.pack $ concat [tempPath, "\\haxm\\intelhaxm-android.exe -f ", appPath, "\\haxm -a /qn MEMSIZETYPE=1 CUSTOMMEMSIZE=0 NOTCHECKVTENABLE=1"]) empty
  pure ()

qemu :: Version -> FilePath -> FilePath -> IO ()
qemu (Version v) tempPath appPath = do
  download ("https://github.com/luisfdz-jda/MaatJus/releases/download/Maat_win32_x86_64_" ++ show v ++ "/qemu.tgz") $ tempPath ++ "\\qemu.tgz"
  Tar.unpack appPath . Tar.read . GZip.decompress =<< BS.readFile (tempPath ++ "\\qemu.tgz")
  pure ()

slax :: Version -> FilePath -> IO ()
slax (Version v) appData = do
  download ("https://github.com/luisfdz-jda/MaatJus/releases/download/Maat_win32_x86_64_" ++ show v ++ "/slax.iso") $ appData ++ "\\slax.iso"
  pure ()

quemuLauncher :: Version -> FilePath -> IO ()
quemuLauncher (Version v) appPath = do
  download ("https://github.com/luisfdz-jda/MaatJus/releases/download/Maat_win32_x86_64_" ++ show v ++ "/qemu_launcher.bat") $ appPath ++ "\\qemu\\qemu_launcher.bat "
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
  putStrLn "Maat Jus - Mecanismo Andaluz de Acceso al Teletrabajo"
  putStrLn "Consejería de Turismo, Regeneración, Justicia y Administración Local"

setInstalledVersion :: Version -> IO ()
setInstalledVersion (Version v) = do
  appData <- env "APPDATA"
  let installedVersionFilename = appData ++ "\\maat_installed_version.txt"
  writeFile installedVersionFilename (show v)

install :: Version -> IO ()
install v = do
  tempPath <- env "TEMP"
  appPath <- env "LOCALAPPDATA"
  appData <- env "APPDATA"
  haxm v tempPath appPath
  qemu v tempPath appPath
  slax v appData
  quemuLauncher v appPath
  setInstalledVersion v

launch :: IO ()
launch = do
  appPath <- fmap T.pack $ env "LOCALAPPDATA"
  appData <- fmap T.pack $ env "APPDATA"
  shell (T.intercalate "" [ appPath, "\\qemu\\qemu_launcher.bat "
                          , appPath, "\\qemu\\qemu-system-x86_64w "
                          , appData, "\\slax.iso" ]
        )
        empty
  pure ()

currentVersion :: IO Int
currentVersion = do
  appData <- env "APPDATA"
  let currentVersionFilename = appData ++ "\\maat_current_version.txt"
  runConduitRes $ httpSource "https://raw.githubusercontent.com/luisfdz-jda/MaatJus/master/version.txt" getResponseBody .| sinkFile currentVersionFilename
  content <- readFile currentVersionFilename
  pure $ (read content :: Int)

installedVersion :: IO (Maybe Int)
installedVersion = do
  appData <- env "APPDATA"
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
  (nr, cv) <- newVersion
  if nr then do
    putStrLn $ "Descargando e instalando release # " ++ (show cv)
    putStrLn "Puede tardar varios minutos (no interrumpa el proceso). Por favor, espere..."
    install cv
    launch
  else do
    launch
  pure ()

