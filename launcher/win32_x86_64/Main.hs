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
import System.Clock

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

profilingDownload :: Request -> FilePath -> IO ()
profilingDownload source destination = do
  t1 <- liftIO $  getTime Monotonic
  download source destination
  t2 <- liftIO $  getTime Monotonic
  download source destination
  t3 <- liftIO $  getTime Monotonic
  download source destination
  t4 <- liftIO $  getTime Monotonic
  download source destination
  t5 <- liftIO $  getTime Monotonic
  download source destination
  t6 <- liftIO $  getTime Monotonic
  download source destination
  t7 <- liftIO $  getTime Monotonic
  download source destination
  t8 <- liftIO $  getTime Monotonic
  download source destination
  t9 <- liftIO $  getTime Monotonic
  download source destination
  t10 <- liftIO $  getTime Monotonic
  download source destination
  t11 <- liftIO $  getTime Monotonic

  let tt1 = seconds t1 t2
  let tt2 = seconds t2 t3
  let tt3 = seconds t3 t4
  let tt4 = seconds t4 t5
  let tt5 = seconds t5 t6
  let tt6 = seconds t6 t7
  let tt7 = seconds t7 t8
  let tt8 = seconds t8 t9
  let tt9 = seconds t9 t10
  let tt10 = seconds t10 t11
  let avg = (tt1 + tt2 + tt3 + tt4 + tt5 + tt6 + tt7 + tt8 + tt9 + tt10) / 10.0
  let std = ((tt1-avg)**2.0 + (tt2-avg)**2.0 + (tt3-avg)**2.0 + (tt4-avg)**2.0 + (tt5-avg)**2.0 + (tt6-avg)**2.0 + (tt7-avg)**2.0 + (tt8-avg)**2.0 + (tt9-avg)**2.0 + (tt10-avg)**2.0) / 10.0

  putStrLn $ "avg: " ++ show avg ++ " std: " ++ show std ++ " " ++ destination

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

seconds :: TimeSpec -> TimeSpec -> Double
seconds (TimeSpec s1 n1) (TimeSpec s2 n2) =
  (fromIntegral $ s2 - s1) + ((fromIntegral $n2 - n1) / 1e9)

profiling :: IO ()
profiling = do
  tempPath <- env "TEMP"
  
  putStrLn "Consigna profiling, media y desviación típica en segundos de 10 descargas por archivo"
  profilingDownload "https://consigna.juntadeandalucia.es/90b5f6c34f79a86e6d97d0e37434b9f6/descarga" (tempPath ++ "\\haxm.tgz.consigna_profiling")
  profilingDownload "https://consigna.juntadeandalucia.es/17bdf94cb36d9e2aa83647fabdc9617a/descarga" (tempPath ++ "\\qemu.tgz.consigna_profiling")
  profilingDownload "https://consigna.juntadeandalucia.es/222d6dfe7597db407a2821fabb6c4c6c/descarga" (tempPath ++ "\\maat.iso.consigna_profiling")
  profilingDownload "https://consigna.juntadeandalucia.es/75ae284e091d581322c64e13b985d24c/descarga" (tempPath ++ "\\qemu_launcher.bat.consigna_profiling")
  profilingDownload "https://consigna.juntadeandalucia.es/6df748c9190c9878c28f958b49e4c46c/descarga" (tempPath ++ "maat.exe.consigna_profiling")
  profilingDownload "https://consigna.juntadeandalucia.es/b2e952c10551a171a0234b97b214b9e0/descarga" (tempPath ++ "\\maat.ico.consigna_profiling")
  profilingDownload "https://consigna.juntadeandalucia.es/e71c5a611adbdf2f3125d53f52a76024/descarga" (tempPath ++ "\\maat.bat.consigna_profiling")
  profilingDownload "https://consigna.juntadeandalucia.es/6c5f8238d0d16e6e528c9b45f1b2ee1b/descarga" (tempPath ++ "\\Maat.Jus.lnk.consigna_profiling")

  putStrLn ""
  putStrLn "GitHub profiling, media y desviación típica en segundos de 10 descargas por archivo"
  profilingDownload "https://github.com/luisfdz-jda/MaatJus/releases/download/maat_win32_x86_64/haxm.tgz" (tempPath ++ "\\haxm.tgz.github_profiling")
  profilingDownload "https://github.com/luisfdz-jda/MaatJus/releases/download/maat_win32_x86_64/qemu.tgz" (tempPath ++ "\\qemu.tgz.github_profiling")
  profilingDownload "https://github.com/luisfdz-jda/MaatJus/releases/download/maat_win32_x86_64/maat.iso" (tempPath ++ "\\maat.iso.github_profiling")
  profilingDownload "https://github.com/luisfdz-jda/MaatJus/releases/download/maat_win32_x86_64/qemu_launcher.bat" (tempPath ++ "\\qemu_launcher.bat.github_profiling")
  profilingDownload "https://github.com/luisfdz-jda/MaatJus/releases/download/maat_win32_x86_64/maat.exe" (tempPath ++ "maat.exe.github_profiling")
  profilingDownload "https://github.com/luisfdz-jda/MaatJus/releases/download/maat_win32_x86_64/maat.ico" (tempPath ++ "\\maat.ico.github_profiling")
  profilingDownload "https://github.com/luisfdz-jda/MaatJus/releases/download/maat_win32_x86_64/maat.bat" (tempPath ++ "\\maat.bat.github_profiling")
  profilingDownload "https://github.com/luisfdz-jda/MaatJus/releases/download/maat_win32_x86_64/Maat.Jus.lnk" (tempPath ++ "\\Maat.Jus.lnk.github_profiling")

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

