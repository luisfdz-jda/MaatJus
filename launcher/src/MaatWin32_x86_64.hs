{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}

module MaatWin32_x86_64 (install, env, download) where 

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
import Data.Aeson (ToJSON, FromJSON, parseJSON, withObject, (.:), decode, encode)
import Data.Aeson.Types as A
import Data.Bits.Utils (s2w8, w82s)
import Data.List (intercalate)
import System.Clock
import System.Console.ANSI
import System.IO
import Numeric
import Control.Concurrent

import Spinner (spinner)

data Version = Version {
  haxm_tgz :: Int,
  maat_bat :: Int,
  maat_exe :: Int,
  maat_ico :: Int,
  maat_iso :: Int,
  maat_Jus_lnk :: Int,
  qemu_tgz :: Int,
  qemu_launcher_bat :: Int,
  uninstall_maat_bat :: Int
} deriving (Eq)

instance Show Version where
  show v = "(" ++ intercalate ", " [ "haxm.tgz = " ++ (show $ haxm_tgz v)
                                   , "maat.bat = " ++ (show $ maat_bat v)
                                   , "maat.exe = " ++ (show $ maat_exe v)
                                   , "maat.ico = " ++ (show $ maat_ico v)
                                   , "maat.iso = " ++ (show $ maat_iso v)
                                   , "Maat.Jus.lnk = " ++ (show $ maat_Jus_lnk v)
                                   , "qemu.tgz = " ++ (show $ qemu_tgz v)
                                   , "quemu_launcher.bat = " ++ (show $ qemu_launcher_bat v)
                                   , "uninstall_maat.bat = " ++ (show $ uninstall_maat_bat v)
                                   ] ++ ")"

instance FromJSON Version where
  parseJSON = withObject "Version" $ \o -> do
    haxm_tgz <- o .: "haxm.tgz" :: A.Parser Int
    maat_bat <- o .: "maat.bat" :: A.Parser Int
    maat_exe <- o .: "maat.exe" :: A.Parser Int
    maat_ico <- o .: "maat.ico" :: A.Parser Int
    maat_iso <- o .: "maat.iso" :: A.Parser Int
    maat_Jus_lnk <- o .: "Maat.Jus.lnk" :: A.Parser Int
    qemu_tgz <- o .: "qemu.tgz" :: A.Parser Int
    qemu_launcher_bat <- o .: "qemu_launcher.bat" :: A.Parser Int
    uninstall_maat_bat <- o .: "uninstall_maat.bat" :: A.Parser Int
    return $ Version{..}

instance ToJSON Version where
  toJSON v = object [
      "haxm.tgz" .= haxm_tgz v
    , "maat.bat" .= maat_bat v
    , "maat.exe" .= maat_exe v
    , "maat.ico" .= maat_ico v
    , "maat.iso" .= maat_iso v
    , "Maat.Jus.lnk" .= maat_Jus_lnk v
    , "qemu.tgz" .= qemu_tgz v
    , "qemu_launcher.bat" .= qemu_launcher_bat v
    , "uninstall_maat.bat" .= uninstall_maat_bat v
    ]

seconds :: TimeSpec -> TimeSpec -> Double
seconds (TimeSpec s1 n1) (TimeSpec s2 n2) =
  (fromIntegral $ s2 - s1) + ((fromIntegral $n2 - n1) / 1e9)

formatFloatN :: Double -> Int -> String
formatFloatN floatNum numOfDecimals = showFFloat (Just numOfDecimals) floatNum ""

conditionalInstall :: (Version -> FilePath -> FilePath -> FilePath -> IO ()) ->
                      (Version -> Int) ->
                      String ->
                      Maybe Version ->
                      Maybe Version ->
                      FilePath ->
                      FilePath ->
                      FilePath ->
                      IO ()
conditionalInstall _ _ _ Nothing _ _ _ _ =
  pure ()

conditionalInstall f vf appName (Just ov) Nothing tempPath appData desktopPath =
  conditionalInstall' f ov vf appName tempPath appData desktopPath

conditionalInstall f vf appName (Just ov) (Just iv) tempPath appData desktopPath
  | (vf ov) == (vf iv) = pure ()
  | otherwise = conditionalInstall' f ov vf appName tempPath appData desktopPath

conditionalInstall' :: (Version -> FilePath -> FilePath -> FilePath -> IO ()) ->
                       Version ->
                       (Version -> Int) ->
                       String ->
                       FilePath ->
                       FilePath ->
                       FilePath ->
                       IO ()
conditionalInstall' f ov vf appName tempPath appData desktopPath = do
  t1 <- liftIO $  getTime Monotonic
  putStr $ "Instalando " ++ appName ++ " v" ++ (show $ vf ov) ++ "... por favor espere (no cierre esta ventana)"
  hFlush stdout
  -- th <- forkIO spinner
  f ov tempPath appData desktopPath
  t2 <- liftIO $  getTime Monotonic
  -- killThread th
  setSGR [Reset]
  -- cursorBackward 54
  -- cursorBackward 54
  clearFromCursorToLineEnd
  putStrLn $ ", hecho (" ++ (formatFloatN (seconds t1 t2) 2) ++ "\")"

install_haxm :: Version -> FilePath -> FilePath -> FilePath -> IO ()
install_haxm ov tempPath _ _ = do
  request  <- parseRequest $ "https://github.com/luisfdz-jda/MaatJus/releases/download/maat_win32_x86_64/haxm.tgz"
  download request (tempPath ++ "\\haxm.tgz")
  Tar.unpack tempPath . Tar.read . GZip.decompress =<< BS.readFile (tempPath ++ "\\haxm.tgz")
  shell (T.pack $ concat [tempPath, "\\haxm\\silent_install_for_maat.bat -log %temp%\\haxm.log -ld %temp%\\haxm.debug.log"]) empty
  pure ()

install_qemu :: Version -> FilePath -> FilePath -> FilePath -> IO ()
install_qemu ov tempPath appData _ = do
  request <- parseRequest $ "https://github.com/luisfdz-jda/MaatJus/releases/download/maat_win32_x86_64/qemu.tgz"
  download  request (tempPath ++ "\\qemu.tgz")
  Tar.unpack appData . Tar.read . GZip.decompress =<< BS.readFile (tempPath ++ "\\qemu.tgz")
  pure ()

install_maat_iso :: Version -> FilePath -> FilePath -> FilePath -> IO ()
install_maat_iso ov _ appData _ = do
  request <- parseRequest $ "https://github.com/luisfdz-jda/MaatJus/releases/download/maat_win32_x86_64/maat.iso"
  download request (appData ++ "\\maat.iso")
  pure ()

install_qemu_launcher :: Version -> FilePath -> FilePath -> FilePath -> IO ()
install_qemu_launcher ov _ appData _ = do
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

initialMsg :: Maybe Version -> Maybe Version -> IO ()
initialMsg ov iv = do
  setSGR [Reset]
  setTitle "Maat.Jus - Mecanismo Andaluz de Acceso al Teletrabajo"
  setSGR [SetColor Foreground Vivid Green]
  putStrLn "Maat.Jus - Mecanismo Andaluz de Acceso al Teletrabajo"
  putStrLn "Consejería de Turismo, Regeneración, Justicia y Administración Local"
  putStrLn "Junta de Andalucía"
  setSGR [Reset]
  putStrLn "-"
  setSGR [SetColor Foreground Dull Yellow]
  putStrLn "(Build 202005171800)"
  setSGR [Reset]
  displayVersions ov iv

displayVersions :: Maybe Version -> Maybe Version -> IO ()
displayVersions Nothing _ = putStrLn "Especificación incorrecta de la versión en repositorio, contacte con el CAU e indique este error"
displayVersions (Just ov) Nothing = do
  putStrLn $ "Maat no está instalado en este equipo"
  putStrLn $ "Versión disponible online " ++ show ov
displayVersions (Just ov) (Just iv)
  | ov == iv = putStrLn $ "Versión instalada y al día " ++ show iv
  | otherwise = do
      putStrLn $ "Versión online " ++ show ov
      putStrLn $ "Versión instalada " ++ show iv

updateInstalledVersion :: Maybe Version -> IO ()
updateInstalledVersion Nothing = pure ()
updateInstalledVersion (Just v) = do
  appData <- env "LOCALAPPDATA"
  let installedVersionFilename = appData ++ "\\maat_installed_version.json"
  let jsonifiedVersion = w82s $ BS.unpack $ encode v
  writeFile installedVersionFilename jsonifiedVersion

install_maat_exe :: Version -> FilePath -> FilePath -> FilePath -> IO ()
install_maat_exe ov _ appData _ = do
  let maatExeFilename = appData ++ "\\maat.exe"
  fileExists <- doesFileExist maatExeFilename
  let maatExeFilename1 = if fileExists then appData ++ "\\maat1.exe" else maatExeFilename
  download "https://github.com/luisfdz-jda/MaatJus/releases/download/maat_win32_x86_64/maat.exe" maatExeFilename1
  pure ()

install_maat_ico :: Version -> FilePath -> FilePath -> FilePath -> IO ()
install_maat_ico ov _ appData _ = do
  download "https://github.com/luisfdz-jda/MaatJus/releases/download/maat_win32_x86_64/maat.ico" (appData ++ "\\maat.ico")
  pure ()

install_uninstall_maat_bat :: Version -> FilePath -> FilePath -> FilePath -> IO ()
install_uninstall_maat_bat ov _ appData _ = do
  download "https://github.com/luisfdz-jda/MaatJus/releases/download/maat_win32_x86_64/uninstall_maat.bat" (appData ++ "\\uninstall_maat.bat")
  pure ()

install_maat_bat :: Version -> FilePath -> FilePath -> FilePath -> IO ()
install_maat_bat ov _ appData _ = do
  download "https://github.com/luisfdz-jda/MaatJus/releases/download/maat_win32_x86_64/maat.bat" (appData ++ "\\maat.bat")
  pure ()

install_maat_jus_lnk :: Version -> FilePath -> FilePath -> FilePath -> IO ()
install_maat_jus_lnk ov _ _ desktopPath = do
  download "https://github.com/luisfdz-jda/MaatJus/releases/download/maat_win32_x86_64/Maat.Jus.lnk" (desktopPath ++ "\\Maat.Jus.lnk")
  pure ()

launch :: IO ()
launch = do
  appData <- fmap T.pack $ env "LOCALAPPDATA"
  shell (T.intercalate "" [ appData, "\\qemu\\qemu_launcher.bat "
                          , appData, "\\qemu\\qemu-system-x86_64w "
                          , appData, "\\maat.iso" ])
        empty
  pure ()

onlineVersion :: IO (Maybe Version)
onlineVersion = do
  appData <- env "LOCALAPPDATA"
  let currentVersionFilename = appData ++ "\\maat_online_version.json"
  runConduitRes $ httpSource "https://github.com/luisfdz-jda/MaatJus/releases/download/maat_win32_x86_64/maat_online_version.json" getResponseBody .| sinkFile currentVersionFilename
  content <- (fmap $ BS.pack . s2w8) $ readFile currentVersionFilename
  pure $ (decode content :: Maybe Version)

installedVersion :: IO (Maybe Version)
installedVersion = do
  appData <- env "LOCALAPPDATA"
  let installedVersionFilename = appData ++ "\\maat_installed_version.json"
  e <- doesFileExist installedVersionFilename
  if e then do
    content <- (fmap $ BS.pack . s2w8) $ readFile installedVersionFilename
    pure $ (decode content :: Maybe Version)
  else
    pure Nothing

paths :: IO (FilePath, FilePath, FilePath)
paths = do
  tempPath <- env "TEMP"
  appData <- env "LOCALAPPDATA"
  profilePath <- env "USERPROFILE"
  let desktopPath = profilePath ++ "\\Desktop"
  pure (tempPath, appData, desktopPath)

install' :: Maybe Version -> Maybe Version -> IO ()
install' ov iv = do
  putStrLn "-"
  (tempPath, appData, desktopPath) <- paths
  conditionalInstall install_haxm haxm_tgz "haxm.tgz"                                   ov iv tempPath appData desktopPath
  conditionalInstall install_maat_exe maat_exe "maat.exe"                               ov iv tempPath appData desktopPath
  conditionalInstall install_maat_ico maat_ico "maat.ico"                               ov iv tempPath appData desktopPath
  conditionalInstall install_maat_bat maat_bat "maat.bat"                               ov iv tempPath appData desktopPath
  conditionalInstall install_maat_jus_lnk maat_Jus_lnk "Maat.Jus.lnk"                   ov iv tempPath appData desktopPath
  conditionalInstall install_qemu qemu_tgz "qemu.tgz"                                   ov iv tempPath appData desktopPath
  conditionalInstall install_qemu_launcher qemu_launcher_bat "qemu_launcher.bat"        ov iv tempPath appData desktopPath
  conditionalInstall install_maat_iso maat_iso "maat.iso"                               ov iv tempPath appData desktopPath
  conditionalInstall install_uninstall_maat_bat uninstall_maat_bat "uninstall_maat.bat" ov iv tempPath appData desktopPath
  updateInstalledVersion ov

install :: IO ()
install = do
  ov <- onlineVersion
  iv <- installedVersion
  initialMsg ov iv
  install' ov iv
  launch
  pure ()
