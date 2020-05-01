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

haxm :: FilePath -> FilePath -> IO ()
haxm tempPath appPath = do
  download "https://github.com/luisfdz-jda/MaatJus/releases/download/Maat_win32_x86_64_1/haxm.tgz" $ tempPath ++ "\\haxm.tgz"
  Tar.unpack tempPath . Tar.read . GZip.decompress =<< BS.readFile (tempPath ++ "\\haxm.tgz")
  shell (T.pack $ concat [tempPath, "\\haxm\\intelhaxm-android.exe -f ", appPath, "\\haxm -a /qn MEMSIZETYPE=1 CUSTOMMEMSIZE=0 NOTCHECKVTENABLE=1"]) empty
  pure ()

qemu :: FilePath -> FilePath -> IO ()
qemu tempPath appPath = do
  download "https://github.com/luisfdz-jda/MaatJus/releases/download/Maat_win32_x86_64_1/qemu.tgz" $ tempPath ++ "\\qemu.tgz"
  Tar.unpack appPath . Tar.read . GZip.decompress =<< BS.readFile (tempPath ++ "\\qemu.tgz")
  pure ()

slax :: FilePath -> IO ()
slax appData = do
  download "https://github.com/luisfdz-jda/MaatJus/releases/download/Maat_win32_x86_64_1/slax.iso" $ appData ++ "\\slax.iso"
  pure ()

quemuLauncher :: FilePath -> IO ()
quemuLauncher appPath = do
  download "https://github.com/luisfdz-jda/MaatJus/releases/download/Maat_win32_x86_64_1/qemu_launcher.bat" $ appPath ++ "\\qemu\\qemu_launcher.bat "
  pure ()


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

install :: IO ()
install = do
  tempPath <- env "TEMP"
  appPath <- env "LOCALAPPDATA"
  appData <- env "APPDATA"
  -- haxm tempPath appPath
  -- qemu tempPath appPath
  -- slax appData
  quemuLauncher appPath

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

main :: IO ()
main = do
  initialMsg
  putStrLn "Descargando e instalando nueva release"
  putStrLn "Puede tardar varios minutos (no interrumpa el proceso). Por favor, espere..."
  install
  launch
