{-# LANGUAGE OverloadedStrings #-}

module Profiling where

import System.Clock
import Network.HTTP.Client (Request, parseRequest)
import Control.Monad.IO.Class (liftIO)

import MaatWin32_x86_64 (env, download)

seconds :: TimeSpec -> TimeSpec -> Double
seconds (TimeSpec s1 n1) (TimeSpec s2 n2) =
  (fromIntegral $ s2 - s1) + ((fromIntegral $n2 - n1) / 1e9)

profilingDownload :: String -> FilePath -> IO ()
profilingDownload source destination = do
  request <- parseRequest source

  t1 <- liftIO $  getTime Monotonic
  download request destination
  t2 <- liftIO $  getTime Monotonic
  download request destination
  t3 <- liftIO $  getTime Monotonic
  download request destination
  t4 <- liftIO $  getTime Monotonic
  download request destination
  t5 <- liftIO $  getTime Monotonic
  download request destination
  t6 <- liftIO $  getTime Monotonic
  download request destination
  t7 <- liftIO $  getTime Monotonic
  download request destination
  t8 <- liftIO $  getTime Monotonic
  download request destination
  t9 <- liftIO $  getTime Monotonic
  download request destination
  t10 <- liftIO $  getTime Monotonic
  download request destination
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
