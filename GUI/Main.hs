module Main where

import System.Environment (getArgs)

import Graphics.UI.Gtk hiding (get)

import System.Glib
import System.Directory

import Control.Monad

import GUI.Gui
import Paths_fun_gui

main :: IO ()
main = do 
    _ <- initGUI
    
    _ <- setProgramName "fun"
    _ <- setApplicationName "fun"
    uifn <- getDataFileName "fun.ui"
    
    xml <- builderNew
    builderAddFromFile xml uifn
    
    args <- getArgs
    
    (existingFiles,errFiles) <- checkArgs args
    
    _ <- when (not $ null errFiles) (errFilesMsg errFiles)
    
    _ <- mainFunGui xml existingFiles
    
    mainGUI

checkArgs :: [String] -> IO ([String],[String])
checkArgs = foldM check ([],[])
    where
        check :: ([String],[String]) -> String -> IO ([String],[String])
        check (fp,er) arg = doesFileExist arg >>= \exist ->
                            if exist
                                then return (arg:fp,er)
                                else return (fp,arg:er)

errFilesMsg :: [String] -> IO ()
errFilesMsg errFiles = putStrLn $ 
                        unwords [ "Los siguientes archivos no existen o"
                                , "fallaron al intentar abrir:"
                                , foldr concatf "" errFiles
                                ]
    where
        concatf :: String -> String -> String
        concatf errf "" = errf
        concatf errf err = errf++", "++err
