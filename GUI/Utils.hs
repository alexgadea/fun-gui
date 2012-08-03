{-# LANGUAGE NoMonomorphismRestriction #-}
-- Módulo para definir funciones útiles, generales a la interfaz
module GUI.Utils where

import Graphics.UI.Gtk

import Control.Monad.Reader
import Control.Monad.State
import Control.Monad.RWS

io = liftIO

textBufferInsertPrompt buf titer = textBufferInsertLn buf titer . ("fun> "++)
textBufferInsertLn buf titer = textBufferInsert buf titer . ("\n"++)

        
-- | Inserta un string al final de un text buffer y scrollea el text view.
--   Retorna el iter inicial y final del texto ingresado
putStrAtEnd :: TextBuffer -> TextView -> String -> IO ()
putStrAtEnd buf tv msg = do
        titer <- textBufferGetEndIter buf
        textBufferInsertLn buf titer $ msg
        -- textViewScrollToIter no anda bien, por eso uso scrollToMark
        textBufferInsertLn buf titer ""
        titer2 <- textBufferGetEndIter buf
        
        mark <- textBufferCreateMark buf Nothing titer2 False
        textViewScrollToMark tv mark 0 Nothing