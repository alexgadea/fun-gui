{-# LANGUAGE NoMonomorphismRestriction #-}
-- Módulo para definir funciones útiles, generales a la interfaz
module GUI.Utils where

import Graphics.UI.Gtk

import Control.Monad.IO.Class
import Control.Monad.Trans.Reader
import Control.Monad.Trans.State
import Control.Monad.Trans.RWS

io = liftIO

textBufferInsertLn buf str = textBufferGetEndIter buf >>= \titer ->
                             textBufferInsert buf titer ("\n"++str)

        
-- | Inserta un string al final de un text buffer y scrollea el text view.
--   Retorna el iter inicial y final del texto ingresado
putStrAtEnd :: TextBuffer -> TextView -> String -> IO ()
putStrAtEnd buf tv msg = do
        textBufferInsertLn buf $ msg
        -- textViewScrollToIter no anda bien, por eso uso scrollToMark
        textBufferInsertLn buf ""
        titer2 <- textBufferGetEndIter buf
        
        mark <- textBufferCreateMark buf Nothing titer2 False
        textViewScrollToMark tv mark 0 Nothing