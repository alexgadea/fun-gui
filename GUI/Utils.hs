{-# LANGUAGE NoMonomorphismRestriction #-}
-- Módulo para definir funciones útiles, generales a la interfaz
module GUI.Utils where

import Graphics.UI.Gtk

import Control.Monad.IO.Class

import Control.Applicative

io :: MonadIO m => IO a -> m a
io = liftIO

textBufferInsertLn :: TextBufferClass self => self -> String -> IO ()
textBufferInsertLn buf str = textBufferGetEndIter buf >>= \titer ->
                             textBufferInsert buf titer ('\n':str)

        
-- | Inserta un string al final de un text buffer y scrollea el text view.
--   Retorna el iter inicial y final del texto ingresado
putStrAtEnd :: TextBuffer -> TextView -> String -> IO ()
putStrAtEnd buf tv msg = do
        textBufferInsertLn buf msg
        -- textViewScrollToIter no anda bien, por eso uso scrollToMark
        textBufferInsertLn buf ""
        titer2 <- textBufferGetEndIter buf
        
        mark <- textBufferCreateMark buf Nothing titer2 False
        textViewScrollToMark tv mark 0 Nothing

-- | Pone un mensaje en una área de estado.
putMsgSB :: Statusbar -> ContextId -> String -> IO ()
putMsgSB st cid m = statusbarPush st cid m >> return ()
                 
-- | 
setLoadedModuleInfo :: Label -> Maybe String -> IO ()
setLoadedModuleInfo label Nothing = labelSetText label "Error al cargar el módulo" >>
                                    styleInfoError >>= widgetModifyFont label
setLoadedModuleInfo label (Just modN) = styleInfoModule >>= widgetModifyFont label >>
                                       labelSetText label modN


-- -- | Estilo para títulos en info-boxes
styleInfoModule ::  IO (Maybe FontDescription)
styleInfoModule = Just <$> fontBold

styleInfoError :: IO (Maybe FontDescription)
styleInfoError = Just <$> fontItalic

fontItalic :: IO FontDescription
fontItalic = fontDescriptionNew >>= \fd -> 
             fontDescriptionSetStyle fd StyleItalic >>
             return fd

fontBold :: IO FontDescription
fontBold = fontDescriptionNew >>= \fd -> 
           fontDescriptionSetWeight fd WeightBold >>
           return fd

