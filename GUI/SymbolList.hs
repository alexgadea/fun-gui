{-# Language DoAndIfThenElse #-}
-- | Configuración de la lista de símbolos.
module GUI.SymbolList where

import Equ.Theories
import Equ.Syntax

import Graphics.UI.Gtk hiding (eventButton, eventSent,get)
import qualified Graphics.UI.Gtk as Gtk
import Data.Text(unpack)

import Control.Lens hiding (set)
import Control.Monad.Trans.RWS
import Control.Applicative ((<$>))
import qualified Data.Foldable as F

import GUI.GState
import GUI.EditBook
import GUI.Utils

type SymItem = String

listSymbols :: IO (ListStore SymItem)
listSymbols = listStoreNew $ map addEncloseItem quantifiersList
                          ++ map addItem operatorsList
                          ++ map addItem constantsList
    where addItem :: Syntactic s =>  s -> SymItem
          addItem syn = unpack $ tRepr syn
          addEncloseItem :: Syntactic s =>  s -> SymItem
          addEncloseItem q = "〈"++ addItem q ++ ":" ++ ":" ++ "〉"

configSymFrameButton :: GuiMonad ()
configSymFrameButton = do
                content <-  ask
                let sf          = content ^. (gFunSymbolList . gSymFrame)
                let (sfButton,sfItem)    = content ^. (gFunToolbar . symFrameCtrl)
                visible <- io $ Gtk.get sf widgetVisible
                io $ toggleToolButtonSetActive sfButton (not visible)
                io $ Gtk.set sfItem [ checkMenuItemActive := not visible ]
                if visible
                  then io $ widgetHide sf
                  else io $ widgetShowAll sf
                
                
configSymbolList :: GuiMonad ()
configSymbolList = do
                content <-  ask
                let sf      = content ^. (gFunSymbolList . gSymFrame)
                let iv      = content ^. (gFunSymbolList . gSymIconView)
                
                list <- io listSymbols
                _ <- io $ setupSymbolList iv list
                eventsSymbolList iv list                
                io $ widgetHide sf
                
                return ()

-- | La configuración de la lista de símbolos propiamente hablando.
setupSymbolList :: IconView -> ListStore SymItem -> IO (ListStore SymItem)
setupSymbolList iv list = 
    return (makeColumnIdString 1) >>= \scol ->
    return (makeColumnIdPixbuf (-1)) >>= \pcol ->
    iconViewSetTextColumn iv scol >>
    iconViewSetPixbufColumn iv pcol >>
    customStoreSetColumn list scol id >>
    set iv [ iconViewModel := Just list
        , iconViewPixbufColumn := pcol
        , iconViewTextColumn := scol
        , iconViewRowSpacing := 0
        , iconViewMargin := 0
        , iconViewSelectionMode := SelectionSingle
        ] >>
    widgetShowAll iv >>
    return list

eventsSymbolList :: IconView -> ListStore SymItem -> GuiMonad ()
eventsSymbolList iv list = do
            content <- ask
            s <- get
            _ <- io $ iv `on` focusInEvent $ (io $ iconViewSelectPath iv [0] >> return True)
            _ <- io $ iv `on` itemActivated $ \path -> 
                        evalRWST (oneSelection list path) content s >> return ()
            return ()

oneSelection :: ListStore SymItem -> TreePath -> GuiMonad ()
oneSelection list path = do
                s <- getGState
                configSelection $ s ^. gFunEditBook
    where
        configSelection :: FunEditBook -> GuiMonad ()
        configSelection editBook = 
                getTextEditFromFunEditBook editBook >>=
                maybe (return ()) (\(_,_,tv) ->
                                    io (getElem list path) >>=
                                    F.mapM_ (addToCursorBuffer tv))
        addToCursorBuffer :: TextView -> String -> GuiMonad ()
        addToCursorBuffer tv repr = io $ do
                buf <- textViewGetBuffer tv
                textBufferInsertAtCursor buf repr
                widgetGrabFocus tv

getElem :: ListStore a -> TreePath -> IO (Maybe a)
getElem l p = treeModelGetIter l p >>= \i ->
              flip (maybe (return Nothing)) i $ \it -> 
                        (\idx -> listStoreGetSize l >>= \len -> 
                        if idx < len
                            then Just <$> listStoreGetValue l idx
                            else return Nothing) (listStoreIterToIndex it)
