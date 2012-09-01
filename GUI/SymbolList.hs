{-# Language DoAndIfThenElse #-}
-- | Configuración de la lista de símbolos.
module GUI.SymbolList where

import Equ.Theories
import Equ.Syntax

import Graphics.UI.Gtk hiding (eventButton, eventSent,get)

import Data.Text(unpack)

import Lens.Family

import Control.Monad.Trans.State hiding (get,put)
import Control.Monad.Trans.RWS
import Control.Applicative ((<$>))
import Control.Monad (when)
import qualified Data.Foldable as F

import GUI.GState
import GUI.EditBook
import GUI.Config
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
                let sfButton    = content ^. (gFunToolbar . symFrameB)
                
                active <- io $ toggleToolButtonGetActive sfButton
                if active 
                   then io $ widgetShowAll sf
                   else io $ widgetHideAll sf

configSymbolList :: GuiMonad ()
configSymbolList = do
                content <-  ask
                s <- get
                let sf      = content ^. (gFunSymbolList . gSymFrame)
                let iv      = content ^. (gFunSymbolList . gSymIconView)
                let goLB    = content ^. (gFunSymbolList . gGoLeftBox)
                let goRB    = content ^. (gFunSymbolList . gGoRightBox)
                let scrollW = content ^. (gFunSymbolList . gScrollW)
                
                list <- io listSymbols
                io $ setupScrolledWindowSymbolList scrollW goLB goRB s
                io $ setupSymbolList iv list
                eventsSymbolList iv list
                io $ widgetHideAll sf
                
                return ()

-- | La configuración de la lista de símbolos propiamente hablando.
setupSymbolList :: IconView -> ListStore SymItem -> IO (ListStore SymItem)
setupSymbolList iv list = 
     listStoreGetSize list >>= \listSize ->
     return (makeColumnIdString 1) >>= \scol ->
     return (makeColumnIdPixbuf (-1)) >>= \pcol ->
     iconViewSetTextColumn iv scol >>
     iconViewSetPixbufColumn iv pcol >>
     customStoreSetColumn list scol id >>
     set iv [ iconViewModel := Just list
            , iconViewPixbufColumn := pcol
            , iconViewTextColumn := scol
            , iconViewColumns := listSize
            , iconViewRowSpacing := 0
            , iconViewMargin := 0
            , iconViewSelectionMode := SelectionSingle
            ] >>
     widgetShowAll iv >>
     return list

setupScrolledWindowSymbolList :: ScrolledWindow -> HBox -> HBox -> GStateRef -> IO ()
setupScrolledWindowSymbolList sw goLb goRb s = do
            goR <- makeScrollArrow goRb stockGoForward
            goL <- makeScrollArrow goLb stockGoBack
            (Just  swslH) <- scrolledWindowGetHScrollbar sw
            adj <- rangeGetAdjustment swslH
            setupScrollWithArrow adj goR scrollInc s
            setupScrollWithArrow adj goL scrollDec s
            widgetSetChildVisible swslH False
            widgetHide swslH
            widgetShowAll goLb
            widgetShowAll goRb

setupScrollWithArrow :: Adjustment -> Button -> Double -> GStateRef -> IO (ConnectId Button)
setupScrollWithArrow adj go inc s = 
                go `on` buttonPressEvent $ tryEvent $ 
                flip evalStateT s $ io $ do
                        val <- io $ adjustmentGetValue adj
                        upper <- adjustmentGetUpper adj
                        pageSize <- adjustmentGetPageSize adj
                        when (upper - pageSize > val + inc) $ 
                             adjustmentSetValue adj (val + inc)

makeScrollArrow :: HBox -> StockId -> IO Button
makeScrollArrow box si = do
                        symGo <- buttonNew
                        
                        buttonSetRelief symGo ReliefNone
                        
                        arrow <- imageNewFromStock si IconSizeMenu
                        
                        buttonSetImage symGo arrow
                        
                        boxPackStart box symGo PackNatural 0
                        return symGo

eventsSymbolList :: IconView -> ListStore SymItem -> GuiMonad ()
eventsSymbolList iv list = do
            content <- ask
            s <- get
            io $ iv `on` itemActivated $ \path -> 
                        evalRWST (oneSelection list path) content s >> return ()
            return ()

oneSelection :: ListStore SymItem -> TreePath -> GuiMonad ()
oneSelection list path = do
                s <- getGState
                let mEditBook = s ^. gFunEditBook
                maybe (return ()) configSelection mEditBook
    where
        configSelection :: FunEditBook -> GuiMonad ()
        configSelection editBook = 
                getTextEditFromFunEditBook editBook >>= \(_,_,tv) ->
                io (getElem list path) >>=
                F.mapM_ (addToCursorBuffer tv)
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
