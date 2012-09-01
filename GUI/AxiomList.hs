-- | Configuración de la lista de axiomas.
module GUI.AxiomList where

import Equ.Syntax
import Equ.Proof 
import qualified Equ.Theories as ET (axiomGroup,Grouped,toForest) 
import qualified Equ.Proof.Proof as P
import Graphics.UI.Gtk hiding (eventButton, eventSent,get)
import Graphics.UI.Gtk.Gdk.Events 

import Data.Text(unpack,pack)
import Data.Maybe
import Data.Map (empty,elems)
import Data.Tree

import Lens.Family

import Control.Monad(liftM, when,unless)
import Control.Monad.Trans(liftIO)
import Control.Monad.Trans.RWS
import qualified Data.Foldable as F (mapM_) 

import GUI.GState
import GUI.Utils
import GUI.EditBook

type AxiomItem = String

-- | Genera el TreeStore con la lista de axiomas.
listAxioms :: IO (TreeStore AxiomItem)
listAxioms = treeStoreNew $ forest ET.axiomGroup ++ forest eval
    where 
        eval :: ET.Grouped Basic
        eval = [(pack "Aritmética", [Evaluate])]

        forest ::  (Truth t, Show t) => ET.Grouped t -> Forest AxiomItem
        forest = ET.toForest unpack addItem

        addItem :: (Truth t, Show t) => t -> AxiomItem
        addItem = unpack . truthName

-- | Configuración del botón para activar/desactivar la lista de axiomas.
configAxFrameButton :: GuiMonad ()
configAxFrameButton = do
                content <-  ask
                let af          = content ^. (gFunAxiomList . gAxFrame)
                let afButton    = content ^. (gFunToolbar . axFrameB)
                
                active <- io $ toggleToolButtonGetActive afButton
                if active 
                   then io $ widgetShowAll af
                   else io $ widgetHideAll af

-- | Configuración general de la lista de axiomas.
configAxiomList :: GuiMonad ()
configAxiomList = do
            content <- ask
            let af = content ^. (gFunAxiomList . gAxFrame)
            let tv = content ^. (gFunAxiomList . gAxTreeView)
            
            list <- io listAxioms
            io $ setupAxiomList tv list
            eventsAxiomList tv list
            io $ widgetHideAll af
            
            return ()

-- | Configuración del treeview de axiomas.
setupAxiomList :: TreeView -> TreeStore AxiomItem -> IO ()
setupAxiomList tv list = 
    treeViewGetColumn tv 0 >>=
    F.mapM_ (\c -> treeViewRemoveColumn tv c) >>
    treeViewColumnNew >>= \col ->
    treeViewSetHeadersVisible tv False >>
    treeViewSetModel tv list >>
    cellRendererTextNew >>= \renderer ->
    cellLayoutPackStart col renderer False >>
    cellLayoutSetAttributes col renderer list (\ind -> [ cellText := ind ]) >>
    treeViewAppendColumn tv col >>
    return ()

-- | Configuración de los eventos del treeview de axiomas.
eventsAxiomList :: TreeView -> TreeStore AxiomItem -> GuiMonad ()
eventsAxiomList tv list = 
            io (treeViewGetSelection tv >>= \tree -> 
            treeSelectionSetMode tree SelectionSingle >>
            treeSelectionUnselectAll tree >>
            treeViewSetModel tv list >> widgetShowAll tv >> return tree) 
            >>= \tree -> ask >>= \content -> get >>= \st ->
            io (onSelectionChanged tree (eval (oneSelection list tree) content st))
            >> return ()
    where
        eval action content st = evalRWST action content st >> return ()

-- | Configuración de un evento en un elemento particular del treeview de 
-- axiomas.
oneSelection :: TreeStore AxiomItem -> TreeSelection -> GuiMonad ()
oneSelection list tree =
            io (treeSelectionGetSelectedRows tree) >>= \sel ->
            unless (null sel) $ return (head sel) >>= \h ->
            unless (length h == 1) $ getGState >>= \st -> 
            return (st ^. gFunEditBook) >>= \mEditBook ->
            maybe (return ()) (configSelection h) mEditBook
    where
        configSelection :: TreePath -> FunEditBook -> GuiMonad ()
        configSelection path editBook = 
                getTextEditFromFunEditBook editBook >>= \(_,_,tv) ->
                io (treeStoreGetValue list path) >>= \ax ->
                addToCursorBuffer tv ax
        addToCursorBuffer :: TextView -> String -> GuiMonad ()
        addToCursorBuffer tv repr = io $ do
                buf <- textViewGetBuffer tv
                textBufferInsertAtCursor buf repr
                widgetGrabFocus tv