{-# Language TypeOperators, RankNTypes, DoAndIfThenElse #-}
-- | Configuración de la lista de declaraciones del panel izquierdo.
module GUI.DeclList where

import GUI.GState
import GUI.EditBook

import Graphics.UI.Gtk hiding (eventButton, eventSent,get)
import Graphics.UI.Gtk.Gdk.Events 

import Data.Text(unpack,pack)
import Data.Maybe
import Data.Map (empty,elems)
import Data.Tree

import qualified Data.Foldable as F (mapM_) 

import Control.Monad.RWS

import Lens.Family

import Fun.Decl
import Fun.Environment
import Fun.Module
import Fun.Declarations

-- | Nombre a mostrar y acción al hace click.
type DeclItem = (String, DeclPos)

-- | Crea un treeStore para los DeclItem.
listDecls :: (Decl d, Show d) => [(DeclPos,d)] -> GuiMonad (TreeStore DeclItem)
listDecls decls = io $ treeStoreNew $ toForest decls

-- | Crea un forest de DeclItem.
toForest :: (Decl d, Show d) => [(DeclPos,d)] -> Forest DeclItem
toForest = map (\(pos,d) -> Node (unpack $ getNameDecl d, pos) [])

-- | Configura una lista de posición y declaracion.
setupDeclList :: (Decl d, Show d) => [(DeclPos,d)] -> TreeView -> Window -> 
                                     GuiMonad (TreeStore DeclItem)
setupDeclList decls tv pwin  = listDecls decls >>= setupDList
    where
        setupDList :: TreeStore DeclItem -> GuiMonad (TreeStore DeclItem)
        setupDList list = io $
            treeViewGetColumn tv 0 >>=
            F.mapM_ (treeViewRemoveColumn tv) >>
            treeViewColumnNew >>= \col ->
            treeViewSetHeadersVisible tv False >>
            treeViewSetModel tv list >>
            cellRendererTextNew >>= \renderer ->
            cellLayoutPackStart col renderer False >>
            cellLayoutSetAttributes col renderer list 
                                (\ind -> [ cellText := fst ind ]) >>
            treeViewAppendColumn tv col >>
            treeViewGetSelection tv >>= \tree ->
            onSelectionChanged tree (onSelection list tree) >>
            return list
        onSelection :: TreeStore DeclItem -> TreeSelection -> IO ()
        onSelection list tree = do
                sel <- treeSelectionGetSelectedRows tree
                if null sel then return ()
                else do
                    let h = head sel
                    (_,pos) <- treeStoreGetValue list h
                    putStrLn (show pos)
                    treeSelectionUnselectAll tree
                    return ()

-- | Configura las acciones de los DeclItem del panel izquierdo.
updateInfoPaned :: Environment -> GuiMonad ()
updateInfoPaned env = do
            content <- ask 
            let w = content ^. gFunWindow
            let specsList   = concatMap (specs . decls) env
            let funcsList   = concatMap (functions . decls) env
            let thmsList    = concatMap (theorems . decls) env
            let valsList    = concatMap (vals . decls) env
            let propsList   = concatMap (props . decls) env

            -- TODO: hay que actualizarlas los expanders aunque las
            -- listas sean vacías si las listas son vacías: sólo no
            -- hay que expandir.
            unless (null specsList) $ updateInfo specsList iSpecs w content
            unless (null funcsList) $ updateInfo funcsList iFuncs w content
            unless (null thmsList)  $ updateInfo thmsList  iThms  w content
            unless (null valsList)  $ updateInfo valsList  iVals  w content
            unless (null propsList) $ updateInfo propsList iProps w content
            
            return ()
    where
        updateInfo decls getExpndr w content = do
                    let expander = content ^. (gFunInfoPaned . getExpndr)
                    tv <- io treeViewNew
                    setupDeclList decls tv w
                    io $ cleanExpander expander
                    io $ set expander [ containerChild := tv 
                                      , expanderExpanded := True
                                      ]
                    io $ widgetShowAll expander
                    return ()
        cleanExpander :: Expander -> IO ()
        cleanExpander e = do
                    cs <- io $ containerGetChildren e
                    when (length cs > 1) (containerRemove e $ head cs)
