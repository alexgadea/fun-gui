{-# Language TypeOperators, RankNTypes#-}
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
type DeclItem = (String, GuiMonad ())

-- | Crea un treeStore para los DeclItem.
listDecls :: (Decl d, Show d) => [(DeclPos,d)] -> IO (TreeStore DeclItem)
listDecls decls = treeStoreNew $ toForest decls

-- | Crea un forest de DeclItem.
toForest :: (Decl d, Show d) => [(DeclPos,d)] -> Forest DeclItem
toForest = map (\(pos,d) -> Node (unpack $ getNameDecl d, io $ print 1) [])

-- | Configura una lista de posición y declaracion.
setupDeclList :: (Decl d, Show d) => [(DeclPos,d)] -> TreeView -> Window -> 
                                     IO (TreeStore DeclItem)
setupDeclList decls tv pwin  = listDecls decls >>= setupDList
    where
        setupDList :: TreeStore DeclItem -> IO (TreeStore DeclItem)
        setupDList list = 
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
            return list

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
                    tree <- io $ treeViewGetSelection tv
                    io $ treeSelectionSetMode tree  SelectionSingle
                    io $ onSelectionChanged tree (print 1)
                    io $ setupDeclList decls tv w
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
