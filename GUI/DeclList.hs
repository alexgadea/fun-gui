{-# Language TypeOperators, RankNTypes#-}
-- | Configuración de la lista de símbolos.
module GUI.DeclList where

import Fun.Decl
import Fun.Environment
import Fun.Module
import Fun.Declarations

import GUI.GState

import Graphics.UI.Gtk hiding (eventButton, eventSent,get)
import Graphics.UI.Gtk.Gdk.Events 

import Data.Text(unpack,pack)
import Data.Maybe
import Data.Map (empty,elems)
import Data.Tree

import qualified Data.Foldable as F (mapM_) 

import Control.Monad.RWS

import Lens.Family

type DeclItem = (String, GuiMonad Int)

listDecls :: (Decl d, Show d) => [d] -> IO (TreeStore DeclItem)
listDecls decls = treeStoreNew $ toForest decls

toForest :: (Decl d, Show d) => [d] -> Forest DeclItem
toForest = map (\d -> Node (unpack $ getNameDecl d, return 0) [])

addItem :: (Decl d, Show d) => d -> DeclItem
addItem d = (show d, return 0)

setupDeclList :: (Decl d, Show d) => [d] -> TreeView -> Window -> IO (TreeStore DeclItem)
setupDeclList decls tv pwin  = 
    listDecls decls >>= setupDList tv 

setupDList :: TreeView  -> TreeStore DeclItem -> IO (TreeStore DeclItem)
setupDList tv list = 
    treeViewGetColumn tv 0 >>=
    F.mapM_ (treeViewRemoveColumn tv) >>
    treeViewColumnNew >>= \col ->
    treeViewSetHeadersVisible tv False >>
    treeViewSetModel tv list >>
    cellRendererTextNew >>= \renderer ->
    cellLayoutPackStart col renderer False >>
    cellLayoutSetAttributes col renderer list (\ind -> [ cellText := fst ind ]) >>
    treeViewAppendColumn tv col >>
    return list
    
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
--         updateInfo :: (Decl d, Show d) => 
--                       [d] -> (FunInfoPaned :->: Expander) ->
--                       Window -> GReader -> GuiMonad ()
        updateInfo decls getExpndr w content = do
                    let expander = content ^. (gFunInfoPaned . getExpndr)
                    tv <- io treeViewNew
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
