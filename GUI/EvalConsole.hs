module GUI.EvalConsole where

import Graphics.UI.Gtk hiding (get)
import Graphics.UI.Gtk.Glade

import Control.Monad.IO.Class
import Control.Monad.State
import Control.Monad.RWS
import Control.Arrow

import Control.Concurrent

import Lens.Family

import Data.Text (pack)
import Data.Maybe (fromJust,isJust)
import Data.Reference

import Text.Parsec
import Text.Parsec.String

import GUI.File
import GUI.GState
import GUI.EditBook
import GUI.File
import GUI.Config
import GUI.SymbolList
import GUI.Utils

import Fun.Environment
import Fun.Eval
import Equ.PreExpr(PreExpr)
import Equ.Parser(parser)

data EvAction = Step Int
              | Trace Int
              | Eval -- no implementado aun
                deriving Show

data EvResult = EvErr String
              | EvOk [PreExpr]

stepP,traceP,evalP :: Parser EvAction
stepP = withNumArg (string "step") >>= return . Step . snd
traceP = withNumArg (string "trace") >>= return . Trace . snd
evalP = string "eval" >> return Eval

withNumArg :: Parser String -> Parser (String,Int)
withNumArg p = p >>= \cmd -> blanks >> 
               many1 digit >>= \ds -> return (cmd,num 0 ds)
    where num ac [] = ac
          num ac (n:ns) = num (ac*10+(read [n])) ns

blanks = many1 (oneOf "\r\t ")

evalParser :: Parser (EvAction,String)
evalParser = choice [try stepP,try traceP, try evalP] >>= 
             \ac -> blanks >> many anyChar >>= \str -> 
             return (ac,str)

configCommTV :: TextView -> IO ()
configCommTV commTV = do
        widgetModifyBase commTV StateNormal backColorCommTV
        widgetModifyText commTV StateNormal textColorCommTV
        widgetShowAll commTV

configCommandConsole :: GuiMonad ()
configCommandConsole= ask >>= \content ->
                      get >>= \ref ->
        io $ do
        let entry = content ^. (gFunCommConsole . commEntry)
        let buf = content ^. (gFunCommConsole . commTBuffer)
        let tv = content ^. (gFunCommConsole . commTView)
        do _ <- entry `on` entryActivate $ io $ do
                    text <- entryGetText entry
                    entrySetText entry ""
                    titer <- textBufferGetEndIter buf
                    textBufferInsertPrompt buf titer $ text
                    st <- readRef ref
                    let env = st ^. gFunEnv
                    res <- processCommand env text
                    titer <- textBufferGetEndIter buf
                    textBufferInsertLn buf titer $ fmtEvResult res
                    titer2 <- textBufferGetEndIter buf
                    -- textViewScrollToIter no anda bien, por eso uso scrollToMark
                    mark <- textBufferCreateMark buf Nothing titer2 False
                    textViewScrollToMark tv mark 0 Nothing
                    widgetShowAll tv
                    return ()
           return ()

fmtEvResult :: EvResult -> String
fmtEvResult (EvErr err) = err
fmtEvResult (EvOk es) = fmtExps es

fmtExps :: [PreExpr] -> String
fmtExps [] = ""
fmtExps [e] = show e
fmtExps es = unlines $ zipWith fmtExp [1..] es
    where fmtExp n = (show n++) . showExp'

showExp' :: PreExpr -> String
showExp' = unlines . map (\l -> "\t"++l) . lines . show

processCommand :: Environment -> String -> IO EvResult
processCommand env str = case runParser evalParser () "EvalCommand" str of
                           Left err -> return $ showConsoleError "Error al parsear comando" (show err)
                           Right (action,str) -> do
                               (m,_,_) <- run env (runCmd action (parser str))
                               return m

showConsoleError :: String -> String -> EvResult
showConsoleError mom err = EvErr $ unlines [ mom ++ ": ", "\t" ++ err]

runCmd :: EvAction -> PreExpr -> EvalM EvResult
runCmd (Step n) e = start e >>
                    step (Just n) >>=
                    return . maybe (EvErr "No se pudo evaluar la expresión") (EvOk . return)
runCmd (Trace n) e = start e >>
                     trace (Just n) >>= \es ->
                     return $ if null es
                              then EvErr "No se pudo evaluar la expresión"
                              else EvOk es
runCmd Eval _ = return $ EvErr "Comando no implementado"