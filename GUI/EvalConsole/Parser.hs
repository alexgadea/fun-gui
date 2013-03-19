module GUI.EvalConsole.Parser where

import Text.Parsec
import Text.Parsec.String


import Equ.PreExpr(PreExpr)
import Equ.Parser(parsePreExpr,initPExprState,PExprState(..),ParenFlag(..))

import GUI.EvalConsole.EvalComm

import Data.Functor.Identity


type ParserCmd b = ParsecT String PExprState Identity b


{- | Comandos del evaluador:
     $ load expr 
        Carga la expresión en el estado del evaluador.
     $ step
        Realiza un paso de evaluación sobre la expresión
        cargada en el estado.
     $ expr
        Evalúa la expresión expr hasta llegar a una expresión canónica.
-}

parseLoad :: ParserCmd EvalComm
parseLoad = string "load" >>
            spaces >>
            parsePreExpr >>= \e ->
            spaces >>
            return (Load e)
            
parseStep :: ParserCmd EvalComm
parseStep = string "step" >>
            return Step
            
parseEval :: ParserCmd EvalComm
parseEval = spaces >>
            parsePreExpr >>= \e ->
            spaces >>
            return (Eval e)
            
            
parserCmd :: ParserCmd EvalComm
parserCmd =     (try parseLoad)
            <|> (try parseStep)
            <|> parseEval
            <?> "Comandos válidos: <expr> | load <expr> | step"
            
            
-- | Función principal de parseo desde String
parseFromString :: String -> Either ParseError EvalComm
parseFromString = runParser parserCmd (initPExprState UnusedParen) ""

            