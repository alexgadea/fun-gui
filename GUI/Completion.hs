module GUI.Completion where

import qualified Data.Text as T ( Text, concat, init, pack, length )

data Alias = Alias 
             { nickname :: [T.Text]
             , sym      :: T.Text
             }

data Completion = Completion { comp  :: T.Text }

allAlias :: [Alias]
allAlias = [ Alias [T.pack "equiv"] (T.pack "≡")
           --, Alias [T.pack "nequiv"] (T.pack "≢")
           , Alias [T.pack "impl", T.pack "rightarrow"] (T.pack "⇒")
           , Alias [T.pack "cons", T.pack "leftarrow"] (T.pack "⇐")
           , Alias [T.pack "neg"] (T.pack "¬")
           , Alias [T.pack "exist"] (T.pack "∃")
           , Alias [T.pack "forall"] (T.pack "∀")
           , Alias [T.pack "or"] (T.pack "∨")
           , Alias [T.pack "and"] (T.pack "∧")
           ----
           , Alias [T.pack "append"] (T.pack "▹")
           , Alias [T.pack "drop"] (T.pack "↓")
           , Alias [T.pack "take"] (T.pack "↑")
           ----
           , Alias [T.pack "<"] (T.pack "⟨")
           , Alias [T.pack ">"] (T.pack "⟩")
           ]

newCompletion :: Completion
newCompletion = Completion { comp  = T.pack "" }

addCharToCompletion :: T.Text -> Completion -> Completion
addCharToCompletion str cpl = cpl { comp = T.concat [comp cpl, str] }

rmCharToCompletion :: Completion -> Completion
rmCharToCompletion cpl
                   | T.length (comp cpl) > 0 = cpl { comp = T.init $ comp cpl }
                   | otherwise = cpl

-- Dado un completion chequea si se corresponde con un
-- alias. En tal caso, retorna el símbolo y el alias por
-- substituir.
checkCompletion :: Completion -> Maybe (T.Text, T.Text)
checkCompletion cpl = let cs = filter ((elem (comp cpl)) . nickname) allAlias
                      in if null cs
                         then Nothing
                         else Just (sym $ head cs, comp cpl)

beginChars :: [T.Text]
beginChars = map T.pack ["\\"]

endChars :: [T.Text]
endChars = map T.pack [" ", "\n"]

checkBeginChar :: T.Text -> Bool
checkBeginChar str = or $ map (str==) beginChars

checkEndChar :: T.Text -> Bool
checkEndChar str = or $ map (str==) endChars
