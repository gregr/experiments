{-# LANGUAGE NoMonomorphismRestriction #-}
module Repl where

import Simpler
import qualified Parse as P
import qualified Data.ByteString.Char8 as BS
import System.Console.Haskeline

-- TODO: need a way to accumulate partial lines
readPrompt = runInputT defaultSettings (getInputLine "> ")

i_repl = do
  text <- readPrompt >>= maybe (error "EOF") return
  case P.parseForms $ BS.pack text of
    Left msg -> error msg
    Right forms -> putStrLn $ "parsed: " ++ show (map elemToTerm forms)
  i_repl

litSym = Literal . Sym
elemToTerm (P.Atom atom) = litSym $ Global $ BS.unpack atom -- TODO: symbol maps
elemToTerm (P.Form []) = litSym nat_zero -- TODO: unit
-- TODO: process syntactic forms
elemToTerm (P.Form (first : rest)) = Apply first' rest'
  where first' = elemToTerm first
        rest' = map elemToTerm rest
