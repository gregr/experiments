{-# LANGUAGE OverloadedStrings #-}
module Parse where

import Data.Attoparsec.Text
import Data.Text as TS
import Data.Text.IO as TS
import Data.Char
import Control.Applicative
import Control.Monad

data FormElem atom = Atom atom
                   | Form [FormElem atom]
  deriving (Show)

isOneOf preds ch = or $ preds <*> [ch]
isIdentChar = isOneOf [isAlpha, isDigit, ('_' ==)]
isLParen = ('(' ==)
identifier = takeWhile1 isIdentChar
buffer body = skipSpace *> body <* skipSpace
bracket open close body = char open *> buffer body <* char close

peekPred pred = do
  mch <- peekChar
  case mch of
    Nothing -> return False
    Just ch -> return $ pred ch

parseParenForm = buffer $ bracket '(' ')' parseForm

parseFormElement = do
  skipSpace
  cond <- peekPred isLParen
  if cond then liftM Form parseParenForm
   else liftM Atom identifier

parseForm = do
  skipSpace
  cond <- peekPred $ isOneOf [isIdentChar, isLParen]
  if cond then do
    elem <- parseFormElement
    rest <- parseForm
    return $ elem : rest
   else return []

finishParse result@(Partial _) = finishParse $ feed result TS.empty
finishParse result = result
consumedResult done@(Done "" out) = done
consumedResult (Done text out) = Fail text [] "Unparsed trailing text"
consumedResult fail@(Fail _ _ _) = fail
consumedResult partial = consumedResult $ finishParse partial

parseForms = consumedResult . parse parseForm

testText = pack " (proc (w) w ((  ( w))) ) (proc (x  ) x) 1"
test = parseForms testText
