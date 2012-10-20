{-# LANGUAGE OverloadedStrings #-}
module ParseForm where

import Data.Attoparsec.Text
import Data.Text as TS
import Data.Text.IO as TS
import Data.Char
import Control.Applicative

data FormElem atom = Atom atom
                   | Form [FormElem atom]
  deriving (Show)

isOneOf preds ch = or $ preds <*> [ch]
isIdentChar = isOneOf [isAlpha, isDigit, ('_' ==)]
isLParen = ('(' ==)
identifier = takeWhile1 isIdentChar
buffer body = skipSpace *> body <* skipSpace
bracket open close body = char open *> buffer body <* char close

parseParenForm = bracket '(' ')' parseForm
parseFormElement = Form <$> parseParenForm <|> Atom <$> identifier
parseForm = many $ buffer parseFormElement

finishParse result@(Partial _) = finishParse $ feed result TS.empty
finishParse result = result
consumedResult done@(Done "" out) = done
consumedResult (Done text out) = Fail text [] "Unparsed trailing text"
consumedResult fail@(Fail _ _ _) = fail
consumedResult partial = consumedResult $ finishParse partial

parseForms = consumedResult . parse parseForm

testText = pack " (proc (w) w ((  ( w))) ) (proc (x  ) x) 1"
test = parseForms testText
