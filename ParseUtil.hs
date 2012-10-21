{-# LANGUAGE OverloadedStrings #-}
module ParseUtil (
  module Data.Attoparsec.Text,
  isAllOf, isOneOf, identifier, skipHSpace, buffer, bufferH, bracket,
  finishParse, consumedResult) where

import Data.Attoparsec.Text
import Data.Text as TS
import Data.Char
import Control.Applicative

isCombOf comb preds ch = comb $ preds <*> [ch]
isAllOf = isCombOf and
isOneOf = isCombOf or
isIdentChar = isOneOf [isAlpha, isDigit, ('_' ==)]
identifier = takeWhile1 isIdentChar
skipHSpace = skipWhile isHorizontalSpace
buffer body = skipSpace *> body <* skipSpace
bufferH body = skipHSpace *> body <* skipHSpace
bracket open close body = char open *> buffer body <* char close

-- sometimes you get another Partial after feeding empty
finishParse result@(Partial _) = finishParse $ feed result TS.empty
finishParse result = result
consumedResult done@(Done "" out) = done
consumedResult (Done text out) = Fail text [] "Unparsed trailing text"
consumedResult fail@(Fail _ _ _) = fail
consumedResult partial = consumedResult $ finishParse partial
