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
lparen = char '('
rparen = char ')'

peekPred pred = do
  mch <- peekChar
  case mch of
    Nothing -> return False
    Just ch -> return $ pred ch

parseParenForm = do
  skipSpace
  lparen
  form <- parseForm
  skipSpace
  rparen
  return form

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

parseMore parser text = killPartial $ parse parser text
  where killPartial res = case res of
          Partial _ -> killPartial $ feed res TS.empty
          _ -> res

parseAll parser text = case res of
  Done text out -> case feed (parse skipSpace text) TS.empty of
    Done "" _ -> Right out
    _ -> Left $ "Unparsed trailing text: " ++ unpack text
  _ -> eitherResult res
  where res = parseMore parser text

parseForms = parseAll parseForm

testText = pack " (proc (w) w ((  ( w))) ) (proc (x  ) x) 1"
test = parseForms testText
