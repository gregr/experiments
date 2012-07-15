{-# LANGUAGE OverloadedStrings #-}
module Parse where

import Control.Monad
import Data.Attoparsec.ByteString.Char8
import Data.ByteString.Char8

data FormElem atom = Atom atom
                   | Form [FormElem atom]
  deriving (Show)

isAlpha = isAlpha_iso8859_15
isLParen = ('(' ==)
identifier = takeWhile1 isAlpha
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
  cond <- peekPred $ \ch -> isAlpha ch || isLParen ch
  if cond then do
    elem <- parseFormElement
    rest <- parseForm
    return $ elem : rest
   else return []

parseMore parser text = killPartial $ parse parser text
  where killPartial res = case res of
          Partial _ -> killPartial $ feed res empty
          _ -> res

parseAll parser text = case res of
  Done text out -> case feed (parse skipSpace text) empty of
    Done "" _ -> Right out
    _ -> Left $ "Unparsed trailing text: " ++ unpack text
  _ -> eitherResult res
  where res = parseMore parser text

testText = pack " (proc (w) w ((  ( w))) ) (proc (x  ) x)  1"
