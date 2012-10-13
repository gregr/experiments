{-# LANGUAGE OverloadedStrings #-}
module ParseSpec where

import Data.Attoparsec.ByteString.Char8
import Data.ByteString.Char8 as BS
import Data.Maybe
import Control.Applicative
import Control.Monad

type Name = ByteString
type DConstr = (Name, [DefTerm])
data DefStmt = DefType Name [Name] DConstr
             | DefVariant Name [Name] [DConstr]
             deriving (Show)
data DefTerm = DefConstr DConstr
             | DefTupleTy [DefTerm]
             | DefFn Name DefTerm
             deriving (Show)

isCombOf comb preds ch = comb $ preds <*> [ch]
isAllOf = isCombOf and
isOneOf = isCombOf or

--endOfLine isEndOfLine
skipNonNewlineSpace = void $ skipWhile $ isAllOf [isSpace, ('\n' /=)]
isIdentChar = isOneOf [isAlpha_iso8859_15, isDigit, ('_' ==)]
identifier = takeWhile1 isIdentChar
peekCh = fromMaybe '\n' <$> peekChar

parseToList mfirst mrest = (:) <$> mfirst <*> mrest
peekActList fin act mfirst mrest = do
  ch <- peekCh
  if ch == fin then when (ch == '\n') (void space) >> return []
    else act >> parseToList mfirst mrest
peekList fin = peekActList fin $ return ()

parseNameList = skipNonNewlineSpace *> peekList '\n' identifier parseNameList
parseDConstr fin = skipNonNewlineSpace >> identifier >>= parseDConstrBody fin
parseDConstrBody fin name = (,) name <$> parseDefTermList fin
parseDConstrList = peekList '\n' (parseDConstr '\n') parseDConstrList
parseDefTupleTy =
  char '[' *> liftA DefTupleTy (parseCommaTermList False) <* char ']'
parseDefFn = skipSpace *> liftA DefFn identifier <*> parseDefTerm <* skipSpace
parseDefTermAlt alt = skipSpace *>
  (parseDefTermParen <|> parseDefTupleTy <|> (identifier >>= alt))
parseDefTermParen =
  char '(' *> parseDefTermAlt alt <* skipSpace <* char ')'
  where alt name = case name of
          "fn" -> parseDefFn
          otherwise -> DefConstr <$> parseDConstrBody ')' name
parseDefTerm = parseDefTermAlt (\name -> return $ DefConstr (name, []))
parseDefTermList fin = do
  if fin == '\n' then skipNonNewlineSpace else skipSpace
  peekList fin parseDefTerm $ parseDefTermList fin
parseCommaTermList comma =
  skipSpace *> peekActList ']' eatComma parseDefTerm (parseCommaTermList True)
  where eatComma = when comma (char ',' >> skipSpace)

parseDefHead = skipNonNewlineSpace *> liftA (,) identifier <*> parseNameList
parseDefType =
  uncurry DefType <$> parseDefHead <* space <* skipSpace <*> parseDConstr '\n'
parseDefVariant = uncurry DefVariant <$> parseDefHead <*> parseDConstrList
parseDefStmt = skipSpace *>
  ("type" .*> parseDefType) <|> ("variant" .*> parseDefVariant)

--endOfInput atEnd

parseDefStmtList = do
  skipSpace
  mch <- peekChar
  case mch of
    Nothing -> return []
    Just _ -> parseToList parseDefStmt parseDefStmtList

parseMore parser text = killPartial $ parse parser text
  where killPartial res = case res of
          Partial _ -> killPartial $ feed res BS.empty
          _ -> res
parseAll parser text = case res of
  Done text out -> case feed (parse skipSpace text) BS.empty of
    Done "" _ -> Right out
    _ -> Left $ "Unparsed trailing text: " ++ unpack text
  _ -> eitherResult res
  where res = parseMore parser text
parseDefs text = parseAll parseDefStmtList (BS.append text "\n")

parseStdin = parseDefs <$> BS.getContents
parseFile fname = parseDefs <$> BS.readFile fname

test = parseFile "basis.spec"
