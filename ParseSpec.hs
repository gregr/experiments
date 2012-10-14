{-# LANGUAGE OverloadedStrings #-}
module ParseSpec where

import Data.Attoparsec.Text
import Data.Text as TS
import Data.Text.IO as TS
import Data.Char
import Data.Maybe
import Control.Applicative
import Control.Monad

type Name = Text
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

skipHSpace = void $ skipWhile isHorizontalSpace
isIdentChar = isOneOf [isAlpha, isDigit, ('_' ==)]
identifier = takeWhile1 isIdentChar
peekCh = fromMaybe '\n' <$> peekChar

parseToList mfirst mrest = (:) <$> mfirst <*> mrest
peekActList fin act mfirst mrest = do
  ch <- peekCh
  if ch == fin then when (ch == '\n') (void space) >> return []
    else act >> parseToList mfirst mrest
peekList fin = peekActList fin $ return ()

parseNameList = many (identifier <* skipHSpace) <* void space -- peekList '\n' identifier parseNameList
parseDConstr fin = skipHSpace >> identifier >>= parseDConstrBody fin
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
  if fin == '\n' then skipHSpace else skipSpace
  peekList fin parseDefTerm $ parseDefTermList fin
parseCommaTermList comma =
  skipSpace *> peekActList ']' eatComma parseDefTerm (parseCommaTermList True)
  where eatComma = when comma (char ',' >> skipSpace)

parseDefHead = skipHSpace *> liftA (,) identifier <*>
  (skipHSpace *> parseNameList)
parseDefType =
  uncurry DefType <$> parseDefHead <* space <* skipSpace <*> parseDConstr '\n'
parseDefVariant = uncurry DefVariant <$> parseDefHead <*> parseDConstrList
parseDefStmt = skipSpace *>
  ("type" .*> parseDefType) <|> ("variant" .*> parseDefVariant)

parseDefStmtList = skipSpace *> (endOfInput *> pure [] <|>
  parseToList parseDefStmt parseDefStmtList)

parseMore parser text = killPartial $ parse parser text
  where killPartial res = case res of
          Partial _ -> killPartial $ feed res TS.empty
          _ -> res
parseAll parser text = case res of
  Done text out -> case feed (parse skipSpace text) TS.empty of
    Done "" _ -> Right out
    _ -> Left $ "Unparsed trailing text: " ++ TS.unpack text
  _ -> eitherResult res
  where res = parseMore parser text
parseDefs text = parseAll parseDefStmtList (TS.append text "\n")
-- TODO: there seems to be an "impossible error" bug with parseOnly currently
-- parseDefs text = parseOnly parseDefStmtList (TS.append text "\n")

parseStdin = parseDefs <$> TS.getContents
parseFile fname = parseDefs <$> TS.readFile fname

test = parseFile "basis.spec"
