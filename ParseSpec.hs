{-# LANGUAGE OverloadedStrings, TupleSections #-}
module ParseSpec where

import ParseUtil
import Data.Text as TS
import Data.Text.IO as TS
import Control.Applicative

type Name = Text
type DConstr = (Name, [DefTerm])
data DefStmt = DefType Name [Name] DConstr
             | DefVariant Name [Name] [DConstr]
             deriving (Show)
data DefTerm = DefConstr DConstr
             | DefTupleTy [DefTerm]
             | DefFn Name DefTerm
             deriving (Show)

namedList spacer term = (,) <$> identifier <*> (spacer *> sepBy term spacer)
parseDConstr = namedList skipHSpace parseDefTerm
parseDefFn = "fn" .*> buffer (liftA DefFn identifier) <*> parseDefTerm
brackTerm = parseDefTermParen <|> parseDefTupleTy
parseDefTerm = brackTerm <|> (DefConstr . (, []) <$> identifier)
parseDefTermParen = bracket '(' ')' $ brackTerm <|> parseDefFn <|>
  (DefConstr <$> namedList skipSpace parseDefTerm)
parseDefTupleTy = bracket '[' ']' $ liftA DefTupleTy commaTerms
  where commaTerms = sepBy parseDefTerm (buffer $ char ',')

-- TODO: maybe support empty lines while defining types/variants?
indent body = char ' ' *> bufferH body -- HACK
parseHead tag =
  uncurry tag <$> bufferH (namedList skipHSpace identifier) <* endOfLine
parseDefType = parseHead DefType <*> indent parseDConstr
parseDefVariant =
  parseHead DefVariant <*> sepBy (indent parseDConstr) endOfLine
parseDefStmt = ("type" .*> parseDefType) <|> ("variant" .*> parseDefVariant)
parseDefStmtList = buffer (sepBy parseDefStmt skipSpace) <* endOfInput

parseDefs = consumedResult . parse parseDefStmtList
parseStdin = parseDefs <$> TS.getContents
parseFile fname = parseDefs <$> TS.readFile fname

test = parseFile "basis.spec"
