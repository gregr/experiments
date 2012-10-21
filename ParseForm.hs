{-# LANGUAGE OverloadedStrings #-}
module ParseForm where

import ParseUtil
import Data.Text
import Control.Applicative

data FormElem atom = Atom atom
                   | Form [FormElem atom]
  deriving (Show)

parseParenForm = bracket '(' ')' parseForm
parseFormElement = Form <$> parseParenForm <|> Atom <$> identifier
parseForm = many $ buffer parseFormElement
parseForms = consumedResult . parse parseForm

testText = pack " (proc (w) w ((  ( w))) ) (proc (x  ) x) 1"
test = parseForms testText
