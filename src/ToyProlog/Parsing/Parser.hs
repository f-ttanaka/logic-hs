module ToyProlog.Parsing.Parser where

import ToyProlog.Common
import ToyProlog.Data.Term
import ToyProlog.Parsing.Common
import ToyProlog.Parsing.Lexer

termP :: Parser Term
termP =
  (TVar <$> capitalized)
    <|> (TFunc <$> identifier' <*> parens (commaSep termP))

ruleP :: Parser Rule
ruleP =
  Rule
    <$> termP
    <*> option [] (reservedOp ":-" >> commaSep1 termP)

parseRule :: String -> Maybe Rule

parseRule
