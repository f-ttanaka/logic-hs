module ToyProlog.Parsing.Parser where

import Data.Text
import ToyProlog.Common
import ToyProlog.Data.Term
import ToyProlog.Parsing.Common
import ToyProlog.Parsing.Lexer

termP :: Parser Term
termP = varP <|> funcP
  where
    varP = TVar <$> capitalized
    funcP = TFunc <$> identifier' <*> parens (commaSep termP)

ruleP :: Parser Rule
ruleP =
  Rule
    <$> termP
    <*> option [] (reservedOp ":-" >> commaSep1 termP)

doParse :: (MonadThrow m) => Parser a -> String -> m a
doParse p source = case parse p "" (pack source) of
  Right x -> return x
  Left err -> throwString $ show err
