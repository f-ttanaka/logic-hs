module ToyProlog.Parsing.Lexer where

import qualified Text.Parsec.Token as Tok
import ToyProlog.Common
import ToyProlog.Parsing.Common

reservedNames :: [String]
reservedNames = []

reservedOps :: [String]
reservedOps =
  [ ":-"
  ]

lexerDef :: Tok.GenLanguageDef Text () Identity
lexerDef =
  Tok.LanguageDef
    { Tok.commentStart = "{-",
      Tok.commentEnd = "-}",
      Tok.commentLine = "--",
      Tok.nestedComments = True,
      Tok.identStart = letter,
      Tok.identLetter = alphaNum <|> oneOf "_'",
      Tok.opStart = oneOf ":!#$%&*+./<=>?@\\^|-~",
      Tok.opLetter = oneOf ":!#$%&*+./<=>?@\\^|-~",
      Tok.reservedNames = reservedNames,
      Tok.reservedOpNames = reservedOps,
      Tok.caseSensitive = True
    }

lexer :: Tok.GenTokenParser Text () Identity
lexer = Tok.makeTokenParser lexerDef

reserved :: String -> Parser ()
reserved = Tok.reserved lexer

reservedOp :: String -> Parser ()
reservedOp = Tok.reservedOp lexer

identifier :: Parser String
identifier = Tok.identifier lexer

symbol :: String -> Parser ()
symbol str = void $ Tok.symbol lexer str

-- ()
parens :: Parser a -> Parser a
parens = Tok.parens lexer

-- []
brackets :: Parser a -> Parser a
brackets = Tok.brackets lexer

-- sep by ,
commaSep :: Parser a -> Parser [a]
commaSep = Tok.commaSep lexer

commaSep1 :: Parser a -> Parser [a]
commaSep1 = Tok.commaSep1 lexer

capitalized :: Parser String
capitalized = (:) <$> upper <*> many (alphaNum <|> oneOf "_'")

-- not consumption of just after spaces
identifier' :: Parser String
identifier' =
  (:)
    <$> Tok.identStart lexerDef
    <*> many (Tok.identLetter lexerDef)
