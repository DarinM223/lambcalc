module Lambcalc.Parser (parseLam) where

import Control.Monad.Combinators.Expr
import Data.Void (Void)
import Lambcalc.Lam (Exp (..))
import Lambcalc.Shared (Bop (..))
import Text.Megaparsec
import Text.Megaparsec.Char
import qualified Text.Megaparsec.Char.Lexer as L

type Parser = Parsec Void String

sc :: Parser ()
sc = L.space space1  (L.skipLineComment "--") empty

lexeme :: Parser a -> Parser a
lexeme = L.lexeme sc

symbol :: String -> Parser String
symbol = L.symbol sc

integer :: Parser Int
integer = lexeme L.decimal

keyword :: String -> Parser String
keyword word = lexeme (string word <* notFollowedBy alphaNumChar)

identifier :: Parser String
identifier = lexeme (letters >>= check)
 where
  letters = (:) <$> letterChar <*> many (alphaNumChar <|> char '_')
  check s
    | s `elem` rws = fail $ "keyword " ++ show s ++ " cannot be an identifier"
    | otherwise    = return s

rws :: [String]
rws = ["if", "then", "else", "fn"]

parens :: Parser a -> Parser a
parens = between (char '(') (char ')')

expr :: Parser Exp
expr = makeExprParser term operatorTable

term :: Parser Exp
term = try $ lexeme $ choice
  [ parens expr
  , If <$> (keyword "if" *> expr) <*> (keyword "then" *> expr) <*> (keyword "else" *> expr)
  , Lam <$> (keyword "fn" *> identifier) <*> (symbol "=>" *> expr)
  , Var <$> identifier
  , Int <$> integer
  ]

operatorTable :: [[Operator Parser Exp]]
operatorTable =
  [ [ binary "" App]
  , [ binary "*" (Bop Mul) ]
  , [ binary "+" (Bop Add)
    , binary "-" (Bop Sub)
    ]
  , [ binary "<<" (Bop Shl)
    , binary ">>" (Bop Lshr)
    , binary ">>>" (Bop Ashr)
    ]
  , [ binary "&" (Bop And) ]
  , [ binary "^" (Bop Xor) ]
  , [ binary "|" (Bop Or) ]
  ]

binary :: String -> (Exp -> Exp -> Exp) -> Operator Parser Exp
binary s f = InfixL (f <$ symbol s)

parseLam :: String -> Exp
parseLam s = case runParser (expr <* eof) "" s of
  Right e  -> e
  Left err -> error $ errorBundlePretty err