module Parser where

import Text.ParserCombinators.Parsec hiding (spaces)

data LispVal
  = Atom String
  | List [LispVal]
  | DottedList [LispVal] LispVal
  | Number Integer
  | String String
  | Boolean Bool
  deriving (Show, Eq)

regularParse :: Parser a -> String -> Either ParseError a
regularParse p = parse p "(unknown)"

symbol :: Parser Char
symbol = oneOf "!$%&|*+-/:<=>?@~_^#"

spaces :: Parser ()
spaces = skipMany1 space

parseExpr :: Parser LispVal
parseExpr =
  choice
    [ parseAtom,
      parseStr,
      parseNumber,
      parseQuoted,
      char '(' *> choice [try parseList, parseDotted] <* char ')'
    ]

parseAtom :: Parser LispVal
parseAtom = do
  first <- choice [letter, symbol]
  rest <- many $ choice [letter, digit, symbol]
  let atom = first : rest
  return $ case atom of
    "#t" -> Boolean True
    "#f" -> Boolean False
    _ -> Atom atom

parseStr :: Parser LispVal
parseStr = String <$> (char '\"' *> (many . noneOf) "\"" <* char '\"')

parseNumber :: Parser LispVal
parseNumber = Number . read <$> many1 digit

parseList :: Parser LispVal
parseList = List <$> sepBy parseExpr spaces

parseDotted :: Parser LispVal
parseDotted =
  DottedList <$> endBy parseExpr spaces <*> (char '.' >> spaces >> parseExpr)

parseQuoted :: Parser LispVal
parseQuoted = (\x -> List [Atom "quote", x]) <$> (char '\'' *> parseExpr)
