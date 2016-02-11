import Text.ParserCombinators.Parsec hiding (spaces)
import System.Environment

data LispVal = Atom String
             | List [LispVal]
             | DottedList [LispVal] LispVal
             | Number Integer
             | String String
             | Bool Bool

symbol :: Parser Char
symbol = oneOf "!#$%&|*+-/:<=>?@^_~"

readExpr :: String -> String
readExpr input =
    case parse parseExpr "lisp" input of
      Left err -> "No match: " ++ show err
      Right _ -> "Found value"

spaces :: Parser ()
spaces = skipMany1 space

parseString :: Parser LispVal
parseString = 
  char '"' >> do
    x <- many (noneOf "\"")
    char '"'
    return $ String x

parseAtom ::Parser LispVal
parseAtom = do
  first <- (letter <|> symbol)
  rest <- many (letter <|> digit <|> symbol)
  let atom = first:rest
  return $ case atom of
             "#t" -> Bool True
             "#f" -> Bool False
             _  -> Atom atom

parseNumber :: Parser LispVal
parseNumber = (Number . read) <$> many1 digit

parseExpr :: Parser LispVal
parseExpr = parseAtom
            <|> parseString
            <|> parseNumber
            <|> parseQuoted
            <|> (char '(' >>
                 ((try parseList <|> parseDottedList) >>= \x ->
                 char ')' >>
                 pure x))

parseList :: Parser LispVal
parseList = List <$> sepBy parseExpr spaces

parseDottedList :: Parser LispVal
parseDottedList =
    DottedList <$> endBy parseExpr spaces <*>
                   (char '.' >> spaces >> parseExpr)

parseQuoted :: Parser LispVal
parseQuoted =
    char '\'' >> do 
      x <- parseExpr
      return $ List [Atom "quote", x]

main =
    readExpr <$> head <$> getArgs >>= putStrLn
