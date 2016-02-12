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

readExpr :: String -> LispVal
readExpr input =
    case parse parseExpr "lisp" input of
      Left err -> String $ "No match: " ++ show err
      Right val -> val

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

showVal :: LispVal -> String
showVal (String str) = "\"" ++ str ++ "\""
showVal (Atom name) = name
showVal (Number num) = show num
showVal (Bool True) = "#t"
showVal (Bool False) = "#f"
showVal (List li) = "(" ++ unwordsList li ++ ")"
showVal (DottedList hd tl) = "(" ++ unwordsList hd ++ " . " ++ showVal tl ++ ")"

unwordsList :: [LispVal] -> String
unwordsList = unwords.map showVal

instance Show LispVal where show = showVal

eval :: LispVal -> LispVal
eval val@(String _) = val
eval val@(Number _) = val
eval val@(Bool _) = val
eval (List [Atom "quote", val]) = val
eval (List (Atom func : args)) = apply func $ map eval args
                                 
apply :: String -> [LispVal] -> LispVal
apply func args = maybe (Bool False) ($ args) $ lookup func primitives

primitives :: [(String, [LispVal] -> LispVal)]
primitives = [("+", numericBinop (+)),
              ("-", numericBinop (-)),
              ("*", numericBinop (*)),
              ("/", numericBinop div),
              ("mod", numericBinop mod),
              ("quotient", numericBinop quot),
              ("remainder", numericBinop rem)]

numericBinop :: (Integer -> Integer -> Integer) -> [LispVal] -> LispVal
numericBinop op params = Number $ foldl1 op $ map unpackNum params

unpackNum :: LispVal -> Integer
unpackNum (Number n) = n
unpackNum (String n) = let parsed = reads n in 
                          if null parsed 
                            then 0
                            else fst.head $ parsed
unpackNum (List [n]) = unpackNum n
unpackNum _ = 0


main =
    show <$> eval <$> readExpr <$> head <$> getArgs >>= putStrLn
