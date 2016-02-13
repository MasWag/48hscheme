import Text.ParserCombinators.Parsec hiding (spaces)
import System.Environment
import Control.Monad.Error

data LispVal = Atom String
             | List [LispVal]
             | DottedList [LispVal] LispVal
             | Number Integer
             | String String
             | Bool Bool

symbol :: Parser Char
symbol = oneOf "!#$%&|*+-/:<=>?@^_~"

readExpr :: String -> ThrowsError LispVal
readExpr input =
    case parse parseExpr "lisp" input of
      Left err -> throwError $ Parser err
      Right val -> return val

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

eval :: LispVal -> ThrowsError LispVal
eval val@(String _) = pure val
eval val@(Number _) = pure val
eval val@(Bool _) = pure val
eval (List [Atom "quote", val]) = pure val
eval (List (Atom func : args)) = mapM eval args >>= apply func
eval badForm = throwError $ BadSpecialForm "Unrecognized special form" badForm
                                 
apply :: String -> [LispVal] -> ThrowsError LispVal
apply func args = maybe (throwError $
                         NotFunction "Unrecognized primitive function args" func)   
  ($ args) (lookup func primitives)

primitives :: [(String, [LispVal] -> ThrowsError LispVal)]
primitives = [("+", numericBinop (+)),
              ("-", numericBinop (-)),
              ("*", numericBinop (*)),
              ("/", numericBinop div),
              ("mod", numericBinop mod),
              ("quotient", numericBinop quot),
              ("remainder", numericBinop rem)]

numericBinop :: (Integer -> Integer -> Integer) -> [LispVal] -> ThrowsError LispVal
numericBinop op singleVal@[_] = throwError $ NumArgs 2 singleVal
numericBinop op params = Number .foldl1 op <$> mapM unpackNum params

unpackNum :: LispVal -> ThrowsError Integer
unpackNum (Number n) = return n
unpackNum (String n) = let parsed = reads n in 
                          if null parsed 
                          then throwError $ TypeMismatch "number" $ String n
                            else return.fst.head $ parsed
unpackNum (List [n]) = unpackNum n
unpackNum notNum = throwError $ TypeMismatch "number" notNum

data LispError = NumArgs Integer [LispVal]
               | TypeMismatch String LispVal
               | Parser ParseError
               | BadSpecialForm String LispVal
               | NotFunction String String
               | UnboundVar String String
               | Default String

showError :: LispError -> String
showError (UnboundVar message varname) = message ++ ": " ++ varname
showError (BadSpecialForm message form) = message ++ ": " ++ show form
showError (NotFunction message func) = message ++ ": " ++ show func
showError (NumArgs expected found) = "Expected " ++ show expected 
                                  ++ " args; found values " ++ unwordsList found
showError (TypeMismatch expected found) = "Invalid type: expected " ++ expected
                                       ++ ", found " ++ show found
showError (Parser parseErr) = "Parse error at " ++ show parseErr

instance Show LispError where show = showError

instance Error LispError where
     noMsg = Default "An error has occurred"
     strMsg = Default

type ThrowsError = Either LispError

trapError action = catchError action (return . show)
extractValue :: ThrowsError a -> a
extractValue (Right val) = val


main = do 
  arg <- head <$> getArgs
  evaled <- return $ liftM show $ readExpr arg >>= eval
  putStrLn $ extractValue $ trapError $ evaled 

-- main = do 
--   evaled <- return <$> liftM show <$> readExpr <$> head <$> getArgs >>= eval
--   (extractValue <$> trapError <$> evaled) >>= putStrLn
