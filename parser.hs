{-# LANGUAGE ExistentialQuantification #-}
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
eval (List [Atom "if", predicate, conseq, alt]) = 
    eval predicate >>= branch
  where
    branch (Bool False) = eval alt
    branch _ = eval conseq
eval (List (Atom func : args)) = mapM eval args >>= apply func
eval badForm = throwError $ BadSpecialForm "Unrecognized special form" badForm

car :: [LispVal] -> ThrowsError LispVal
car [List (x : _)] = pure x
car [DottedList (x : _) _] = pure x
car [badArg] = throwError $ TypeMismatch "pair" badArg
car badArgList = throwError $ NumArgs 1 badArgList
                                 
cdr :: [LispVal] -> ThrowsError LispVal
cdr [List (_ : xs)] = pure $ List xs
cdr [DottedList _ xs] = pure xs
cdr [badArg] = throwError $ TypeMismatch "pair" badArg
cdr badArgList = throwError $ NumArgs 1 badArgList

cons :: [LispVal] -> ThrowsError LispVal
cons [x,List []] = pure $ List [x]
cons [x,List xs] = pure $ List $ x:xs
cons [x,DottedList xs xl] = pure $ DottedList (x:xs) xl
cons [x,y] = pure $ DottedList [x] y
cons badArgList = throwError $ NumArgs 2 badArgList


apply :: String -> [LispVal] -> ThrowsError LispVal
apply func args = maybe (throwError $
                         NotFunction "Unrecognized primitive function args" func)   
  ($ args) (lookup func primitives)

eqv :: [LispVal] -> ThrowsError LispVal
eqv [(Bool arg1), (Bool arg2)] = return $ Bool $ arg1 == arg2
eqv [(Number arg1), (Number arg2)] = return $ Bool $ arg1 == arg2
eqv [(String arg1), (String arg2)] = return $ Bool $ arg1 == arg2
eqv [(Atom arg1), (Atom arg2)] = return $ Bool $ arg1 == arg2
eqv [(DottedList xs x), (DottedList ys y)] = eqv [List $ xs ++ [x], List $ ys ++ [y]]
eqv [(List arg1), (List arg2)] = return $ Bool $ (length arg1 == length arg2) && 
                                                    (all eqvPair $ zip arg1 arg2)
    where eqvPair (x1, x2) = case eqv [x1, x2] of
                               Left err -> False
                               Right (Bool val) -> val
eqv [_, _] = return $ Bool False
eqv badArgList = throwError $ NumArgs 2 badArgList

data Unpacker = forall a. Eq a => AnyUnpacker (LispVal -> ThrowsError a)

unpackEquals :: LispVal -> LispVal -> Unpacker -> ThrowsError Bool
unpackEquals arg1 arg2 (AnyUnpacker unpacker) = do 
  unpacked1 <- unpacker arg1
  unpacked2 <- unpacker arg2
  return $ unpacked1 == unpacked2
 `catchError` (const $ return False)

equal :: [LispVal] -> ThrowsError LispVal
equal [arg1, arg2] = do
    primitiveEquals <- or <$> mapM (unpackEquals arg1 arg2) 
                      [AnyUnpacker unpackNum, AnyUnpacker unpackStr, AnyUnpacker unpackBool]
    Bool eqvEquals <- eqv [arg1, arg2]
    return $ Bool $ (primitiveEquals || eqvEquals)
equal badArgList = throwError $ NumArgs 2 badArgList

primitives :: [(String, [LispVal] -> ThrowsError LispVal)]
primitives = [("+", numericBinop (+)),
              ("-", numericBinop (-)),
              ("*", numericBinop (*)),
              ("/", numericBinop div),
              ("mod", numericBinop mod),
              ("quotient", numericBinop quot),
              ("remainder", numericBinop rem),
              ("=", numBoolBinop (==)),
              ("<", numBoolBinop (<)),
              (">", numBoolBinop (>)),
              ("/=", numBoolBinop (/=)),
              (">=", numBoolBinop (>=)),
              ("<=", numBoolBinop (<=)),
              ("&&", boolBoolBinop (&&)),
              ("||", boolBoolBinop (||)),
              ("string=?", strBoolBinop (==)),
              ("string<?", strBoolBinop (<)),
              ("string>?", strBoolBinop (>)),
              ("string<=?", strBoolBinop (<=)),
              ("string>=?", strBoolBinop (>=)),
              ("car", car),
              ("cdr", cdr),
              ("cons", cons),
              ("eq?", eqv),
              ("eqv?", eqv),
              ("equal?", equal)]

numericBinop :: (Integer -> Integer -> Integer) -> [LispVal] -> ThrowsError LispVal
numericBinop _ singleVal@[_] = throwError $ NumArgs 2 singleVal
numericBinop op params = Number .foldl1 op <$> mapM unpackNum params

boolBinop :: (LispVal -> ThrowsError a) -> (a -> a -> Bool) -> [LispVal] -> ThrowsError LispVal
boolBinop unpacker op (left:right:[]) = 
  Bool <$> (op <$> (unpacker left) <*> (unpacker right))
boolBinop _ _ args = 
  throwError $ NumArgs 2 args
                                     
numBoolBinop = boolBinop unpackNum
strBoolBinop = boolBinop unpackStr
boolBoolBinop = boolBinop unpackBool


unpackNum :: LispVal -> ThrowsError Integer
unpackNum (Number n) = return n
unpackNum (String n) = let parsed = reads n in 
                          if null parsed 
                          then throwError $ TypeMismatch "number" $ String n
                            else return.fst.head $ parsed
unpackNum (List [n]) = unpackNum n
unpackNum notNum = throwError $ TypeMismatch "number" notNum

unpackStr :: LispVal -> ThrowsError String
unpackStr (String s) = pure s
unpackStr (Number s) = pure $ show s
unpackStr (Bool s) = pure $ show s
unpackStr notString = throwError $ TypeMismatch "string" notString

unpackBool :: LispVal -> ThrowsError Bool
unpackBool (Bool b) = pure b
unpackBool notBool = throwError $ TypeMismatch "boolean" notBool

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
