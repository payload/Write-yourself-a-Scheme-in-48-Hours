module Main where
import System.Environment
import Text.ParserCombinators.Parsec hiding (spaces)
import Control.Monad
import Control.Monad.Error

data LispVal =
    Atom String |
    List [LispVal] |
    DottedList [LispVal] LispVal |
    Number Integer |
    String String |
    Bool Bool

instance Show LispVal where
    show = showVal
    
data LispError =
    NumArgs Integer [LispVal] |
    TypeMismatch String LispVal |
    Parser ParseError |
    BadSpecialForm String LispVal |
    NotFunction String String |
    UnboundVar String String |
    Default String

instance Show LispError where
    show = showErr
instance Error LispError where
    noMsg = Default "An error has occured"
    strMsg = Default
    
type ThrowsError = Either LispError

trapError action = catchError action (return . show)

extractValue :: ThrowsError a -> a
extractValue (Right val) = val

main = do
    args <- getArgs
    evaled <- return $ liftM show ((readExpr $ head args) >>= eval)
    putStrLn $ extractValue $ trapError evaled

eval :: LispVal -> ThrowsError LispVal
eval val@(Number _) = return val
eval val@(String _) = return val
eval val@(Bool _) = return val
eval (List [Atom "quote", val]) = return val

eval (List [Atom "if", cond, conseq, alt]) = do
    result <- eval cond
    case result of
        Bool False -> eval alt
        otherwise  -> eval conseq

eval (List (Atom func : args)) = mapM eval args >>= apply func
eval badForm = throwError $ BadSpecialForm "Unrecognized special form" badForm

apply :: String -> [LispVal] -> ThrowsError LispVal
apply func args = maybe
    (throwError $ NotFunction "Dont know that function" func)
    ($ args)
    (lookup func primitives)
    
primitives :: [(String, [LispVal] -> ThrowsError LispVal)]
primitives = [
    ("-", numericBinop (-)),
    ("+", numericBinop (+)),
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
    ("eqv", eqv)]

car :: [LispVal] -> ThrowsError LispVal
car [List (x:_)] = return x
car [DottedList (x:_) _] = return x
car [x] = throwError $ TypeMismatch "List" x
car args@(_:_) = throwError $ NumArgs 1 args

cdr :: [LispVal] -> ThrowsError LispVal
cdr [List (_:xs)] = return $ List xs
cdr [DottedList [_] x] = return x
cdr [DottedList (_:xs) x] = return $ DottedList xs x
cdr [x] = throwError $ TypeMismatch "List" x
cdr args@(_:_) = throwError $ NumArgs 1 args

cons :: [LispVal] -> ThrowsError LispVal
cons [x,List []] = return $ List [x]
cons [x,List xs] = return $ List $ x:xs
cons [x,DottedList xs end] = return $ DottedList (x:xs) end
cons [a,b] = return $ DottedList [a] b
cons args = throwError $ NumArgs 2 args

eqv :: [LispVal] -> ThrowsError LispVal
eqv [Bool a, Bool b] = return $ Bool $ a == b
eqv [String a, String b] = return $ Bool $ a == b
eqv [Number a, Number b] = return $ Bool $ a == b
eqv [Atom a, Atom b] = return $ Bool $ a == b
eqv [DottedList as a, DottedList bs b] =
    eqv [(List $ as ++ [a]), (List $ bs ++ [b])]
eqv [List as, List bs] =
    return $ Bool $
        all eqvPair $ zip as bs
    where
        eqvPair (a,b) = case eqv [a, b] of
            Left err -> False
            Right (Bool val) -> val
eqv [_,_] = return $ Bool False
eqv args = throwError $ NumArgs 2 args

numericBinop ::
    (Integer -> Integer -> Integer) ->
    [LispVal] ->
    ThrowsError LispVal
numericBinop op x@[_] = throwError $ NumArgs 2 x
numericBinop op args = do
    nums <- mapM unpackNumber args
    return $ Number $ foldl1 op nums

boolBinop :: 
    (LispVal -> ThrowsError a) -> 
    (a -> a -> Bool) -> 
    [LispVal] -> 
    ThrowsError LispVal
boolBinop unpacker op args =
    if length args /= 2
    then throwError $ NumArgs 2 args
    else do
        a <- unpacker $ args !! 0
        b <- unpacker $ args !! 1
        return $ Bool $ a `op` b

numBoolBinop = boolBinop unpackNumber
boolBoolBinop = boolBinop unpackBool
strBoolBinop = boolBinop unpackString

unpackNumber :: LispVal -> ThrowsError Integer
unpackNumber (Number n) = return n
unpackNumber (List [n]) = unpackNumber n
unpackNumber val = throwError $ TypeMismatch "Number" val

unpackBool :: LispVal -> ThrowsError Bool
unpackBool (Bool b) = return b
unpackBool val = throwError $ TypeMismatch "Bool" val

unpackString :: LispVal -> ThrowsError String
unpackString (String s) = return s
unpackString val = throwError $ TypeMismatch "String" val
    
showErr :: LispError -> String
showErr (UnboundVar msg var) =
    msg ++ ": " ++ var
showErr (BadSpecialForm msg form) =
    msg ++ ": " ++ show form
showErr (NotFunction msg func) =
    msg ++ ": " ++ func
showErr (NumArgs expected found) =
    "Expected " ++ show expected ++ "args; found vals " ++ showVals found
showErr (TypeMismatch expected found) =
    "Expected type " ++ expected ++ ", found " ++ show found
showErr (Parser parseErr) =
    "Parse error at " ++ show parseErr

showVal :: LispVal -> String
showVal (Atom name) = name
showVal (Number x) = show x
showVal (String s) = show s
showVal (Bool True) = "#t"
showVal (Bool False) = "#f"
showVal (List list) =
    "(" ++ showVals list ++ ")"
showVal (DottedList front end) =
    "(" ++ showVals front ++ " . " ++ showVal end ++ ")"
showVals = unwords . map showVal

readExpr :: String -> ThrowsError LispVal
readExpr input =
    case parse parseExpr "lisp" input of
    Left err -> throwError $ Parser err
    Right val -> return val

parseExpr :: Parser LispVal
parseExpr =
    parseAtom <|>
    parseNumber <|>
    parseString <|>
    parseQuoted <|>
    parseParenthized

parseParenthized :: Parser LispVal
parseParenthized = do
    char '('
    list <- try parseList <|> parseDottedList
    char ')'
    return list

parseList :: Parser LispVal
parseList =
    liftM List $ sepBy parseExpr spaces
    
parseDottedList :: Parser LispVal
parseDottedList = do
    front <- endBy parseExpr spaces
    end <- char '.' >> spaces >> parseExpr
    return $ DottedList front end
    
parseQuoted :: Parser LispVal
parseQuoted = do
    char '\''
    x <- parseExpr
    return $ List [Atom "quote", x]

parseString :: Parser LispVal
parseString = do
    char '"'
    val <- many parseInString
    char '"'
    return $ String val
    
parseInString :: Parser Char
parseInString = do
    c <- noneOf "\""
    case c of
        '\\' -> parseEscapeInString
        _    -> return c
        
parseEscapeInString :: Parser Char
parseEscapeInString = do
    c <- oneOf "\"\\ntr"
    return $ case c of
        '"' -> '"'
        'n' -> '\n'
        't' -> '\t'
        'r' -> '\r'
        '\\' -> '\\'
    
parseAtom :: Parser LispVal
parseAtom = do
    first <- letter <|> symbol
    rest  <- many $ letter <|> symbol <|> digit
    let atom = first:rest
    return $ case atom of
        "#t" -> Bool True
        "#f" -> Bool False
        _    -> Atom atom
        
parseNumber :: Parser LispVal
parseNumber = do
    str <- many1 digit
    return $ Number $ read str
    
symbol :: Parser Char
symbol = oneOf "!#$%&|*+-/:<=>?@^_~"

spaces :: Parser ()
spaces = skipMany1 space
