module Main where
import System.Environment
import Text.ParserCombinators.Parsec hiding (spaces)
import Control.Monad

data LispVal =
    Atom String |
    List [LispVal] |
    DottedList [LispVal] LispVal |
    Number Integer |
    String String |
    Bool Bool

instance Show LispVal where
    show = showVal

main = do
    args <- getArgs
    print $ eval $ readExpr $ head args

eval :: LispVal -> LispVal
eval (List [Atom "quote", val]) = val
eval (List (Atom func : args)) = apply func $ map eval args
eval val = val

apply :: String -> [LispVal] -> LispVal
apply func args =
    maybe (Bool False) ($ args) $ lookup func primitives
    
primitives :: [(String, [LispVal] -> LispVal)]
primitives = [
    ("+", numericBinop (+)),
    ("-", numericBinop (-)),
    ("*", numericBinop (*)),
    ("/", numericBinop div),
    ("mod", numericBinop mod),
    ("quotient", numericBinop quot),
    ("remainder", numericBinop rem)
    ]

numericBinop op args =
    Number $ foldl1 op $ map unpackNumber args 

unpackNumber (Number n) = n
unpackNumber (List [n]) = unpackNumber n
unpackNumber (String s) =
    let parsed = reads s in
    if null parsed
    then 0
    else fst $ head parsed
unpackNumber _ = 0

showVal :: LispVal -> String
showVal (Atom name) = name
showVal (Number x) = show x
showVal (String s) = show s
showVal (Bool True) = "#t"
showVal (Bool False) = "#f"
showVal (List list) =
    "(" ++ showVals list ++ ")"
showVal (DottedList front end) =
    "(" ++ showVals front ++ showVal end ++ ")"
showVals = unwords . map showVal

readExpr :: String -> LispVal
readExpr input =
    case parse parseExpr "lisp" input of
    Left err -> String $ "No match: " ++ show err
    Right val -> val

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
