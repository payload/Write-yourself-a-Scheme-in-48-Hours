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
    deriving Show

main = do
    args <- getArgs
    putStrLn $ readExpr $ args !! 0

readExpr :: String -> String
readExpr input =
    case parse parseExpr "lisp" input of
    Left err -> "No match: " ++ show err
    Right val -> show val

parseExpr :: Parser LispVal
parseExpr =
    parseAtom <|>
    parseNumber <|>
    parseString

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
