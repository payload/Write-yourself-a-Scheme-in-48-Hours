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
    val <- many $ noneOf "\""
    char '"'
    return $ String val
    
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
parseNumber =
    liftM (Number . read) $ many1 digit
    
symbol :: Parser Char
symbol = oneOf "!#$%&|*+-/:<=>?@^_~"

spaces :: Parser ()
spaces = skipMany1 space
