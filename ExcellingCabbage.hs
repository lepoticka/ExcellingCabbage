module ExcellingCabbage(
    Expression(..),
    evaluate,
    processParse,
    parseArithmetic
) where


import Text.ParserCombinators.Parsec
import Text.ParserCombinators.Parsec.Expr
import Text.Parsec.Error

-- Expression algebraic structure for representing arithmetic expressions
data Expression = Constant Integer
                | Add Expression Expression
                | Sub Expression Expression
                | Mult Expression Expression
                | Division Expression Expression
                deriving (Show)
                -- | Reference Coordinate


table = [ [op "*" Mult AssocLeft, op "/" Division AssocLeft], [op "+" Add AssocLeft, op "-" Sub AssocLeft]]
        where
            op s f assoc = Infix( do {string s; return f; }) assoc

-- bracket parser
factor = do {char '('; x <- expr ; char ')'; return x}
            <|> number

-- number parser
number :: Parser Expression
number = do { 
                ds <- many1 digit;
                return (Constant . read $ ds)
            }

-- Parser for arithmetic expressions
expr :: Parser Expression
expr = buildExpressionParser table factor

-- function for parsing
parseArithmetic :: String -> Either Text.Parsec.Error.ParseError Expression
parseArithmetic = parse expr ""

processParse :: Either Text.Parsec.Error.ParseError Expression -> Expression
processParse (Left _) = Constant 0
processParse (Right a) = a

-- Function for evaluation
evaluate :: Expression -> Integer
evaluate (Add a b) = evaluate a + evaluate b
evaluate (Sub a b) = evaluate a - evaluate b
evaluate (Mult a b) = evaluate a * evaluate b
evaluate (Division a b) = evaluate a `div` evaluate b
evaluate (Constant a) = a
