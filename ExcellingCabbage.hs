module ExcellingCabbage(
    Expression(..),
    expr,
    evaluate,
    parse
) where


import Text.ParserCombinators.Parsec
import Text.ParserCombinators.Parsec.Expr
-- import Data.Char

data Expression = Constant Integer
                | Add Expression Expression
                | Sub Expression Expression
                | Mult Expression Expression
                | Division Expression Expression
                deriving (Show)
                -- | Reference Coordinate

expr :: Parser Expression
expr = buildExpressionParser table factor

table = [ [op "*" Mult AssocLeft, op "/" Division AssocLeft], [op "+" Add AssocLeft, op "-" Sub AssocLeft]]
        where
            op s f assoc = Infix( do {string s; return f; }) assoc

factor = do {char '('; x <- expr ; char ')'; return x}
            <|> number

number :: Parser Expression
number = do { 
                ds <- many1 digit;
                return (Constant . read $ ds)
            }

evaluate :: Expression -> Integer
evaluate (Add a b) = (evaluate a) + (evaluate b)
evaluate (Sub a b) = (evaluate a) - (evaluate b)
evaluate (Mult a b) = (evaluate a) * (evaluate b)
evaluate (Division a b) = (evaluate a) `div` (evaluate b)
evaluate (Constant a) = a
