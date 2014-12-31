module EXParser(
    Expression(..),
    evaluate,
    processParse,
    parseArithmetic
) where


import Text.ParserCombinators.Parsec
import Text.ParserCombinators.Parsec.Expr
import Text.Parsec.Error
import Data.Char
import EXData
import Control.Monad


table :: OperatorTable Char () Expression
table = [ [op "*" Mult AssocLeft, op "/" Division AssocLeft], [op "+" Add AssocLeft, op "-" Sub AssocLeft]]
        where
            op s f assoc = Infix( do {_ <- string s; return f;} ) assoc

-- bracket parser
factor :: Parser Expression
factor = do { 
          _ <- char '('; 
          x <- expr ; 
          _ <- char ')'; 
          return x
        } <|> number
          <|> cell

-- number parser
number :: Parser Expression
number = do { 
                ds <- many1 digit;
                return (Constant . read $ ds)
            }

-- cell notation parser
cell :: Parser Expression
cell = do {
         c <- lower;
         y <- many1 digit;
         return (Cell (ord c - ord 'a' + 1) (read y))
       }

-- Space removal
removeSpace :: String -> String
removeSpace [] = []
removeSpace (x:xs)
  | isSpace x = removeSpace xs
  | otherwise = x: removeSpace xs


-- Parser for arithmetic expressions
expr :: Parser Expression
expr = buildExpressionParser table factor


-- function for parsing
parseArithmetic :: String -> Either Text.Parsec.Error.ParseError Expression
parseArithmetic = parse expr "" . removeSpace

processParse :: Either Text.Parsec.Error.ParseError Expression -> Either ExError Expression
processParse (Left _) = Left ParseError
processParse (Right a) = Right a

-- Function for evaluation
evaluate :: Expression -> Either ExError Integer
evaluate (Cell _ _) = Left EvaluationError
evaluate (Constant a) = Right a
evaluate (Add a b) = liftM2 (+) (evaluate a) (evaluate b)
evaluate (Sub a b) = liftM2 (-) (evaluate a) (evaluate b)
evaluate (Mult a b) = liftM2 (*) (evaluate a) (evaluate b)
evaluate (Division a b) = liftM2 div (evaluate a) (evaluate b)
