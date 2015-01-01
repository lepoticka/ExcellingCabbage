module EXParser(
    Expression(..),
    evaluate,
    parseArithmetic
) where


import Text.ParserCombinators.Parsec
import Text.ParserCombinators.Parsec.Expr
import Data.Char
import EXData
import Control.Monad
import Data.Either.Unwrap


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
parseArithmetic :: String -> Either ExError Expression
parseArithmetic s 
  | s == "" = Left NoValue
  | isLeft value = Left ParseError
  | otherwise = Right (fromRight value)
 where
    value = parse expr "" $ removeSpace s


evaluate :: Expression -> Either ExError Expression
evaluate expression
  | isRight value = fmap Constant value
  |otherwise = Left $ fromLeft value
 where
   value = evaluateHelper expression

-- Function for evaluation
evaluateHelper :: Expression -> Either ExError Integer
evaluateHelper (Cell _ _) = Left EvaluationError
evaluateHelper (Constant a) = Right a
evaluateHelper (Add a b) = liftM2 (+) (evaluateHelper a) (evaluateHelper b)
evaluateHelper (Sub a b) = liftM2 (-) (evaluateHelper a) (evaluateHelper b)
evaluateHelper (Mult a b) = liftM2 (*) (evaluateHelper a) (evaluateHelper b)
evaluateHelper (Division a b) = liftM2 div (evaluateHelper a) (evaluateHelper b)
