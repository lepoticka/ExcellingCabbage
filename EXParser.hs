{-# OPTIONS_HADDOCK ignore-exports #-}

{-| In module EXParser we export two main functions(evaluate and parseArithmetic) through which we come to the parsed expression, that is from input data-string to the output-some data structure like parse tree. 
-}

module EXParser(
    evaluate,
    parseArithmetic
) where


import Text.ParserCombinators.Parsec
import Text.ParserCombinators.Parsec.Expr
import Data.Char
import EXData
import Control.Monad
import Data.Either.Unwrap


-- | table indicates possible operators. 
table :: OperatorTable Char () Expression
-- OperatorTable takes 3 arguments: Char, (), Expression
table = [ [op "*" Mult AssocLeft, op "/" Division AssocLeft], [op "+" Add AssocLeft, op "-" Sub AssocLeft]]
-- parser supports operations as *, /, +, -. 
        where
            op s f = Infix(string s >> return f)
            -- ">>" means bind in monads

-- | factor is parser for expression in combination of brackets.
factor :: Parser Expression
factor = do { 
          _ <- char '('; 
          x <- expr ; 
          _ <- char ')'; 
          return x
        } <|> number
          <|> cell
          -- "<|>" is a parser combinator; the idea of using that sign is to combine the small parsers in to big ones

-- | number is parser for numbers.
number :: Parser Expression
number = do { 
                ds <- many1 digit;
                return (Constant . read $ ds)
            }

-- | cell is for parsing notation for referencing other cells.
cell :: Parser Expression
cell = do {
         c <- lower;
         y <- many1 digit;
         -- digit and many1 are functions in library Parsec
         return (Cell (ord c - ord 'a' + 1) (read y))
         -- read convertes to y from a string
       }
      
-- | Function removeSpace is removing spaces.
removeSpace :: String -> String
removeSpace [] = []
removeSpace (x:xs)
  | isSpace x = removeSpace xs
  | otherwise = x: removeSpace xs


-- | expr is parser for arithmetic expressions. 
expr :: Parser Expression
-- it is a parser that returns an expression
expr = buildExpressionParser table factor
-- buildExpressionParser is a function in library parsec

-- | parseArithmetic is function for parsing, that is taking the actual string and returning type Either
parseArithmetic :: String                    -- ^ as string we can take anything, meaning we can type in anything
                -> Either ExError Expression -- ^ arithmetic expression don't return ExError
parseArithmetic s 
  | s == "" = Left NoValue
  | isLeft value = Left ParseError
  | otherwise = Right (fromRight value)
 where
    value = parse expr "" $ removeSpace s
-- parse is a function, that actually parses our entered strings, that are arithmetic expressions in cell
-- so with predefined parser types we help ourselves to define the most important function, that is parser

-- | This function evaluates expressions and then returns error or constant.
evaluate :: Expression -> Either ExError Expression
-- we change type for easy calculation
evaluate expression
  | isRight value = fmap Constant value
  |otherwise = Left $ fromLeft value
 where
   value = evaluateHelper expression

-- | evaluateHelper is function that helps us evaluate expression.
evaluateHelper :: Expression -> Either ExError Integer
evaluateHelper (Cell _ _) = Left EvaluationError
evaluateHelper (Constant a) = Right a
evaluateHelper (Add a b) = liftM2 (+) (evaluateHelper a) (evaluateHelper b)
evaluateHelper (Sub a b) = liftM2 (-) (evaluateHelper a) (evaluateHelper b)
evaluateHelper (Mult a b) = liftM2 (*) (evaluateHelper a) (evaluateHelper b)
evaluateHelper (Division a b) = liftM2 div (evaluateHelper a) (evaluateHelper b)
-- we use function liftM2(Control.Monad library) to promote a function evaluateHelper to a monad, scanning the monadic arguments from left to right
