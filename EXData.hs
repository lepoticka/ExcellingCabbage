{-| In module EXData we introduce two new data types(EXError, Expression) and describe what some existing types represents in our functions, so we are making synonims for already existing types.
-}

module EXData(
  Coordinates,
  References,
  FeedbackValue,
  FeedbackValues,
  Expression (..),
  ExError (..)
)where

-- | The type of Coordinates(Coordinates represent cell coordinates) is (Int, Int).
type Coordinates = (Int, Int)

-- | The type of References, which are references for other cells in the expression, is [(Int, Int)].
type References = [Coordinates]

-- | Type FeedbackValue holds information about one cell.
type FeedbackValue = (Coordinates, Maybe Integer)
-- | FeedbackValues hold information about one or more cells.
type FeedbackValues = [FeedbackValue]

-- | ExError type can have a value of ParseError or EvaluationError or NoValue or ReferenceError.
data ExError = ParseError | EvaluationError | NoValue | ReferenceError
--  We define new data type ExError which tells us what are possible errors in the computation.
-- ParseError occurs through a process of parsing, meaning, that error will occur, when we parse something that is not an arithmetic expression(=izraz).
-- Internal error(=interna) is an error on which we do not have influence. 
-- NoValue is an error that occurs when we are in an empty cell and we push Enter.           
-- ReferenceError occurs when we refer on a cell that is empty.

-- | Expression type gives us possible structures for cell input.
data Expression = Constant Integer
                | Cell Int Int
                | Add Expression Expression
                | Sub Expression Expression
                | Mult Expression Expression
                | Division Expression Expression
                deriving (Show)
-- These possible structures are result of parsed strings.
