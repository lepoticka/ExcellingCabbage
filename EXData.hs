module EXData(
  Coordinates,
  References,
  FeedbackValue,
  FeedbackValues,
  Expression (..),
  ExError (..)
)where

type Coordinates = (Int, Int)
type References = [Coordinates]
type FeedbackValue = (Coordinates, Integer)
type FeedbackValues = [FeedbackValue]
data ExError = ParseError | EvaluationError | NoValue

data Expression = Constant Integer
                | Cell Int Int
                | Add Expression Expression
                | Sub Expression Expression
                | Mult Expression Expression
                | Division Expression Expression
                deriving (Show)


