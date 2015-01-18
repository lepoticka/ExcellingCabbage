module EXData(
  Coordinates,
  References,
  FeedbackValue,
  FeedbackValues,
  Expression (..),
  ExError (..)
)where

-- cell coordinates
type Coordinates = (Int, Int)

-- references for other cells in the expression
type References = [Coordinates]

-- information for other cells about current cell
type FeedbackValue = (Coordinates, Maybe Integer)
type FeedbackValues = [FeedbackValue]

-- possible errors in the computation
data ExError = ParseError | EvaluationError | NoValue | ReferenceError

-- structure for cell input
data Expression = Constant Integer
                | Cell Int Int
                | Add Expression Expression
                | Sub Expression Expression
                | Mult Expression Expression
                | Division Expression Expression
                deriving (Show)
