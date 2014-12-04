import Text.ParserCombinators.Parsec
import Text.ParserCombinators.Parsec.Expr
import Data.Char
-- import Data.Maybe
-- import Graphics.UI.WX hiding (Event)
-- import Reactive.Banana
-- import Reactive.Banana.WX
data Expression = Const Integer
                -- | Reference Coordinate
                | Add Expression Expression
                | Sub Expression Expression
                | Mult Expression Expression
                | Division Expression Expression
                deriving (Show)

-- expr = buildExpressionParser table (fmap (Const . toInteger. digitToInt) digit)
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
                return (Const . read $ ds)
            }

-- Test: parseTest expr "2+3*5"
--
-- main :: IO ()
-- main = start $ do
--     f           <- frame [ text := "Arithmetic"]
--     input1      <- entry f []
--     input2      <- entry f []
--     output      <- staticText f []
--
--     set f [layout := margin IO $ row IO
--             [widget input1, label "+" , widget input2
--             , label "=", minsize (sz 40 20) $ widget output ]]
--
--     let networkDescription :: forall t. Frameworks t => Moment t ()
--         networkDescription = do
--
--         binput1 <- behaviorText input1 ""
--         binput2 <- behaviorText input2 ""
--
--         let 
--             result :: Behavior t (Maybe Int)
--             result = f <$> binput1 <*> binput2
--                 where
--                 f x y = liftA2 (+) (readNumber x) (readNumber y)
--
--             readNumber s = listToMaybe [x | (x, "") <- reads s]
--             showNumber   = maybe "--" show
--
--         sink output [text :== showNumber <$> result]
--
--     network <- compile networkDescription
--     actuate network
