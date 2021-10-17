module Types where
import Data.Maybe(Maybe)

data QuestionOption 
    = VERY_LITTLE
    | LITTLE
    | REGULAR
    | A_LOT
    | VERY_MANY

type QuestionInput = {
    number :: Int,
    question :: String,
    answer :: Maybe Int
 }
