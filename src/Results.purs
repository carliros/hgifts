module Results where

import Prelude

import Data.Array (groupAllBy, index, length, mapWithIndex)
import Data.Maybe (Maybe(..), maybe)
import Data.Ord.Down (Down)
import Effect.Aff.Class (class MonadAff)
import Halogen (Component, ComponentHTML, HalogenM, Slot, mkEval, mkComponent, defaultEval, modify_)
import Halogen.HTML as HH
import Halogen.HTML.Events as HE
import Halogen.HTML.Properties as HP
import Type.Proxy (Proxy(..))
import Types (QuestionInput)

_results = Proxy :: Proxy "results"

data ResultsAction = Receive ResultsInput
type ResultsInput = {
    name :: String,
    questions :: Array QuestionInput,
    page :: Int
}
type ResultsState = {
    name :: String,
    questions :: Array QuestionInput,
    page :: Int
}

results :: forall query output m. Component query ResultsInput output m
results =
  mkComponent
    { initialState
    , render
    , eval: mkEval $ defaultEval
        { handleAction = handleAction
        , receive = Just <<< Receive
        }
    }
  where
    initialState :: ResultsInput -> ResultsState
    initialState intro = intro

    compareQuestions q1 q2 = compare q1.gift q2.gift

    render :: ResultsState -> ComponentHTML ResultsAction () m
    render { name, questions, page } =
        let groups = groupAllBy (compareQuestions) questions
        in HH.div_
            [ HH.h3_ [ HH.text "Resultados" ]
            , HH.p_ [ HH.text $ show groups]
            ]

    handleAction :: ResultsAction -> HalogenM ResultsState ResultsAction () output m Unit
    handleAction = case _ of
        Receive input ->
            modify_ _ { name = input.name }
