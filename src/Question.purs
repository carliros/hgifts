module Question where

import Prelude

import Data.Maybe (Maybe(..), maybe)
import Effect.Aff.Class (class MonadAff)
import Halogen (Component, ComponentHTML, HalogenM, Slot, mkEval, mkComponent, defaultEval, modify_)
import Halogen.HTML as HH
import Halogen.HTML.Events as HE
import Halogen.HTML.Properties as HP
import Type.Proxy (Proxy(..))
import Types (QuestionOption)

type Slots = ( question :: forall query. Slot query Void Unit )
_question = Proxy :: Proxy "question"

data QuestionAction = Receive QuestionInput
type QuestionInput = {
    number :: Int,
    question :: String,
    answer :: Maybe Int
 }
type QuestionState = {
    number :: Int,
    question :: String,
    answer :: Maybe Int
}

--button :: forall query output m. H.Component query ButtonInput output m

question :: forall query output m. Component query QuestionInput output m
question =
  mkComponent
    { initialState
    , render
    , eval: mkEval $ defaultEval
        { handleAction = handleAction
        , receive = Just <<< Receive
        }
    }
  where
    initialState :: QuestionInput -> QuestionState
    initialState question = question

    render :: QuestionState -> ComponentHTML QuestionAction () m
    render { number, question, answer } =
        HH.div_
            [ HH.span [] [ HH.text $ show number <> ". "]
            , HH.span [] [HH.text question]
            , HH.div_ [ HH.input [HP.id "op1", HP.type_ HP.InputRadio, HP.name "options", HP.value "1", HP.checked $ maybe false (\v -> v == 1) answer]
                      , HH.label [HP.for "op1"] [HH.text "POCO"]
                      , HH.input [HP.id "op2", HP.type_ HP.InputRadio, HP.name "options", HP.value "2", HP.checked $ maybe false (\v -> v == 2) answer]
                      , HH.label [HP.for "op2"] [HH.text "MUY POCO"]
                      , HH.input [HP.id "op3", HP.type_ HP.InputRadio, HP.name "options", HP.value "3", HP.checked $ maybe false (\v -> v == 3) answer]
                      , HH.label [HP.for "op3"] [HH.text "REGULAR"]
                      , HH.input [HP.id "op4", HP.type_ HP.InputRadio, HP.name "options", HP.value "4", HP.checked $ maybe false (\v -> v == 4) answer]
                      , HH.label [HP.for "op4"] [HH.text "MUCHO"]
                      , HH.input [HP.id "op5", HP.type_ HP.InputRadio, HP.name "options", HP.value "5", HP.checked $ maybe false (\v -> v == 5) answer]
                      , HH.label [HP.for "op5"] [HH.text "MUCHÍSIMO"]
                      ]
            ]

    handleAction :: QuestionAction -> HalogenM QuestionState QuestionAction () output m Unit
    handleAction = case _ of
        Receive input ->
            modify_ _ { number = input.number
                      , question = input.question
                      , answer = input.answer
                      }