module Question where

import Prelude

import Data.Maybe (Maybe(..), maybe)
import Effect.Aff.Class (class MonadAff)
import Effect.Class (class MonadEffect)
import Effect.Class.Console (logShow)
import Halogen (Component, ComponentHTML, HalogenF(..), HalogenM, raise, Slot, defaultEval, mkComponent, mkEval, modify_)
import Halogen.HTML as HH
import Halogen.HTML.Events as HE
import Halogen.HTML.Properties as HP
import Type.Proxy (Proxy(..))
import Types (QuestionInput)

_question = Proxy :: Proxy "question"

data QuestionOutput = SelectedOption Int Int

data QuestionAction = Receive QuestionInput
                    | SelectOption Int Int
type QuestionState = {
    number :: Int,
    question :: String,
    answer :: Maybe Int,
    gift :: String
}

--button :: forall query output m. H.Component query ButtonInput output m

question :: forall query input output m. MonadEffect m => Component query QuestionInput QuestionOutput m
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
            , HH.div_ [ HH.input [HP.id "op1", HP.type_ HP.InputRadio, HP.name "options", HP.value "1", HE.onChecked  \_ -> SelectOption number 1, HP.checked $ maybe false (\v -> v == 1) answer]
                      , HH.label [HP.for "op1"] [HH.text "POCO"]
                      , HH.input [HP.id "op2", HP.type_ HP.InputRadio, HP.name "options", HP.value "2", HE.onChecked  \_ -> SelectOption number 2, HP.checked $ maybe false (\v -> v == 2) answer]
                      , HH.label [HP.for "op2"] [HH.text "MUY POCO"]
                      , HH.input [HP.id "op3", HP.type_ HP.InputRadio, HP.name "options", HP.value "3", HE.onChecked  \_ -> SelectOption number 3, HP.checked $ maybe false (\v -> v == 3) answer]
                      , HH.label [HP.for "op3"] [HH.text "REGULAR"]
                      , HH.input [HP.id "op4", HP.type_ HP.InputRadio, HP.name "options", HP.value "4", HE.onChecked  \_ -> SelectOption number 4, HP.checked $ maybe false (\v -> v == 4) answer]
                      , HH.label [HP.for "op4"] [HH.text "MUCHO"]
                      , HH.input [HP.id "op5", HP.type_ HP.InputRadio, HP.name "options", HP.value "5", HE.onChecked  \_ -> SelectOption number 5, HP.checked $ maybe false (\v -> v == 5) answer]
                      , HH.label [HP.for "op5"] [HH.text "MUCHÍSIMO"]
                      ]
            ]

    handleAction :: QuestionAction -> HalogenM QuestionState QuestionAction () QuestionOutput m Unit
    handleAction = case _ of
        Receive input ->
            modify_ _ { number = input.number
                      , question = input.question
                      , answer = input.answer
                      , gift = input.gift
                      }
        SelectOption number value ->
            do logShow $ "click " <> show value
               raise (SelectedOption number value)
                                        

