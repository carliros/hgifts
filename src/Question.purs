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
            [ HH.span [HP.classes [HH.ClassName "text-black"]] [ HH.text $ show number <> ". "]
            , HH.span [] [HH.text question]
            , HH.div [HP.classes [HH.ClassName "p-5"]]
                [ radioBtn number answer 1 "POCO"
                , radioBtn number answer 2 "MUY POCO"
                , radioBtn number answer 3 "REGULAR"
                , radioBtn number answer 4 "MUCHO"
                , radioBtn number answer 4 "MUCHÍSIMO"
                ]
            ]

    radioBtn nro ans value label = 
        let name = "op" <> show value
        in HH.div_ [
              HH.input [HP.id name, HP.type_ HP.InputRadio, HP.name "options", HP.value (show value), HE.onChecked  \_ -> SelectOption nro value, HP.checked $ maybe false (\v -> v == value) ans]
            , HH.label [HP.for name] [HH.text label]
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
                                        

