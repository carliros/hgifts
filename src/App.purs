module App where

import Prelude

import Data.Array (index, length)
import Data.Maybe (Maybe(..), fromJust)
import Effect.Aff as Aff
import Effect.Aff.Class (class MonadAff)
import Halogen (Component, ComponentHTML, HalogenM, Slot, mkEval, mkComponent, defaultEval, modify_)
import Halogen.HTML as HH
import Halogen.HTML.Events as HE
import Halogen.HTML.Properties as HP
import Intro (_intro, intro)
import Question (_question, question)
import Types (QuestionInput)
import Web.HTML.Event.EventTypes (offline)

type AppSlots = (
      question :: forall query. Slot query Void Unit
    , intro :: forall query. Slot query Void Unit
    )

type AppState = {
      page :: Int
    , questions :: Array QuestionInput
    , name :: String
    }
data AppAction = Next | Previous

app :: forall query input output m. MonadAff m => Component query input output m
app =
  mkComponent
    { initialState
    , render
    , eval: mkEval $ defaultEval
        { handleAction = handleAction }
    }
  where
  initialState :: input -> AppState
  initialState _ = {
        page: 3
      , questions: [
            { number: 1, question: "Question A", answer: Nothing }
          , { number: 2, question: "Question b", answer: Nothing }
          , { number: 3, question: "Question c", answer: Nothing }
          , { number: 4, question: "Question d", answer: Nothing }
          ]
      , name: ""
    }

  render :: AppState -> ComponentHTML AppAction AppSlots m
  render { page, questions, name } =
    let bodyContent = if page == 0
                        then HH.div_ [ HH.slot_ _intro unit intro { name: name }]
                        else if page >= 1 && page <= length questions
                                then let maybeQuestionInput = index questions (page - 1)
                                     in case maybeQuestionInput of
                                            Just questionInput -> HH.div_ [ HH.slot_ _question unit question questionInput ]
                                            Nothing -> HH.text "Error"
                                else HH.text "END PAGE"
    in HH.div_ [
          HH.h2 [] [HH.text "Test de dones espirituales"]
        , bodyContent
        , HH.br_
        , HH.div [] [
              HH.button [ HE.onClick \_ -> Previous, HP.disabled (page == 0)] [ HH.text "Anterior" ]
            , HH.button [ HE.onClick \_ -> Next, HP.disabled (page == (2 + length questions)) ] [ HH.text "Siguiente" ]]
        ]

  handleAction :: AppAction -> HalogenM AppState AppAction AppSlots output m Unit
  handleAction = case _ of
    Next -> modify_ \st -> st { page = st.page + 1 }
    Previous -> modify_ \st -> st { page = st.page - 1 }
