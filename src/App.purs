module App where

import Prelude

import Data.Array (index, length, mapWithIndex)
import Data.Maybe (Maybe(..), fromJust)
import Effect.Aff as Aff
import Effect.Aff.Class (class MonadAff)
import Effect.Class (class MonadEffect)
import Effect.Class.Console (logShow)
import Halogen (Component, ComponentHTML, HalogenM, Slot, mkEval, mkComponent, defaultEval, modify_)
import Halogen.HTML as HH
import Halogen.HTML.Events as HE
import Halogen.HTML.Properties as HP
import Intro (_intro, intro)
import Question (QuestionOutput(..), _question, question)
import Results (_results, results)
import Types (QuestionInput)

type AppSlots = (
      question :: forall query. Slot query QuestionOutput Int
    , intro :: forall query. Slot query Void Unit
    , results :: forall query. Slot query Void Unit
    )

type AppState = {
      page :: Int
    , questions :: Array QuestionInput
    , name :: String
    }
data AppAction 
    = Next
    | Previous
    | HandleSelectedOption QuestionOutput

app :: forall query input output m. MonadEffect m => Component query input output m
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
        page: 0
      , questions: [
            { number: 1, gift: "Administrar", answer: Nothing, question: "Question A" }
          , { number: 2, gift: "Animar", answer: Nothing, question: "Question B" }
          , { number: 3, gift: "Administrar", answer: Nothing, question: "Question C" }
          , { number: 4, gift: "Animar", answer: Nothing, question: "Question D" }
          , { number: 5, gift: "Administrar", answer: Nothing, question: "Question E" }
          , { number: 6, gift: "Animar", answer: Nothing, question: "Question F" }
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
                                            Just questionInput -> HH.div_ [ HH.slot _question page question questionInput HandleSelectedOption ]
                                            Nothing -> HH.text "Error"
                                else HH.div_ [ HH.slot_ _results unit results { name: name, questions: questions, page: page }]
    in HH.div_ [
          HH.h2 [] [HH.text "Test de dones espirituales"]
        , bodyContent
        , HH.br_
        , HH.div [] [
              HH.button [ HE.onClick \_ -> Previous, HP.disabled (page == 0)] [ HH.text "Anterior" ]
            , HH.button [ HE.onClick \_ -> Next, HP.disabled (page == (1 + length questions)) ] [ HH.text "Siguiente" ]]
        , HH.p_ [HH.text $ show questions]
        ]

  handleAction :: AppAction -> HalogenM AppState AppAction AppSlots output m Unit
  handleAction = case _ of
    Next -> modify_ \st -> st { page = st.page + 1 }
    Previous -> modify_ \st -> st { page = st.page - 1 }
    HandleSelectedOption (SelectedOption nro value) -> do
        logShow $ "got " <> show nro <> " " <> show value
        modify_ \st -> let newQuestions = mapWithIndex (\_ q -> if nro == q.number then q {answer = Just value} else q) st.questions
                       in st { questions = newQuestions, page = st.page + 1 }
