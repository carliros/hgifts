module App where

import Prelude

import Data.Maybe (Maybe(..))
import Effect.Aff as Aff
import Effect.Aff.Class (class MonadAff)
import Halogen (Component, ComponentHTML, HalogenM, mkEval, mkComponent, defaultEval, modify_)
import Halogen.HTML as HH
import Halogen.HTML.Events as HE
import Question (Slots, _question, question)

type AppState = { count :: Int }
data AppAction = Increment

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
  initialState _ = { count: 0 }

  render :: AppState -> ComponentHTML AppAction Slots m
  render { count } =
    HH.div_ [ HH.slot_ _question unit question { number: 1, question: "Question A", answer: Just 2 } ]

  handleAction :: AppAction -> HalogenM AppState AppAction Slots output m Unit
  handleAction = case _ of
    Increment -> modify_ \st -> st { count = st.count + 1 }