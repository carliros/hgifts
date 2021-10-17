module Counter where

import Prelude
import Halogen (Component, HalogenM, mkEval, mkComponent, defaultEval, modify_)
import Halogen.HTML as HH
import Halogen.HTML.Events as HE

data Action = Increment | Decrement
type State = Int

initialState :: forall input. input -> State
initialState _ = 0

render :: forall m. State -> HH.ComponentHTML Action () m
render state =
    HH.div_
      [ HH.button [ HE.onClick \_ -> Decrement ] [ HH.text "-" ]
      , HH.span_ [ HH.text $ show state ]
      , HH.button [ HE.onClick \_ -> Increment ] [ HH.text "+" ]
      ]

handler :: forall output m. Action -> HalogenM State Action () output m Unit
handler = case _ of
    Increment -> modify_ \state -> state + 1
    Decrement -> modify_ \state -> state - 1

counterComponent :: forall query input output m. Component query input output m
counterComponent =
  mkComponent
    { initialState
    , render
    , eval: mkEval $ defaultEval { handleAction = handler }
    }
