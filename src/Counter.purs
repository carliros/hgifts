module Counter where

import Prelude
import Halogen (Component, mkEval, mkComponent, defaultEval, modify_)
import Halogen.HTML as HH
import Halogen.HTML.Events as HE

data Action = Increment | Decrement

initialState _ = 0

render state =
    HH.div_
      [ HH.button [ HE.onClick \_ -> Decrement ] [ HH.text "-" ]
      , HH.span_ [ HH.text $ show state ]
      , HH.button [ HE.onClick \_ -> Increment ] [ HH.text "+" ]
      ]

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
