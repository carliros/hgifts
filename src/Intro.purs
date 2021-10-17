module Intro where

import Prelude

import Data.Maybe (Maybe(..), maybe)
import Effect.Aff.Class (class MonadAff)
import Halogen (Component, ComponentHTML, HalogenM, Slot, mkEval, mkComponent, defaultEval, modify_)
import Halogen.HTML as HH
import Halogen.HTML.Events as HE
import Halogen.HTML.Properties as HP
import Type.Proxy (Proxy(..))

_intro = Proxy :: Proxy "intro"

data IntroAction = Receive IntroInput
type IntroInput = {
    name :: String
}
type IntroState = {
    name :: String
}

intro :: forall query output m. Component query IntroInput output m
intro =
  mkComponent
    { initialState
    , render
    , eval: mkEval $ defaultEval
        { handleAction = handleAction
        , receive = Just <<< Receive
        }
    }
  where
    initialState :: IntroInput -> IntroState
    initialState intro = intro

    render :: IntroState -> ComponentHTML IntroAction () m
    render { name } =
        HH.div_
            [ HH.p_ [ HH.text "Querido hermano(a):" ]
            , HH.p_ [ HH.text "Esta herramienta[1] es para examinar qué dones tenemos, o podemos desarrollar o cuáles dones no tenemos. Consiste de 66 preguntas que recaban información sobre nuestras capacidades, habilidades, gustos, etc. por medio de expresiones o afirmaciones que nos permiten evaluarlas de acuerdo a nuestras vidas."]
            , HH.p_ [ HH.text "En cada pregunta debes responder el grado de verdad que cada frase tiene para usted seleccionando una de las opciones siguientes:"]
            , HH.p_ [ HH.text "1. MUY POCO\n2. POCO\n3. REGULAR\n4. MUCHO\n5. MUCHÍSIMO"]
            , HH.p_ [ HH.text "Una vez que finalice la encuesta, los resultados serán procesados y se darán a conocer los resultados posteriormente al finalizar la reunión."]
            , HH.p_ [ HH.text "Por favor, presione continuar para realizar la encuesta."]
            , HH.p_ [ HH.text "[1]: Este cuestionario fue recabado de la sección de recursos de la Iglesia Metodista Libre (https://iml-latinoamerica.org/materialpic/). Es utilizado sin permiso y solamente para propósitos de conocer los dones espirituales de los participantes del Programa de Discipulado \"Ahora que Soy Miembro\" de la Iglesia Evangélica Bautista \"Nueva Vida\". "]
            ]

    handleAction :: IntroAction -> HalogenM IntroState IntroAction () output m Unit
    handleAction = case _ of
        Receive input ->
            modify_ _ { name = input.name }
