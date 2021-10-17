module Main where

import Prelude

import Effect (Effect)
import Halogen.Aff (runHalogenAff, awaitBody)
import Halogen.VDom.Driver (runUI)

import App (app)

main :: Effect Unit
main = runHalogenAff do
  body <- awaitBody
  runUI app unit body
