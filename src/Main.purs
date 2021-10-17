module Main where

import Prelude

import Effect (Effect)
import Halogen.Aff (runHalogenAff, awaitBody)
import Halogen.VDom.Driver (runUI)

import Counter (counterComponent)

main :: Effect Unit
main = runHalogenAff do
  body <- awaitBody
  runUI counterComponent unit body
