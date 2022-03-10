module VSCode.Window
  ( showInformationMessage
  ) where

import Prelude

import Control.Promise (Promise, toAffE)
import Effect (Effect)
import Effect.Aff (Aff, runAff)
import Effect.Aff.Compat (EffectFnAff)
import Effect.Uncurried (EffectFn1, runEffectFn1)

foreign import _showInformationMessage :: String -> Effect (Promise Unit)

showInformationMessage :: String -> Aff Unit
showInformationMessage msg = toAffE $ _showInformationMessage msg

