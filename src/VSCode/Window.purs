module VSCode.Window where

import Prelude

import Effect (Effect)

foreign import showInformationMessage :: String -> Effect Unit

