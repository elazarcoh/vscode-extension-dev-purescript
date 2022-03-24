module Extension
  ( activateImpl
  , deactivateImpl
  ) where

import Prelude

import Effect (Effect)
import Effect.Console as Console
import VSCode.Types (ExtensionContext)

activateImpl :: ExtensionContext -> Effect Unit
activateImpl ctx = do
  Console.log "activated"

deactivateImpl :: Effect Unit
deactivateImpl = do
  Console.log "deactivated"
