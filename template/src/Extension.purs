module Extension
  ( activateImpl
  , deactivateImpl
  )
  where

import Prelude

activateImpl :: ExtensionContext -> Effect Unit
activateImpl ctx = do
    Console.log "activated"

deactivateImpl :: Effect Unit
deactivateImpl = do
  Console.log "deactivated"

