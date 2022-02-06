module VSCode.Commands where

import Prelude

import Control.Promise (Promise, toAff)
import Effect (Effect)
import Effect.Aff (Aff)
import Effect.Class (liftEffect)
import Foreign (Foreign)
import VSCode.Common (disposeImpl)
import VSCode.Types (class Disposable)

data Command = Command

instance disposeCommand :: Disposable Command where
  dispose = disposeImpl


foreign import _registerCommand :: String -> (Array Foreign -> Effect Unit) -> Command
registerCommand :: String -> (Array Foreign -> Effect Unit) -> Effect Command
registerCommand s fn = pure $ _registerCommand s fn

foreign import _getCommands :: Effect (Promise (Array String))
getCommands :: Aff (Array String)
getCommands = liftEffect _getCommands >>= toAff

-- foreign import executeCommand :: String -> Array Foreign ->  a
-- foreign import getCommands :: Effect a
