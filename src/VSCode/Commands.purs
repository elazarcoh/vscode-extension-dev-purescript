module VSCode.Commands
  ( Command(..)
  , getCommands
  , registerCommand
  ) where

import Prelude

import Control.Promise (Promise, toAff)
import Effect (Effect)
import Effect.Aff (Aff)
import Effect.Class (liftEffect)
import Foreign (Foreign)
import VSCode.Common (disposeImpl)
import VSCode.Types (class Disposable)

data Command

instance disposeCommand :: Disposable Command where
  dispose = disposeImpl

foreign import _registerCommand :: String -> (Array Foreign -> Effect Unit) -> Effect Command

registerCommand ∷ String → (Array Foreign → Effect Unit) → Effect Command
registerCommand = _registerCommand

foreign import _getCommands :: Effect (Promise (Array String))

getCommands :: Aff (Array String)
getCommands = liftEffect _getCommands >>= toAff

-- foreign import executeCommand :: String -> Array Foreign ->  a
