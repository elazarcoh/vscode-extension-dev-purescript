module VSCode.Commands
  ( Command(..)
  , executeCommand
  , getCommands
  )
  where

import Prelude

import Control.Promise (Promise, fromAff, toAff)
import Effect (Effect)
import Effect.Aff (Aff)
import Effect.Class (liftEffect)
import Effect.Uncurried (EffectFn1, mkEffectFn1)
import Foreign (Foreign)
import Untagged.Union (UndefinedOr)
import VSCode.Common (disposeImpl)
import VSCode.Types (class Disposable, class Register)

data Command

instance Disposable Command where
  dispose = disposeImpl

foreign import _registerCommand :: forall a. String -> EffectFn1 (Foreign) (Promise a) -> Effect Command


instance Register Command (Foreign → Aff a) where
  register id act = _registerCommand id (mkEffectFn1 $ fromAff <<< act)

foreign import _getCommands :: Effect (Promise (Array String))

getCommands :: Aff (Array String)
getCommands = liftEffect _getCommands >>= toAff

foreign import _executeCommand :: forall a. String -> Array Foreign -> Effect (Promise a)

executeCommand :: forall a. String → Array Foreign → Aff a
executeCommand id args =  liftEffect (_executeCommand id args) >>= toAff
