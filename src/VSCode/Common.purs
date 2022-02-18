module VSCode.Common
  ( disposeImpl
  , subscribeDisposable
  ) where

import Prelude

import Effect (Effect)
import VSCode.Types (class Disposable, ExtensionContext)

foreign import disposeImpl :: forall a. a -> Unit
foreign import subscribeDisposable :: forall a. Disposable a => a -> ExtensionContext -> Effect Unit

