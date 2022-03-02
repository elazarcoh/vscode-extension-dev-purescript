module VSCode.Common
  ( class VSCConvertible
  , toVSC
  , fromVSC
  , disposeImpl
  , subscribeDisposable
  ) where

import Prelude

import Effect (Effect)
import VSCode.Types (class Disposable, ExtensionContext)

foreign import disposeImpl :: forall a. a -> Unit
foreign import subscribeDisposable :: forall a. Disposable a => ExtensionContext -> a -> Effect Unit

class VSCConvertible a b where
  toVSC :: a -> b
  fromVSC :: b -> a
