module VSCode.Common
  ( class FromVSC
  , class ToVSC
  , class VSCConvertible
  , disposeImpl
  , fromVSC
  , subscribeDisposable
  , toVSC
  )
  where

import Prelude

import Effect (Effect)
import VSCode.Types (class Disposable, ExtensionContext)

foreign import disposeImpl :: forall a. a -> Unit
foreign import subscribeDisposable :: forall a. Disposable a => ExtensionContext -> a -> Effect Unit

class ToVSC a b | a -> b, b -> a where
  toVSC :: a -> b

class FromVSC a b | a -> b, b -> a where
  fromVSC :: b -> a

class (ToVSC a b, FromVSC a b) <= VSCConvertible a b 
