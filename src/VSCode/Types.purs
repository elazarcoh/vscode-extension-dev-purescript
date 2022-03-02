module VSCode.Types
  ( ExtensionContext
  , Uri
  , class Disposable
  , dispose
  ) where

import Prelude

class Disposable a where
  dispose :: a -> Unit

foreign import data ExtensionContext :: Type

foreign import data Uri :: Type

