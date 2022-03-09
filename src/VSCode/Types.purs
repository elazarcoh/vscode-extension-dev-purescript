module VSCode.Types
  ( ExtensionContext
  , Uri
  , class Disposable
  , dispose
  , class Registerable
  , register
  ) where

import Prelude

import Effect (Effect)

class Disposable a where
  dispose :: a -> Unit

class (Disposable t) <= Registerable t requirements | t -> requirements where
  register :: requirements -> String -> Effect t

foreign import data ExtensionContext :: Type

foreign import data Uri :: Type

