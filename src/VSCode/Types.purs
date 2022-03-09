module VSCode.Types
  ( ExtensionContext
  , Uri
  , class Disposable
  , dispose
  , class Register
  , register
  ) where

import Prelude

import Effect (Effect)

class Disposable a where
  dispose :: a -> Unit

class (Disposable t) <= Register t requirements | t -> requirements where
  register :: String -> requirements -> Effect t

foreign import data ExtensionContext :: Type

foreign import data Uri :: Type

