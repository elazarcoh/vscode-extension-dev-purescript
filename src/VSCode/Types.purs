module VSCode.Types where

import Prelude

import Data.Undefined.NoProblem (Opt)

class Disposable a where
  dispose :: a -> Unit

foreign import data ExtensionContext :: Type

foreign import data Uri :: Type

