module VSCode.Types where

import Prelude

class Disposable a where
  dispose :: a -> Unit

