module VSCode.Uri
  ( fromFile
  ) where

import Prelude
import VSCode.Types (Uri)

foreign import fromFile :: String -> Uri
