module VSCode.Types where

import Prelude

import Data.UndefinedOr (UndefinedOr)

class Disposable a where
  dispose :: a -> Unit

foreign import data ExtensionContext :: Type

foreign import data Uri :: Type

-- TreeView Types
type TreeItem = { label :: UndefinedOr String }
data TreeNode = Root | Child TreeItem
