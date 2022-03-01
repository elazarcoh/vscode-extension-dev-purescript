module VSCode.TreeView
  ( Collapsed(..)
  , Expanded(..)
  , None(..)
  , TreeItem
  , TreeItemCollapsibleState(..)
  , TreeNode(..)
  , TreeView(..)
  , collapsed
  , expanded
  , none
  , registerTreeView
  , registerTreeViewAff
  ) where

import Prelude

import Control.Promise (Promise, fromAff)
import Data.Maybe (Maybe(..))
import Data.Undefined.NoProblem (Opt)
import Data.UndefinedOr (UndefinedOr, fromUndefined)
import Effect (Effect)
import Effect.Aff (Aff)
import Effect.Uncurried (EffectFn1, mkEffectFn1)
import Literals (IntLit, intLit)
import Untagged.Union (type (|+|), asOneOf)
import VSCode.Commands (Command)
import VSCode.Common (disposeImpl)
import VSCode.Types (class Disposable)

data TreeView

instance disposeTreeView :: Disposable TreeView where
  dispose = disposeImpl

type Collapsed = IntLit "1"
type Expanded = IntLit "2"
type None = IntLit "0"

collapsed :: TreeItemCollapsibleState
collapsed = asOneOf (intLit :: IntLit "1")

expanded :: TreeItemCollapsibleState
expanded = asOneOf (intLit :: IntLit "2")

none :: TreeItemCollapsibleState
none = asOneOf (intLit :: IntLit "0")

type TreeItemCollapsibleState = Collapsed |+| Expanded |+| None

-- TreeView Types
type TreeItem =
  { label :: Opt String
  , command :: Opt Command
  , collapsibleState :: Opt TreeItemCollapsibleState
  }

data TreeNode = Root | Child TreeItem

type ForeignTreeItemGetter = (UndefinedOr TreeItem -> Array TreeItem)
  |+| (EffectFn1 (UndefinedOr TreeItem) (Promise (Array TreeItem)))

foreign import _registerTreeView :: String -> ForeignTreeItemGetter -> Effect TreeView

toTreeNode :: UndefinedOr TreeItem -> TreeNode
toTreeNode u = case fromUndefined u of
  Nothing -> Root
  Just x -> (Child x)

registerTreeViewAff :: String -> (TreeNode -> Aff (Array TreeItem)) -> Effect TreeView
registerTreeViewAff id children = _registerTreeView id $ asOneOf (mkEffectFn1 asPromiseEff)
  where
  asPromiseEff :: UndefinedOr TreeItem -> Effect (Promise (Array TreeItem))
  asPromiseEff item = fromAff $ (children <<< toTreeNode) item

registerTreeView :: String -> (TreeNode -> Array TreeItem) -> Effect TreeView
registerTreeView id children = _registerTreeView id $ asOneOf (children <<< toTreeNode)

