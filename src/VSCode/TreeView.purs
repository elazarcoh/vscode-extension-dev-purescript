module VSCode.TreeView
  ( TreeItem(..)
  , TreeItemCollapsibleState(..)
  , TreeNode(..)
  , TreeView(..)
  , registerTreeView
  , registerTreeViewAff
  ) where

import Prelude

import Control.Promise (Promise, fromAff)
import Data.Maybe (Maybe(..))
import Data.Undefined.NoProblem (Opt, pseudoMap)
import Data.UndefinedOr (UndefinedOr, fromUndefined)
import Effect (Effect)
import Effect.Aff (Aff)
import Effect.Uncurried (EffectFn1, mkEffectFn1)
import Untagged.Union (type (|+|), asOneOf)
import VSCode.Common (class VSCConvertible, disposeImpl, fromVSC, toVSC)
import VSCode.Types (class Disposable)

data TreeView

instance disposeTreeView :: Disposable TreeView where
  dispose = disposeImpl

data TreeItemCollapsibleState = Collapsed | Expanded | None
type VSCTreeItemCollapsibleState = Int

instance vscodeConvertibleTreeItemCollapsibleState :: VSCConvertible TreeItemCollapsibleState VSCTreeItemCollapsibleState where
  toVSC c = case c of
    Collapsed -> 1
    Expanded -> 2
    None -> 0
  fromVSC c = case c of
    1 -> Collapsed
    2 -> Expanded
    0 -> None
    _ -> None

-- TreeView Types

newtype TreeItem = TreeItem
  { label :: Opt String
  , collapsibleState :: Opt TreeItemCollapsibleState
  }

newtype VSCTreeItem = VSCTreeItem
  { label :: Opt String
  , collapsibleState :: Opt VSCTreeItemCollapsibleState
  }

instance treeItemVSCodeConvertible :: VSCConvertible TreeItem VSCTreeItem where
  toVSC (TreeItem c) = VSCTreeItem $ c { collapsibleState = pseudoMap toVSC c.collapsibleState }
  fromVSC (VSCTreeItem c) = TreeItem $ c { collapsibleState = pseudoMap fromVSC c.collapsibleState }

data TreeNode = Root | Child TreeItem

type ForeignTreeItemGetter = (UndefinedOr TreeItem -> Array VSCTreeItem)
  |+| (EffectFn1 (UndefinedOr TreeItem) (Promise (Array VSCTreeItem)))

foreign import _registerTreeView :: String -> ForeignTreeItemGetter -> Effect TreeView

toTreeNode :: UndefinedOr TreeItem -> TreeNode
toTreeNode u = case fromUndefined u of
  Nothing -> Root
  Just x -> (Child x)

registerTreeViewAff :: String -> (TreeNode -> Aff (Array TreeItem)) -> Effect TreeView
registerTreeViewAff id children = _registerTreeView id $ asOneOf (mkEffectFn1 asPromiseEff)
  where
  asPromiseEff :: UndefinedOr TreeItem -> Effect (Promise (Array VSCTreeItem))
  asPromiseEff item = fromAff $ (vscodeChildren <<< toTreeNode) item

  vscodeChildren :: TreeNode -> Aff (Array VSCTreeItem)
  vscodeChildren = (map <<< map <<< map) toVSC children

registerTreeView :: String -> (TreeNode -> Array TreeItem) -> Effect TreeView
registerTreeView id children = _registerTreeView id $ asOneOf (vscodeChildren <<< toTreeNode)
  where
  vscodeChildren :: TreeNode -> Array VSCTreeItem
  vscodeChildren = (map <<< map) toVSC children

