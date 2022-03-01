module VSCode.TreeView
  ( TreeItemCollapsibleState(..)
  , TreeView(..)
  , collapsibleState
  , createTreeItem
  , label
  , registerTreeView
  , registerTreeViewAff
  )
  where

import Prelude

import Control.Promise (Promise, fromAff)
import Data.Functor.Contravariant (cmap)
import Data.Maybe (Maybe(..))
import Data.Options (Option, Options, opt, options)
import Data.UndefinedOr (UndefinedOr, fromUndefined)
import Effect (Effect)
import Effect.Aff (Aff)
import Effect.Uncurried (EffectFn1, mkEffectFn1)
import Foreign (Foreign)
import Untagged.Union (type (|+|), asOneOf)
import VSCode.Common (disposeImpl)
import VSCode.Types (class Disposable, TreeNode(..), TreeItem)

data TreeView

instance disposeTreeView :: Disposable TreeView where
  dispose = disposeImpl

label :: Option TreeItem String
label = opt "label"

data TreeItemCollapsibleState
  = Collapsed
  | Expanded
  | None

treeItemCollapsibleStateToVSCodeEnum :: TreeItemCollapsibleState -> Int
treeItemCollapsibleStateToVSCodeEnum Collapsed = 1

treeItemCollapsibleStateToVSCodeEnum Expanded = 2

treeItemCollapsibleStateToVSCodeEnum None = 0

collapsibleState :: Option TreeItem TreeItemCollapsibleState
collapsibleState = cmap treeItemCollapsibleStateToVSCodeEnum (opt "collapsibleState")

foreign import createTreeItemImpl :: Foreign -> TreeItem

createTreeItem :: Options TreeItem -> TreeItem
createTreeItem opts = createTreeItemImpl (options opts)

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

