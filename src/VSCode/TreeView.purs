module VSCode.TreeView
  ( TreeElement(..)
  , TreeItem(..)
  , TreeItemCollapsibleState(..)
  , TreeView(..)
  , makeTreeItem
  )
  where

import Prelude

import Control.Promise (Promise, fromAff)
import Data.Identity (Identity)
import Data.Maybe (Maybe(..))
import Data.Undefined.NoProblem (Opt, pseudoMap)
import Data.Undefined.NoProblem.Open as Open
import Data.UndefinedOr (UndefinedOr, fromUndefined)
import Effect (Effect)
import Effect.Aff (Aff)
import Effect.Uncurried (EffectFn1, mkEffectFn1)
import Untagged.Union (type (|+|), asOneOf)
import VSCode.Common (class VSCConvertible, disposeImpl, fromVSC, toVSC)
import VSCode.Types (class Disposable, class Register, Uri)

data TreeView :: forall k. (k -> Type) -> k -> Type
data TreeView f a

instance disposeTreeView :: Disposable (TreeView f a) where
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

newtype TreeItem = TreeItem
  { label :: Opt String
  , collapsibleState :: Opt TreeItemCollapsibleState
  , resourceUri :: Opt Uri
  }

makeTreeItem = TreeItem <<< Open.coerce

newtype VSCTreeItem = VSCTreeItem
  { label :: Opt String
  , collapsibleState :: Opt VSCTreeItemCollapsibleState
  , resourceUri :: Opt Uri
  }

instance treeItemVSCodeConvertible :: VSCConvertible TreeItem VSCTreeItem where
  toVSC (TreeItem c) = VSCTreeItem $ c { collapsibleState = pseudoMap toVSC c.collapsibleState }
  fromVSC (VSCTreeItem c) = TreeItem $ c { collapsibleState = pseudoMap fromVSC c.collapsibleState }

data TreeElement a = Root | Child a

type ForeignTreeElementGetter a = (UndefinedOr a -> Array a)
  |+| (EffectFn1 (UndefinedOr a) (Promise (Array a)))

type ForeignTreeElementToVSCTreeItem a = (a -> VSCTreeItem)
  |+| (EffectFn1 a (Promise VSCTreeItem))

foreign import _registerTreeView :: forall f a. String -> ForeignTreeElementGetter a -> ForeignTreeElementToVSCTreeItem a -> Effect (TreeView f a)

toTreeElement :: forall a. UndefinedOr a -> TreeElement a
toTreeElement u = case fromUndefined u of
  Nothing -> Root
  Just x -> Child x

instance registerTreeView :: Register (TreeView Identity a) { children :: TreeElement a -> Array a, resolve :: a -> TreeItem } where
  register id { children, resolve } = _registerTreeView id (asOneOf $ children <<< toTreeElement) (asOneOf resolveAsVSC)
    where
      resolveAsVSC :: a -> VSCTreeItem
      resolveAsVSC = toVSC <<< resolve
else instance registerTreeViewAff :: Register (TreeView Aff a) { children :: TreeElement a -> Aff (Array a), resolve :: a -> Aff TreeItem } where
  register id { children, resolve } = _registerTreeView id (asOneOf $ mkEffectFn1 childredPromised) (asOneOf $ mkEffectFn1 resolvePromised)
    where
    childredPromised :: UndefinedOr a -> Effect (Promise (Array a))
    childredPromised i = fromAff $ (children <<< toTreeElement) i
    resolvePromised :: a -> Effect (Promise VSCTreeItem)
    resolvePromised i = fromAff $ (map toVSC $ resolve i)