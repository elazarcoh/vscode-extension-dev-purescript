module Main
  ( activateImpl
  , deactivateImpl
  , treeViewAff
  )
  where

import Prelude

import Data.Array (filter)
import Data.Either (Either(..))
import Data.Maybe (Maybe(..))
import Data.Options ((:=))
import Data.Tuple (Tuple(..), uncurry)
import Data.UndefinedOr (fromUndefined)
import Effect (Effect)
import Effect.Aff (Aff, attempt, launchAff_)
import Effect.Class (liftEffect)
import Effect.Class.Console as Console
import Effect.Exception.Unsafe (unsafeThrowException)
import Node.FS.Aff as FSA
import Node.Path (FilePath)
import VSCode.Commands (getCommands, registerCommand)
import VSCode.Common (subscribeDisposable)
import VSCode.TreeView (TreeItemCollapsibleState(..), collapsibleState, createTreeItem, label, registerTreeView)
import VSCode.Types (ExtensionContext, TreeNode(..), TreeItem)
import VSCode.Window (showInformationMessage)

fooMessageCommand ∷ ExtensionContext -> Effect Unit
fooMessageCommand ctx = do
  disposable <- registerCommand "test-purs.helloWorld" (\_ -> showInformationMessage "foo")
  subscribeDisposable ctx disposable

fileToItem :: FilePath -> TreeItem
fileToItem filepath = createTreeItem $ (label := filepath) <> (collapsibleState := Collapsed)

affChild ∷ Aff (Array TreeItem)
affChild = map (map fileToItem) (FSA.readdir ".")

treeViewChildren ∷ TreeNode → Array TreeItem
treeViewChildren Root =
  [ createTreeItem $ mempty
      <> label := "Foo"
      <> collapsibleState := Collapsed
  , createTreeItem $ mempty
      <> label := "Foo2"
      <> collapsibleState := Collapsed
  ]
treeViewChildren (Child x) = case fromUndefined $ _.label x of
  Just "Foo" → pure $ createTreeItem $ mempty
    <> label := "Bar"
    <> collapsibleState := Collapsed
  Just "Bar" → mempty
  _ -> mempty

treeView ∷ Tuple String (TreeNode → Array TreeItem)
treeView = Tuple "nodeDependencies" treeViewChildren

treeViewAff ∷ Tuple String (TreeNode → Aff (Array TreeItem))
treeViewAff = Tuple "nodeDependencies" children
  where
  children ∷ TreeNode → Aff (Array TreeItem)
  children Root = affChild
  children (Child _) = pure mempty

errorLogged ∷ forall a. Aff a → Aff a
errorLogged aff = do
  x <- attempt aff
  case x of
    Left e -> do
      Console.log $ "Error: " <> show e
      unsafeThrowException e
    Right v -> pure v

activateImpl :: ExtensionContext -> Effect Unit
activateImpl ctx =
  launchAff_ $ errorLogged
    $ do
        Console.log "Activating..."
        liftEffect $ (fooMessageCommand ctx)
        Console.log "Registered command"
        commands <- getCommands
        Console.log (show $ filter (eq "test-purs.helloWorld") commands)
        Console.log "Registering tree view..."
        liftEffect $ uncurry registerTreeView treeView >>= subscribeDisposable ctx
        -- liftEffect $ uncurry registerTreeViewAff treeViewAff >>= subscribeDisposable ctx
        Console.log "TreeView registered"
        -- _ <- printDir
        -- x <- affChild
        -- Console.log (show x)
        Console.log "printed"

deactivateImpl :: Effect Unit
deactivateImpl = do
  Console.log "🍜"

