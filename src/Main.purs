module Main
  ( activateImpl
  , deactivateImpl
  , treeView
  )
  where

import Prelude

import Control.Promise (Promise, toAff)
import Data.Array (filter, head, toUnfoldable, (!!))
import Data.Either (Either(..))
import Data.Identity (Identity)
import Data.List (List(..))
import Data.Maybe (Maybe(..), fromMaybe, maybe)
import Data.String (joinWith)
import Data.Undefined.NoProblem (toMaybe, undefined)
import Data.Undefined.NoProblem.Open as Open
import Effect (Effect)
import Effect.Aff (Aff, attempt, forkAff, joinFiber, launchAff, launchAff_, supervise)
import Effect.Aff.Class (liftAff)
import Effect.Class (liftEffect)
import Effect.Console as Console
import Effect.Exception.Unsafe (unsafeThrowException)
import Foreign (Foreign, unsafeFromForeign, unsafeToForeign)
import Literals.Undefined (Undefined)
import Node.FS.Aff as FSA
import Node.FS.Stats as FS
import Node.Path (FilePath, concat)
import Unsafe.Coerce (unsafeCoerce)
import Untagged.Union (UndefinedOr, defined, fromUndefinedOr, maybeToUor, uorToMaybe)
import VSCode.Commands (Command, executeCommand, getCommands)
import VSCode.Common (subscribeDisposable)
import VSCode.TreeView (TreeElement(..), TreeItem(..), TreeItemCollapsibleState(..), TreeView, makeTreeItem)
import VSCode.Types (ExtensionContext, register)
import VSCode.Uri as Uri
import VSCode.Window (MessageItem(..), messageItem, showInformationMessage, showInformationMessage')

treeViewChildren :: TreeElement TreeItem -> Array TreeItem
treeViewChildren Root =
  [ makeTreeItem { label: "Foo", collapsibleState: Expanded }
  , makeTreeItem { label: "Foo2", collapsibleState: Collapsed }
  ]
treeViewChildren (Child (TreeItem parent)) = case toMaybe $ parent.label of
  Just "Foo" -> [ makeTreeItem { label: "Bar", collapsibleState: None } ]
  Just "Bar" -> mempty
  _ -> mempty

treeView :: { children :: TreeElement TreeItem -> Array TreeItem, resolve :: TreeItem -> TreeItem }
treeView = { children: treeViewChildren, resolve: identity }

type File = { directory :: FilePath, name :: String }

treeViewAff :: { children :: TreeElement File -> Aff (Array File), resolve :: File -> Aff TreeItem }
treeViewAff = { children: children, resolve: fileToItem }
  where
  root = "C:/elazar/private/vscode_extenstions/test-purs"

  fullPath :: File -> FilePath
  fullPath { directory, name } = concat [ directory, name ]

  readdir :: FilePath -> Aff (Array File)
  readdir dir = (map <<< map) (\name -> { directory: dir, name: name }) $ FSA.readdir dir

  children :: TreeElement File -> Aff (Array File)
  children Root = readdir root
  children (Child file) = do
    let fp = fullPath file
    stat <- FSA.stat fp
    if FS.isDirectory stat then readdir fp
    else pure mempty

  fileToItem :: File -> Aff TreeItem
  fileToItem file = do
    let fp = fullPath file
    stat <- FSA.stat fp
    if FS.isDirectory stat then pure $ makeTreeItem { resourceUri: Uri.fromFile fp, collapsibleState: Collapsed }
    else pure $ makeTreeItem { resourceUri: Uri.fromFile fp, collapsibleState: None }

errorLogged :: forall a. Aff a -> Aff a
errorLogged aff = do
  x <- attempt aff
  case x of
    Left e -> do
      liftEffect $ Console.log $ "Error: " <> show e
      unsafeThrowException e
    Right v -> pure v

showWelcome :: String -> Aff String
showWelcome msg = do
  showInformationMessage' msg [ messageItem { title: "Foo" } ]
  pure msg

activateImpl :: ExtensionContext -> Effect Unit
activateImpl ctx =
  do
    Console.log "Activating..." 
    (register "test-purs.helloWorld" (\msg -> showWelcome $ fromUndefinedOr "Default" (unsafeFromForeign msg)) :: Effect Command) >>= subscribeDisposable ctx

    Console.log "Registered command"
    launchAff_ do
      commands <- getCommands
      liftEffect $ Console.log (show $ filter (eq "test-purs.helloWorld") commands)

    -- Console.log "Registering tree view..."
    -- (register "nodeDependencies" treeView :: Effect (TreeView Identity TreeItem)) >>= subscribeDisposable ctx
    -- (register "nodeDependencies" treeViewAff :: Effect (TreeView Aff File)) >>= subscribeDisposable ctx
    -- Console.log "TreeView registered"

    launchAff_  $ errorLogged $ supervise do
      fb1 <- forkAff $ executeCommand "test-purs.helloWorld" $ [unsafeToForeign "MyMessage"]
      -- fb2 <- forkAff $ executeCommand "test-purs.helloWorld" $ []
      z <- joinFiber fb1 :: Aff String
      liftEffect $ Console.log $ "fb1: " <> show z
      pure unit
    
    Console.log "activated"

deactivateImpl :: Effect Unit
deactivateImpl = do
  Console.log "🍜"

