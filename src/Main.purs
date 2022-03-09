module Main
  ( activateImpl
  , deactivateImpl
  , treeView
  ) where

import Prelude

import Data.Array (filter)
import Data.Either (Either(..))
import Data.Identity (Identity)
import Data.Maybe (Maybe(..))
import Data.Undefined.NoProblem (toMaybe)
import Data.Undefined.NoProblem.Open as Open
import Effect (Effect)
import Effect.Aff (Aff, attempt, launchAff_)
import Effect.Class (liftEffect)
import Effect.Class.Console as Console
import Effect.Exception.Unsafe (unsafeThrowException)
import Node.FS.Aff as FSA
import Node.FS.Stats as FS
import Node.Path (FilePath, concat)
import VSCode.Commands (Command, getCommands)
import VSCode.Common (subscribeDisposable)
import VSCode.TreeView (TreeElement(..), TreeItem(..), TreeItemCollapsibleState(..), TreeView)
import VSCode.Types (ExtensionContext, register)
import VSCode.Window (showInformationMessage)

treeViewChildren :: TreeElement TreeItem -> Array TreeItem
treeViewChildren Root =
  [ TreeItem $ Open.coerce { label: "Foo", collapsibleState: Expanded }
  , TreeItem $ Open.coerce { label: "Foo2", collapsibleState: Collapsed }
  ]
treeViewChildren (Child (TreeItem parent)) = case toMaybe $ parent.label of
  Just "Foo" -> [ TreeItem $ Open.coerce { label: "Bar", collapsibleState: None } ]
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
    if FS.isDirectory stat then pure <<< TreeItem $ Open.coerce { label: file.name, collapsibleState: Collapsed }
    else pure <<< TreeItem $ Open.coerce { label: file.name, collapsibleState: None }

errorLogged :: forall a. Aff a -> Aff a
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
        liftEffect $ (register "test-purs.helloWorld" (\_ -> showInformationMessage "foo")  :: Effect Command) >>= subscribeDisposable ctx
        Console.log "Registered command"
        commands <- getCommands
        Console.log (show $ filter (eq "test-purs.helloWorld") commands)
        Console.log "Registering tree view..."
        liftEffect $ (register "nodeDependencies" treeView :: Effect (TreeView Identity TreeItem)) >>= subscribeDisposable ctx
        -- liftEffect $ (register "nodeDependencies" treeViewAff  :: Effect (TreeView Aff File)) >>= subscribeDisposable ctx
        Console.log "TreeView registered"
        -- Console.log (show x)
        Console.log "printed"

deactivateImpl :: Effect Unit
deactivateImpl = do
  Console.log "🍜"

