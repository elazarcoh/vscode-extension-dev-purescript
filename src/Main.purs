module Main
  ( activateImpl
  , deactivateImpl
  ) where

import Prelude
import Data.Array (filter)
import Data.Either (Either(..))
import Effect (Effect)
import Effect.Aff (Aff, attempt, launchAff_)
import Effect.Class (liftEffect)
import Effect.Class.Console as Console
import Effect.Exception.Unsafe (unsafeThrowException)
import VSCode.Commands (getCommands, registerCommand)
import VSCode.Common (subscribeDisposable)
import VSCode.Types (ExtensionContext)
import VSCode.Window (showInformationMessage)

fooMessageCommand ∷ ExtensionContext -> Effect Unit
fooMessageCommand ctx = do
  disposable <- registerCommand "test-purs.helloWorld" (\_ -> showInformationMessage "foo")
  subscribeDisposable disposable ctx

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

deactivateImpl :: Effect Unit
deactivateImpl = do
  Console.log "🍜"
