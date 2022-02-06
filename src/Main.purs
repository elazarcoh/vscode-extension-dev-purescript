module Main
  ( activate
  , deactivate
  )
  where

import Prelude

import Data.Array (filter)
import Effect (Effect)
import Effect.Aff (launchAff_)
import Effect.Class (liftEffect)
import Effect.Class.Console as Console
import VSCode.Commands (getCommands, registerCommand)
import VSCode.Window (showInformationMessage)

activate :: Effect Unit
activate = launchAff_ do
  Console.log "Activating..."
  _ <- liftEffect $ registerCommand "test-purs.helloWorld" (\_ -> showInformationMessage ("foo"))
  commands <- getCommands
  Console.log (show $ filter (eq "test-purs.helloWorld") commands)
  pure unit

deactivate :: Effect Unit
deactivate = do
  Console.log "🍜"
