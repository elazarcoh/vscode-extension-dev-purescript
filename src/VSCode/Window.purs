module VSCode.Window
  ( MessageItem(..)
  , messageItem
  , showInformationMessage
  , showInformationMessage'
  )
  where

import Prelude

import Control.Promise (Promise, toAffE)
import Data.Undefined.NoProblem (Opt)
import Data.Undefined.NoProblem.Open as Open
import Effect (Effect)
import Effect.Aff (Aff)

newtype MessageItem = MessageItem { title :: String, isCloseAffordance :: Opt Boolean }

foreign import _showInformationMessage :: forall a. String -> Array a -> Effect (Promise a)

showInformationMessage :: String -> Aff Unit
showInformationMessage msg = toAffE $ _showInformationMessage msg []

showInformationMessage' :: String -> Array MessageItem -> Aff MessageItem
showInformationMessage' msg items = toAffE $ _showInformationMessage msg items

messageItem = MessageItem <<< Open.coerce
