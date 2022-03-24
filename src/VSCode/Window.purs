module VSCode.Window
  ( MessageItem(..)
  , messageItem
  , showErrorMessage
  , showErrorMessage'
  , showInformationMessage
  , showInformationMessage'
  , showWarningMessage
  , showWarningMessage'
  ) where

import Prelude

import Control.Promise (Promise, toAffE)
import Data.Undefined.NoProblem (Opt)
import Data.Undefined.NoProblem.Open as Open
import Effect (Effect)
import Effect.Aff (Aff)

newtype MessageItem = MessageItem { title :: String, isCloseAffordance :: Opt Boolean }

foreign import _showInformationMessage :: forall a. String -> Array a -> Effect (Promise a)
foreign import _showErrorMessage :: forall a. String -> Array a -> Effect (Promise a)
foreign import _showWarningMessage :: forall a. String -> Array a -> Effect (Promise a)

wrapShowMessage' :: forall a. (String -> Array a -> Effect (Promise a)) -> String -> Array a -> Aff a
wrapShowMessage' func msg items = toAffE $ func msg items

wrapShowMessage :: forall a. (String -> Array a -> Effect (Promise a)) -> String -> Aff a
wrapShowMessage func msg = toAffE $ func msg []

showInformationMessage :: String -> Aff Unit
showInformationMessage = wrapShowMessage _showInformationMessage

showInformationMessage' :: String -> Array MessageItem -> Aff MessageItem
showInformationMessage' = wrapShowMessage' _showInformationMessage

showErrorMessage :: String -> Aff Unit
showErrorMessage = wrapShowMessage _showErrorMessage

showErrorMessage' :: String -> Array MessageItem -> Aff MessageItem
showErrorMessage' = wrapShowMessage' _showErrorMessage

showWarningMessage :: String -> Aff Unit
showWarningMessage = wrapShowMessage _showWarningMessage

showWarningMessage' :: String -> Array MessageItem -> Aff MessageItem
showWarningMessage' = wrapShowMessage' _showWarningMessage

messageItem = MessageItem <<< Open.coerce
