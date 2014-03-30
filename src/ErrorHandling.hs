module ErrorHandling(
	ThrowsError,
	TFLError(Parse, Default, TypeErr),
	extractValue) where

import Control.Monad.Error
import Text.Parsec.Error

type ThrowsError = Either TFLError

extractValue :: ThrowsError a -> a
extractValue (Right val) = val

data TFLError
	= Parse ParseError
	| TypeErr String
	| Default String

showError :: TFLError -> String
showError (Parse parseError) = "Parse error at " ++ show parseError
showError (TypeErr str) = "Type error: " ++ str
showError (Default str) = str

instance Show TFLError where
	show = showError

instance Error TFLError where
	noMsg = Default "An error has occured"
	strMsg = Default