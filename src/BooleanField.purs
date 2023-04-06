module BooleanField
  ( BooleanField
  , mkBooleanField
  , withDefault
  , withDescription
  , withRequired
  ) where

import Prelude

import Data.Maybe (Maybe(..))

newtype BooleanField = BooleanField
  { defaultValue :: Maybe Boolean
  , description :: Maybe String
  , displayName :: String
  , isRequired :: Boolean
  }

instance showBooleanField :: Show BooleanField where
  show (BooleanField field) = "(BooleanField '" <> field.displayName <> "')"

-- | Creates a simple, empty BooleanField
-- |
-- | @since 0.0.1
mkBooleanField :: String -> BooleanField
mkBooleanField val = BooleanField
  { defaultValue: Nothing
  , description: Nothing
  , displayName: val
  , isRequired: false
  }

-- | Sets a default value when none was provided by the user.
-- |
-- | @since 0.0.1
withDefault :: Boolean -> BooleanField -> BooleanField
withDefault val (BooleanField field) =
  BooleanField $ field { defaultValue = Just val }

-- | Sets the value in the UI table the user will see when they hover their mouse over the column header.
-- |
-- | @since 0.0.1
withDescription :: String -> BooleanField -> BooleanField
withDescription val (BooleanField field) =
  BooleanField $ field { description = Just val }

-- | Ensures a field must have a value otherwise an error message will be present.
-- |
-- | @since 0.0.1
withRequired :: BooleanField -> BooleanField
withRequired (BooleanField field) =
  BooleanField $ field { isRequired = true }
