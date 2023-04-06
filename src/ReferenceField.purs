module ReferenceField
  ( ReferenceField
  , mkReferenceField
  , withDefault
  , withDescription
  , withRequired
  ) where

import Prelude

import Data.Maybe (Maybe(..))

newtype ReferenceField = ReferenceField
  { defaultValue :: Maybe Boolean
  , description :: Maybe String
  , displayName :: String
  , isRequired :: Boolean
  }

instance showReferenceField :: Show ReferenceField where
  show (ReferenceField field) = "(ReferenceField '" <> field.displayName <> "')"

-- | Creates a simple, empty ReferenceField
-- |
-- | @since 0.0.1
mkReferenceField :: String -> ReferenceField
mkReferenceField val = ReferenceField
  { defaultValue: Nothing
  , description: Nothing
  , displayName: val
  , isRequired: false
  }

-- | Sets a default value when none was provided by the user.
-- |
-- | @since 0.0.1
withDefault :: Boolean -> ReferenceField -> ReferenceField
withDefault val (ReferenceField field) =
  ReferenceField $ field { defaultValue = Just val }

-- | Sets the value in the UI table the user will see when they hover their mouse over the column header.
-- |
-- | @since 0.0.1
withDescription :: String -> ReferenceField -> ReferenceField
withDescription val (ReferenceField field) =
  ReferenceField $ field { description = Just val }

-- | Ensures a field must have a value otherwise an error message will be present.
-- |
-- | @since 0.0.1
withRequired :: ReferenceField -> ReferenceField
withRequired (ReferenceField field) =
  ReferenceField $ field { isRequired = true }
