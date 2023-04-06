module NumberField
  ( NumberField
  , mkNumberField
  , withCast
  , withCompute
  , withDefault
  , withDescription
  , withReadonly
  , withRequired
  , withUnique
  , withValidate
  ) where

import Prelude

import Data.Maybe (Maybe(..))
import Types (CastFn, ComputeFn, ValidateFn)

newtype NumberField = NumberField
  { cast :: Maybe (CastFn Number)
  , compute :: Maybe (ComputeFn Number)
  , defaultValue :: Maybe Number
  , description :: Maybe String
  , displayName :: String
  , isReadonly :: Boolean
  , isRequired :: Boolean
  , isUnique :: Boolean
  , validate :: Maybe (ValidateFn Number)
  }

instance showNumberField :: Show NumberField where
  show (NumberField field) = "(NumberField '" <> field.displayName <> "')"

-- | Creates a simple, empty NumberField.
-- |
-- | @since 0.0.1
mkNumberField :: String -> NumberField
mkNumberField val = NumberField
  { cast: Nothing
  , compute: Nothing
  , defaultValue: Nothing
  , description: Nothing
  , displayName: val
  , isReadonly: false
  , isRequired: false
  , isUnique: false
  , validate: Nothing
  }

-- | Parse the given value into a special string.
-- |
-- | @since 0.0.1
withCast :: CastFn Number -> NumberField -> NumberField
withCast fn (NumberField field) =
  NumberField $ field { cast = Just fn }

-- | Change the current value into something new.
-- |
-- | @since 0.0.1
withCompute :: ComputeFn Number -> NumberField -> NumberField
withCompute fn (NumberField field) =
  NumberField $ field { compute = Just fn }

-- | Sets a default value when none was provided by the user.
-- |
-- | @since 0.0.1
withDefault :: Number -> NumberField -> NumberField
withDefault val (NumberField field) =
  NumberField $ field { defaultValue = Just val }

-- | Sets the value in the UI table the user will see when they hover their mouse over the column header.
-- |
-- | @since 0.0.1
withDescription :: String -> NumberField -> NumberField
withDescription val (NumberField field) =
  NumberField $ field { description = Just val }

-- | Ensures a user cannot edit the value.
-- |
-- | @since 0.0.1
withReadonly :: NumberField -> NumberField
withReadonly (NumberField field) =
  NumberField $ field { isReadonly = true }

-- | Ensures a field must have a value otherwise an error message will be present.
-- |
-- | @since 0.0.1
withRequired :: NumberField -> NumberField
withRequired (NumberField field) =
  NumberField $ field { isRequired = true }

-- | Ensures a value is unique in the entire column.
-- |
-- | @since 0.0.1
withUnique :: NumberField -> NumberField
withUnique (NumberField field) =
  NumberField $ field { isUnique = true }

-- | Validate the current value against certain conditions and display a message to the user when those conditions are not met.
-- |
-- | @since 0.0.1
withValidate :: ValidateFn Number -> NumberField -> NumberField
withValidate fn (NumberField field) =
  NumberField $ field { validate = Just fn }
