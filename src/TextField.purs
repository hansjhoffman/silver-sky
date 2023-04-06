module TextField
  ( TextField
  , mkTextField
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

newtype TextField = TextField
  { cast :: Maybe (CastFn String)
  , compute :: Maybe (ComputeFn String)
  , defaultValue :: Maybe String
  , description :: Maybe String
  , displayName :: String
  , isReadonly :: Boolean
  , isRequired :: Boolean
  , isUnique :: Boolean
  , validate :: Maybe (ValidateFn String)
  }

instance showTextField :: Show TextField where
  show (TextField field) = "(TextField '" <> field.displayName <> "')"

-- | Creates a simple, empty TextField.
-- |
-- | @since 0.0.1
mkTextField :: String -> TextField
mkTextField val = TextField
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
withCast :: CastFn String -> TextField -> TextField
withCast fn (TextField field) =
  TextField $ field { cast = Just fn }

-- | Change the current value into something new.
-- |
-- | @since 0.0.1
withCompute :: ComputeFn String -> TextField -> TextField
withCompute fn (TextField field) =
  TextField $ field { compute = Just fn }

-- | Sets a default value when none was provided by the user.
-- |
-- | @since 0.0.1
withDefault :: String -> TextField -> TextField
withDefault val (TextField field) =
  TextField $ field { defaultValue = Just val }

-- | Sets the value in the UI table the user will see when they hover their mouse over the column header.
-- |
-- | @since 0.0.1
withDescription :: String -> TextField -> TextField
withDescription val (TextField field) =
  TextField $ field { description = Just val }

-- | Ensures a user cannot edit the value.
-- |
-- | @since 0.0.1
withReadonly :: TextField -> TextField
withReadonly (TextField field) =
  TextField $ field { isReadonly = true }

-- | Ensures a field must have a value otherwise an error message will be present.
-- |
-- | @since 0.0.1
withRequired :: TextField -> TextField
withRequired (TextField field) =
  TextField $ field { isRequired = true }

-- | Ensures a value is unique in the entire column.
-- |
-- | @since 0.0.1
withUnique :: TextField -> TextField
withUnique (TextField field) =
  TextField $ field { isUnique = true }

-- | Validate the current value against certain conditions and display a message to the user when those conditions are not met.
-- |
-- | @since 0.0.1
withValidate :: ValidateFn String -> TextField -> TextField
withValidate fn (TextField field) =
  TextField $ field { validate = Just fn }
