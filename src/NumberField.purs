module NumberField
  ( NumberField
  , ValidationMessage(..)
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

import Data.Either (Either)
import Data.Eq.Generic (genericEq)
import Data.Generic.Rep (class Generic)
import Data.Maybe (Maybe(..))
import Data.Show.Generic (genericShow)

newtype NumberField = NumberField
  { cast :: Maybe CastFn
  , compute :: Maybe ComputeFn
  , defaultValue :: Maybe Number
  , description :: Maybe String
  , displayName :: String
  , isReadonly :: Boolean
  , isRequired :: Boolean
  , isUnique :: Boolean
  , validate :: Maybe ValidateFn
  }

instance showNumberField :: Show NumberField where
  show (NumberField field) = "(NumberField '" <> field.displayName <> "')"

type CastFn = (Number -> Either Number String)

type ComputeFn = (Number -> Number)

type ValidateFn = (Number -> Maybe ValidationMessage)

-- | UI validation message displayed to the user in a table cell.
-- | Messages should be 'user friendly'.
data ValidationMessage
  = ErrorMsg String
  | InfoMsg String
  | WarnMsg String

derive instance Generic ValidationMessage _

instance Eq ValidationMessage where
  eq = genericEq

instance Show ValidationMessage where
  show = genericShow

-- | Creates a simple, empty NumberField.
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
-- | @since 0.0.1
withCast :: CastFn -> NumberField -> NumberField
withCast fn (NumberField field) =
  NumberField $ field { cast = Just fn }

-- | Change the current value into something new.
-- | @since 0.0.1
withCompute :: ComputeFn -> NumberField -> NumberField
withCompute fn (NumberField field) =
  NumberField $ field { compute = Just fn }

-- | Sets a default value when none was provided by the user.
-- | @since 0.0.1
withDefault :: Number -> NumberField -> NumberField
withDefault val (NumberField field) =
  NumberField $ field { defaultValue = Just val }

-- | Sets the value in the UI table the user will see when they hover their mouse over the column header.
-- | @since 0.0.1
withDescription :: String -> NumberField -> NumberField
withDescription val (NumberField field) =
  NumberField $ field { description = Just val }

-- | Ensures a user cannot edit the value.
-- | @since 0.0.1
withReadonly :: NumberField -> NumberField
withReadonly (NumberField field) =
  NumberField $ field { isReadonly = true }

-- | Ensures a field must have a value otherwise an error message will be present.
-- | @since 0.0.1
withRequired :: NumberField -> NumberField
withRequired (NumberField field) =
  NumberField $ field { isRequired = true }

-- | Ensures a value is unique in the entire column.
-- | @since 0.0.1
withUnique :: NumberField -> NumberField
withUnique (NumberField field) =
  NumberField $ field { isUnique = true }

-- | Validate the current value against certain conditions and display a message to the user when those conditions are not met.
-- | @since 0.0.1
withValidate :: ValidateFn -> NumberField -> NumberField
withValidate fn (NumberField field) =
  NumberField $ field { validate = Just fn }
