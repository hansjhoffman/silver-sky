module DateField
  ( DateField
  , ValidationMessage(..)
  , mkDateField
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

import Data.DateTime (DateTime)
import Data.Either (Either)
import Data.Eq.Generic (genericEq)
import Data.Generic.Rep (class Generic)
import Data.Maybe (Maybe(..))
import Data.Show.Generic (genericShow)

newtype DateField = DateField
  { cast :: Maybe CastFn
  , compute :: Maybe ComputeFn
  , defaultValue :: Maybe DateTime
  , description :: Maybe String
  , displayName :: String
  , isReadonly :: Boolean
  , isRequired :: Boolean
  , isUnique :: Boolean
  , validate :: Maybe ValidateFn
  }

instance showDateField :: Show DateField where
  show (DateField field) = "(DateField '" <> field.displayName <> "')"

type CastFn = (DateTime -> Either DateTime String)

type ComputeFn = (DateTime -> DateTime)

type ValidateFn = (DateTime -> Maybe ValidationMessage)

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

-- | Creates a simple, empty DateField.
-- |
-- | @since 0.0.1
mkDateField :: String -> DateField
mkDateField val = DateField
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
withCast :: CastFn -> DateField -> DateField
withCast fn (DateField field) =
  DateField $ field { cast = Just fn }

-- | Change the current value into something new.
-- |
-- | @since 0.0.1
withCompute :: ComputeFn -> DateField -> DateField
withCompute fn (DateField field) =
  DateField $ field { compute = Just fn }

-- | Sets a default value when none was provided by the user.
-- |
-- | @since 0.0.1
withDefault :: DateTime -> DateField -> DateField
withDefault val (DateField field) =
  DateField $ field { defaultValue = Just val }

-- | Sets the value in the UI table the user will see when they hover their mouse over the column header.
-- |
-- | @since 0.0.1
withDescription :: String -> DateField -> DateField
withDescription val (DateField field) =
  DateField $ field { description = Just val }

-- | Ensures a user cannot edit the value.
-- |
-- | @since 0.0.1
withReadonly :: DateField -> DateField
withReadonly (DateField field) =
  DateField $ field { isReadonly = true }

-- | Ensures a field must have a value otherwise an error message will be present.
-- |
-- | @since 0.0.1
withRequired :: DateField -> DateField
withRequired (DateField field) =
  DateField $ field { isRequired = true }

-- | Ensures a value is unique in the entire column.
-- |
-- | @since 0.0.1
withUnique :: DateField -> DateField
withUnique (DateField field) =
  DateField $ field { isUnique = true }

-- | Validate the current value against certain conditions and display a message to the user when those conditions are not met.
-- |
-- | @since 0.0.1
withValidate :: ValidateFn -> DateField -> DateField
withValidate fn (DateField field) =
  DateField $ field { validate = Just fn }
