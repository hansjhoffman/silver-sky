module TextField
  ( TextField
  , ValidationMessage(..)
  , CastFn
  , ComputeFn
  , ValidateFn
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

import Data.Either (Either)
import Data.Eq.Generic (genericEq)
import Data.Generic.Rep (class Generic)
import Data.Maybe (Maybe(..))
import Data.Show.Generic (genericShow)

newtype TextField = TextField
  { cast :: Maybe CastFn
  , compute :: Maybe ComputeFn
  , defaultValue :: Maybe String
  , description :: Maybe String
  , displayName :: String
  , isReadonly :: Boolean
  , isRequired :: Boolean
  , isUnique :: Boolean
  , validate :: Maybe ValidateFn
  }

instance showTextField :: Show TextField where
  show (TextField field) = "(TextField '" <> field.displayName <> "')"

type CastFn = (String -> Either String String)

type ComputeFn = (String -> String)

type ValidateFn = (String -> Maybe ValidationMessage)

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
withCast :: CastFn -> TextField -> TextField
withCast fn (TextField field) =
  TextField $ field { cast = Just fn }

-- | Change the current value into something new.
-- |
-- | @since 0.0.1
withCompute :: ComputeFn -> TextField -> TextField
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
withValidate :: ValidateFn -> TextField -> TextField
withValidate fn (TextField field) =
  TextField $ field { validate = Just fn }
