module Sheet
  ( Scalar
  , Sheet
  , mkSheet
  , withReadonly
  , useDateField
  , useNumberField
  , useOptionField
  , useTextField
  ) where

import Prelude

import Data.Tuple (Tuple(..))
import DateField (DateField)
import NumberField (NumberField)
import OptionField (OptionField)
import TextField (TextField)

newtype Sheet = Sheet
  { displayName :: String
  , fields :: Array (Tuple String Scalar)
  , isReadonly :: Boolean
  }

-- , fields :: NonEmptyArray (Tuple String Scalar)

instance showSheet :: Show Sheet where
  show (Sheet sheet) = "(Sheet '" <> sheet.displayName <> "')"

data Scalar
  = DScalar DateField
  | NScalar NumberField
  | OScalar OptionField
  | TScalar TextField

useDateField :: String -> DateField -> Tuple String Scalar
useDateField key field =
  Tuple key $ DScalar field

useNumberField :: String -> NumberField -> Tuple String Scalar
useNumberField key field =
  Tuple key $ NScalar field

useOptionField :: String -> OptionField -> Tuple String Scalar
useOptionField key field =
  Tuple key $ OScalar field

useTextField :: String -> TextField -> Tuple String Scalar
useTextField key field =
  Tuple key $ TScalar field

-- | Creates a simple sheet.
-- |
-- | @since 0.0.1
-- mkSheet :: String -> NonEmptyArray (Tuple String Scalar) -> Sheet
mkSheet :: String -> Array (Tuple String Scalar) -> Sheet
mkSheet displayName fields = Sheet
  { displayName: displayName
  , fields: fields
  , isReadonly: false
  }

-- | Ensures a user cannot edit any value in the entire sheet.
-- |
-- | @since 0.0.1
withReadonly :: Sheet -> Sheet
withReadonly (Sheet sheet) =
  Sheet $ sheet { isReadonly = true }
