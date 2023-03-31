module Sheet
  ( Scalar(..)
  , Sheet
  , mkSheet
  , withReadonly
  ) where

import Prelude

import Data.Array.NonEmpty (NonEmptyArray)
import Data.Tuple (Tuple)
import DateField (DateField)
import NumberField (NumberField)
import TextField (TextField)

newtype Sheet = Sheet
  { displayName :: String
  , fields :: NonEmptyArray (Tuple String Scalar)
  , isReadonly :: Boolean
  }

instance showSheet :: Show Sheet where
  show (Sheet sheet) = "(Sheet '" <> sheet.displayName <> "')"

data Scalar
  = DScalar DateField
  | NScalar NumberField
  | TScalar TextField

-- | Creates a simple sheet.
-- | @since 0.0.1
mkSheet :: String -> NonEmptyArray (Tuple String Scalar) -> Sheet
mkSheet displayName fields = Sheet
  { displayName: displayName
  , fields: fields
  , isReadonly: false
  }

-- | Ensures a user cannot edit any value in the entire sheet.
-- | @since 0.0.1
withReadonly :: Sheet -> Sheet
withReadonly (Sheet sheet) =
  Sheet $ sheet { isReadonly = true }
