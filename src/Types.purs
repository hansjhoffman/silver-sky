module Types
  ( CastFn
  , ComputeFn
  , ValidateFn
  , ValidationMessage(..)
  ) where

import Prelude

import Data.Either (Either)
import Data.Eq.Generic (genericEq)
import Data.Generic.Rep (class Generic)
import Data.Maybe (Maybe)
import Data.Show.Generic (genericShow)

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

-- | User defined cast fn
type CastFn a = (a -> Either String a)

-- | User defined compute fn
type ComputeFn a = (a -> a)

-- | User defined validation fn
type ValidateFn a = (a -> Maybe ValidationMessage)
