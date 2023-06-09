module OptionField
  ( MatchStrategy(..)
  , OptionField
  , mkOptionField
  , useOption
  , withDefault
  , withDescription
  , withMatchStrategy
  , withRequired
  ) where

import Prelude

import Data.Maybe (Maybe(..))
import Data.Tuple (Tuple(..))

newtype OptionField = OptionField
  { defaultValue :: Maybe String
  , description :: Maybe String
  , displayName :: String
  , isRequired :: Boolean
  , matchStrategy :: MatchStrategy
  -- , options :: {_internal :: String} -- Maybe use type JSON.Value?
  -- , options :: forall r . Homogenous r String -- https://pursuit.purescript.org/packages/purescript-typelevel-prelude/7.0.0/docs/Type.Row.Homogeneous
  , options :: Array (Tuple String String)
  }

instance showOptionField :: Show OptionField where
  show (OptionField field) = "(OptionField '" <> field.displayName <> "')"

data MatchStrategy
  = ExactMatch
  | FuzzyMatch

-- | Creates an OptionField
-- |
-- | @since 0.0.1
mkOptionField :: String -> Array (Tuple String String) -> OptionField
mkOptionField val opts = OptionField
  { defaultValue: Nothing
  , description: Nothing
  , displayName: val
  , isRequired: false
  , matchStrategy: FuzzyMatch
  , options: opts
  }

useOption :: String -> String -> Tuple String String
useOption key val =
  Tuple key val

-- | Sets a default value when none was provided by the user.
-- |
-- | @since 0.0.1
withDefault :: String -> OptionField -> OptionField
withDefault val (OptionField field) =
  OptionField $ field { defaultValue = Just val }

-- | Sets the value in the UI table the user will see when they hover their mouse over the column header.
-- |
-- | @since 0.0.1
withDescription :: String -> OptionField -> OptionField
withDescription val (OptionField field) =
  OptionField $ field { description = Just val }

-- | Sets the matching strategy.
-- |
-- | @since 0.0.1
withMatchStrategy :: MatchStrategy -> OptionField -> OptionField
withMatchStrategy strategy (OptionField field) =
  OptionField $ field { matchStrategy = strategy }

-- | Sets the choices the user is constrained to choose from.
-- |
-- | @since 0.0.1
-- withOptions :: Record a -> OptionField -> OptionField
-- withOptions opts (OptionField field) =
--   OptionField $ field { options = opts }

-- | Ensures a field must have a value otherwise an error message will be present.
-- |
-- | @since 0.0.1
withRequired :: OptionField -> OptionField
withRequired (OptionField field) =
  OptionField $ field { isRequired = true }
