module SpaceConfig
  ( SpaceConfig
  , mkSpaceConfig
  , withSlug
  ) where

import Prelude

import Data.Array.NonEmpty (NonEmptyArray)
import Data.Eq.Generic (genericEq)
import Data.Generic.Rep (class Generic)
import Data.Maybe (Maybe(..))
import Data.Show.Generic (genericShow)
import Workbook (Workbook)

newtype SpaceConfig = SpaceConfig
  { name :: String
  , slug :: Maybe Slug
  , workbooks :: NonEmptyArray Workbook
  }

instance showSpaceConfig :: Show SpaceConfig where
  show (SpaceConfig sc) = "(SpaceConfig '" <> sc.name <> "')"

newtype Slug = Slug String

derive instance Generic Slug _

instance Eq Slug where
  eq = genericEq

instance Show Slug where
  show = genericShow

-- | Creates a re-usable Space Config
-- |
-- | @since 0.0.1
mkSpaceConfig :: String -> NonEmptyArray Workbook -> SpaceConfig
mkSpaceConfig name workbooks = SpaceConfig
  { name: name
  , slug: Nothing
  , workbooks: workbooks
  }

-- | Overwrites the default slug with a custom slug.
-- |
-- | @since 0.0.1
withSlug :: String -> SpaceConfig -> SpaceConfig
withSlug val (SpaceConfig sc) =
  SpaceConfig $ sc { slug = Just $ Slug val }
