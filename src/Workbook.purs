module Workbook
  ( Workbook
  , mkWorkbook
  ) where

import Prelude

import Data.Array.NonEmpty (NonEmptyArray)
import Sheet (Sheet)

newtype Workbook = Workbook
  { displayName :: String
  , sheets :: NonEmptyArray Sheet
  }

instance showWorkbook :: Show Workbook where
  show (Workbook wb) = "(Workbook '" <> wb.displayName <> "')"

-- | Creates a workbook.
-- | @since 0.0.1
mkWorkbook :: String -> NonEmptyArray Sheet -> Workbook
mkWorkbook displayName sheets = Workbook
  { displayName: displayName
  , sheets: sheets
  }
