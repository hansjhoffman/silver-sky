module Main where

import Data.Array.NonEmpty as NEA
import DateField as DF
import Effect (Effect)
import Effect.Console as Console
import NumberField as NF
import Prelude (Unit, ($))
import Sheet (Sheet, mkSheet)
import SpaceConfig (SpaceConfig, mkSpaceConfig)
import TextField (mkTextField)
import TextField as TF
import Workbook (Workbook, mkWorkbook)

main :: Effect Unit
main = do
  -- let
  --   firstName =
  --     TF.withRequired
  --     $ TF.withDescription "some description"
  --     $ mkTextField "First Name"
  -- let
  --   age = build
  --     $ NF.withDescription "some desc"
  --     $ NF.mkNumberField "Age"
  -- let
  --   dob = build
  --     $ DF.withDescription "some desc"
  --     $ DF.mkDateField "DoB"

  -- let
  --   sheet :: Sheet
  --   sheet = mkSheet "Contacts" $ NEA.singleton firstName
  --
  -- let
  --   workbook :: Workbook
  --   workbook = mkWorkbook "Some Workbook" $ NEA.singleton sheet
  --
  -- let
  --   spaceConfig :: SpaceConfig
  --   spaceConfig = mkSpaceConfig "Some SpaceConfig" $ NEA.singleton workbook

  -- Console.logShow firstName
  -- Console.logShow $ Slug "some-cool-slug"
  Console.log "🍝"
