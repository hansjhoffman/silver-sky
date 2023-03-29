module Main where

import Prelude

import Data.Array.NonEmpty as NEA
import Data.DateTime as DateTime
import Data.Either (Either(..))
import Data.Int as Int
import Data.Maybe (Maybe(..))
import Data.String as Str
import DateField as DF
import Effect (Effect)
import Effect.Console as Console
import NumberField as NF
import OptionField as OF
import Sheet as SH
import SpaceConfig as SC
import TextField as TF
import Workbook as WB

ensureMaxLength :: Int -> String -> Either TF.ValidationMessage Unit
ensureMaxLength maxLen val =
  if Str.length val > maxLen then
    Left $ TF.ErrorMsg ("Cannot be more than " <> Int.toStringAs Int.decimal maxLen <> " characters.")
  else
    Right unit

ensureValidEmail :: String -> Either TF.ValidationMessage Unit
ensureValidEmail val =
  case Str.indexOf (Str.Pattern "@") val of
    Just _ ->
      Right unit
    Nothing ->
      Left $ TF.ErrorMsg "Invalid email address."

ensureValidSchoolGrade :: Number -> Either NF.ValidationMessage Unit
ensureValidSchoolGrade val =
  if val < Int.toNumber 0 || val > Int.toNumber 12 then
    Left $ NF.ErrorMsg "Grades can only be 1-12"
  else
    Right unit

main :: Effect Unit
main = do
  let
    userId =
      TF.withValidate (ensureMaxLength 6)
        $ TF.withCompute (Str.trim >>> Str.toUpper)
        $ TF.withRequired
        $ TF.withDescription "ID number/code used to identify student/teacher in your internal system."
        $ TF.mkTextField "User Id"

  let
    givenName =
      TF.withValidate (ensureMaxLength 75)
        $ TF.withCompute (Str.trim)
        $ TF.withRequired
        $ TF.withDescription "First Name"
        $ TF.mkTextField "Given Name"

  let
    middleName =
      TF.withCompute (Str.trim)
        $ TF.withDescription "Middle Name"
        $ TF.mkTextField "Middle Name"

  let
    familyName =
      TF.withValidate (ensureMaxLength 75)
        $ TF.withCompute (Str.trim)
        $ TF.withRequired
        $ TF.withDescription "Last Name"
        $ TF.mkTextField "Family Name"

  let
    dob =
      DF.withDescription "Date of Birth"
      $ DF.mkDateField "Date of Birth"

  -- let
  --   role =
  --     -- OF.withOptions {admin: "Admin", student: "Student", teacher: "Teacher"}
  --       OF.withRequired
  --       $ OF.mkOptionField "Role" {admin: "Admin", student: "Student", teacher: "Teacher"}

  let
    email =
      TF.withValidate (ensureValidEmail)
        $ TF.withCompute (Str.trim >>> Str.toLower)
        $ TF.withRequired
        $ TF.withUnique
        $ TF.withDescription "Email"
        $ TF.mkTextField "Email"

  let
    phone =
      TF.withDescription "US Only"
        $ TF.mkTextField "Phone"

  let
    username =
      TF.withCompute (Str.trim >>> Str.toLower)
        $ TF.withRequired
        $ TF.withDescription "Username"
        $ TF.mkTextField "Username"

  let
    grade =
      NF.withValidate (ensureValidSchoolGrade)
        $ NF.withDescription "Grade"
        $ NF.mkNumberField "Grade"

  let
    siteId =
      TF.withCompute (Str.trim)
        $ TF.withRequired
        $ TF.withDescription "School Id that is associated with the school being rostered."
        $ TF.mkTextField "Site Id"

  let
    classroomId =
      TF.withCompute (Str.trim)
        $ TF.withDescription "Classroom Id"
        $ TF.mkTextField "Classroom Id"

  let
    classroomDescriptor =
      TF.withValidate (ensureMaxLength 50)
        $ TF.withCompute (Str.trim)
        $ TF.withDescription "Classroom Descriptor"
        $ TF.mkTextField "Classroom Descriptor"

  -- let
  --   sheet :: SH.Sheet
  --   sheet = SH.mkSheet "School Districts" $ NEA.singleton firstName

  -- let
  --   workbook :: WB.Workbook
  --   workbook = WB.mkWorkbook "School Districts Workbook" $ NEA.singleton sheet

  -- let
  --   spaceConfig :: SC.SpaceConfig
  --   spaceConfig = SC.mkSpaceConfig "School Districts v1" $ NEA.singleton workbook

  Console.logShow userId
  Console.logShow givenName
  Console.logShow middleName
  Console.logShow familyName
  Console.logShow dob
  -- Console.logShow role
  Console.logShow email
  Console.logShow phone
  Console.logShow username
  Console.logShow grade
  Console.logShow siteId
  Console.logShow classroomId
  Console.logShow classroomDescriptor
-- Console.log "🍝"
