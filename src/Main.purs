module Main where

import Prelude

import Data.Array.NonEmpty as NEA
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
import Types (ValidationMessage(..))

ensureMaxLength :: Int -> String -> Maybe ValidationMessage
ensureMaxLength maxLen val =
  if Str.length val > maxLen then
    Just $ ErrorMsg $ "Cannot be more than "
      <> Int.toStringAs Int.decimal maxLen
      <> " characters."
  else
    Nothing

ensureValidEmail :: String -> Maybe ValidationMessage
ensureValidEmail val =
  if not Str.contains (Str.Pattern "@") val then
    Just $ ErrorMsg "Invalid email address."
  else
    Nothing

ensureValidSchoolGrade :: Number -> Maybe ValidationMessage
ensureValidSchoolGrade val =
  if val < Int.toNumber 0 || val > Int.toNumber 12 then
    Just $ ErrorMsg "Grades can only be 1-12."
  else
    Nothing

-- https://qiita.com/kimagure/items/581c63707673db61e061
-- alternate approach to builder pattern using unions for partial properties
-- use comonad? https://kodimensional.dev/posts/2019-03-25-comonadic-builders

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

  let
    role =
      OF.mkOptionField "Role"
        [ OF.useOption "admin" "Admin"
        , OF.useOption "student" "Student"
        , OF.useOption "teacher" "Teacher"
        ]

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

  let
    sheet :: SH.Sheet
    sheet =
      SH.mkSheet "School Districts"
        [ SH.useTextField "user_id" userId
        , SH.useTextField "given_name" givenName
        , SH.useTextField "middle_name" middleName
        , SH.useTextField "family_name" familyName
        , SH.useDateField "dob" dob
        , SH.useOptionField "role" role
        , SH.useTextField "email" email
        , SH.useTextField "phone" phone
        , SH.useTextField "username" username
        , SH.useNumberField "grade" grade
        , SH.useTextField "site_id" siteId
        , SH.useTextField "classroom_id" classroomId
        , SH.useTextField "classroom_descriptor" classroomDescriptor
        ]

  let
    workbook :: WB.Workbook
    workbook = WB.mkWorkbook "School Districts Workbook"
      $ NEA.singleton sheet

  let
    spaceConfig :: SC.SpaceConfig
    spaceConfig = SC.mkSpaceConfig "School Districts v1"
      $ NEA.singleton workbook

  Console.logShow userId
  Console.logShow givenName
  Console.logShow middleName
  Console.logShow familyName
  Console.logShow dob
  Console.logShow role
  Console.logShow email
  Console.logShow phone
  Console.logShow username
  Console.logShow grade
  Console.logShow siteId
  Console.logShow classroomId
  Console.logShow classroomDescriptor
  Console.logShow sheet
  Console.logShow workbook
  Console.logShow spaceConfig
-- Console.log "🍝"
