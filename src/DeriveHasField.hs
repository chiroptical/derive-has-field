{-# LANGUAGE OverloadedRecordDot #-}
{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE TemplateHaskell #-}

module DeriveHasField (
  module GHC.Records,
  deriveHasField,
  deriveHasFieldWith,
  deriveHasFieldWithPrefix,
)
where

import Control.Monad
import Data.Char (toLower)
import Data.Foldable as Foldable
import Data.List (isPrefixOf, stripPrefix)
import Data.Maybe (fromMaybe)
import Data.Traversable (for)
import GHC.Records
import Language.Haskell.TH
import Language.Haskell.TH.Datatype

deriveHasFieldWith :: (String -> String) -> Name -> DecsQ
deriveHasFieldWith fieldModifier name = do
  (datatypeInfo, constructorInfo) <- getSingleDataConstructorInfo name
  makeDeriveHasField fieldModifier datatypeInfo constructorInfo

deriveHasField :: Name -> DecsQ
deriveHasField name = do
  (datatypeInfo, constructorInfo) <- getSingleDataConstructorInfo name
  fieldNames <- getRecordConstructorFieldNames constructorInfo
  let prefix = lowerFirst $ nameBase constructorInfo.constructorName
      dropPrefix input = fromMaybe input $ stripPrefix prefix input
  validateFieldNames Assumed prefix fieldNames
  when (nameBase constructorInfo.constructorName /= nameBase datatypeInfo.datatypeName) $
    fail "deriveHasField: type and data constructor must have the same string representation"
  makeDeriveHasField dropPrefix datatypeInfo constructorInfo

deriveHasFieldWithPrefix :: String -> Name -> DecsQ
deriveHasFieldWithPrefix prefix name = do
  (datatypeInfo, constructorInfo) <- getSingleDataConstructorInfo name
  fieldNames <- getRecordConstructorFieldNames constructorInfo
  validateFieldNames Given prefix fieldNames
  let dropPrefix input = fromMaybe input $ stripPrefix prefix input
  makeDeriveHasField dropPrefix datatypeInfo constructorInfo

data ValidateFieldNamesVersion = Given | Assumed

validateFieldNamesVersionToHuman :: ValidateFieldNamesVersion -> String
validateFieldNamesVersionToHuman = \case
  Given -> "given"
  Assumed -> "assumed"

validateFieldNames :: ValidateFieldNamesVersion -> String -> [Name] -> Q ()
validateFieldNames version prefix fieldNames =
  unless (all (isPrefixOf prefix . nameBase) fieldNames) $ do
    fail $
      "deriveHasField: the "
        <> validateFieldNamesVersionToHuman version
        <> " prefix `"
        <> prefix
        <> "` doesn't match the data constructor names"

getRecordConstructorFieldNames :: ConstructorInfo -> Q [Name]
getRecordConstructorFieldNames info =
  case info.constructorVariant of
    RecordConstructor names -> pure names
    _ -> fail "deriveHasField: only supports data constructors with field names"

getSingleDataConstructorInfo :: Name -> Q (DatatypeInfo, ConstructorInfo)
getSingleDataConstructorInfo name = do
  datatypeInfo <- reifyDatatype name
  constructorInfo <- case datatypeInfo.datatypeCons of
    [info] -> pure info
    _ -> fail "deriveHasField: only supports product types with a single data constructor"
  pure (datatypeInfo, constructorInfo)

makeDeriveHasField :: (String -> String) -> DatatypeInfo -> ConstructorInfo -> DecsQ
makeDeriveHasField fieldModifier datatypeInfo constructorInfo = do
  -- We only support data and newtype declarations
  when (datatypeInfo.datatypeVariant `Foldable.notElem` [Datatype, Newtype]) $
    fail "deriveHasField: only supports data and newtype"

  -- We only support data types with field names
  recordConstructorNames <- getRecordConstructorFieldNames constructorInfo

  -- Build the instances
  let constructorNamesAndTypes :: [(Name, Type)]
      constructorNamesAndTypes = zip recordConstructorNames constructorInfo.constructorFields
      parentType =
        foldl'
          (\acc var -> appT acc (varT $ tyVarBndrToName var))
          (conT datatypeInfo.datatypeName)
          datatypeInfo.datatypeVars
  decs <- for constructorNamesAndTypes $ \(name, ty) ->
    let currentFieldName = nameBase name
        wantedFieldName = lowerFirst $ fieldModifier currentFieldName
        litTCurrentField = litT $ strTyLit currentFieldName
        litTFieldWanted = litT $ strTyLit wantedFieldName
     in if currentFieldName == wantedFieldName
          then fail "deriveHasField: after applying fieldModifier, field didn't change"
          else
            [d|
              instance HasField $litTFieldWanted $parentType $(pure ty) where
                getField = $(appTypeE (varE 'getField) litTCurrentField)
              |]
  pure $ Foldable.concat decs

lowerFirst :: String -> String
lowerFirst = \case
  [] -> []
  (x : xs) -> toLower x : xs

tyVarBndrToName :: TyVarBndr flag -> Name
tyVarBndrToName = \case
  PlainTV name _ -> name
  KindedTV name _ _ -> name
