{-# LANGUAGE OverloadedRecordDot #-}
{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE TemplateHaskell #-}

module DeriveHasField (
  module GHC.Records,
  deriveHasFieldWith,
)
where

import Control.Monad
import Data.Char (toLower)
import Data.Foldable as Foldable
import Data.Traversable (for)
import Debug.Trace
import GHC.Records
import Language.Haskell.TH
import Language.Haskell.TH.Datatype

deriveHasFieldWith :: (String -> String) -> Name -> DecsQ
deriveHasFieldWith fieldModifier = makeDeriveHasField fieldModifier <=< reifyDatatype

makeDeriveHasField :: (String -> String) -> DatatypeInfo -> DecsQ
makeDeriveHasField fieldModifier datatypeInfo = do
  -- We do not support sum of product types
  constructorInfo <- case datatypeInfo.datatypeCons of
    [info] -> trace (show datatypeInfo) $ pure info
    _ -> fail "deriveHasField: only supports product types with a single data constructor"

  -- We only support data and newtype declarations
  when (datatypeInfo.datatypeVariant `Foldable.notElem` [Datatype, Newtype]) $
    fail "deriveHasField: only supports data and newtype"

  -- We only support data types with field names and concrete types
  let isConcreteType = \case
        ConT _ -> True
        AppT _ _ -> True
        _ -> False
  recordConstructorNames <- case constructorInfo.constructorVariant of
    RecordConstructor names -> pure names
    _ -> fail "deriveHasField: only supports constructors with field names"
  unless (Foldable.all isConcreteType constructorInfo.constructorFields) $
    fail "deriveHasField: only supports concrete field types"

  -- Build the instances
  let constructorNamesAndTypes :: [(Name, Type)]
      constructorNamesAndTypes = zip recordConstructorNames constructorInfo.constructorFields
  decs <- for constructorNamesAndTypes $ \(name, ty) ->
    let currentFieldName = nameBase name
        wantedFieldName = lowerFirst $ fieldModifier currentFieldName
        litTCurrentField = litT $ strTyLit currentFieldName
        litTFieldWanted = litT $ strTyLit wantedFieldName
        parentTypeConstructor = conT datatypeInfo.datatypeName
     in if currentFieldName == wantedFieldName
          then fail "deriveHasField: after applying fieldModifier, field didn't change"
          else
            [d|
              instance HasField $litTFieldWanted $parentTypeConstructor $(pure ty) where
                getField = $(appTypeE (varE $ mkName "getField") litTCurrentField)
              |]
  pure $ Foldable.concat decs

lowerFirst :: String -> String
lowerFirst = \case
  [] -> []
  (x : xs) -> toLower x : xs
