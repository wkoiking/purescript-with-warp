{-# LANGUAGE DefaultSignatures #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE OverloadedStrings #-}

module Argonaut where

import GHC.Generics (Generic, Rep)
import Data.Aeson (SumEncoding(..), Value(..), GFromJSON, GToJSON', Zero, Options(..), genericToJSON, genericParseJSON, defaultOptions)
import qualified Data.Aeson.Types as Aeson (Result(..), Parser, parse)
import qualified Data.Attoparsec.ByteString as Atto (Result, IResult(..), parse)
import Data.Aeson.Parser.Internal (json')
import Data.Aeson.Encoding (value, encodingToLazyByteString)
import Data.ByteString (ByteString)
import qualified Data.ByteString.Lazy as LB
import Data.HashMap.Strict as HM
import Data.Vector as V
import Data.Text (Text)
import Data.List (intercalate)
import Data.Maybe (fromMaybe)

customOptions :: Options
customOptions = defaultOptions
    { fieldLabelModifier      = id
    , constructorTagModifier  = id
    , allNullaryToStringTag   = True
    , omitNothingFields       = False
    , sumEncoding             = TaggedObject
        { tagFieldName      = "tag"
        , contentsFieldName = "values"
        }
    , unwrapUnaryRecords      = False
    , tagSingleConstructors   = True
    , rejectUnknownFields     = False
    }
-- Encode

-- parse :: Parser a -> ByteString -> Result a
-- jsonEOF' :: Parser Value

class FromArgonaut a where
    parseArgonaut :: Value -> Aeson.Parser a
    
    default parseArgonaut :: (Generic a, GFromJSON Zero (Rep a)) => Value -> Aeson.Parser a
    parseArgonaut = genericParseJSON customOptions

fromAttoResult :: Atto.Result a -> Either String a
fromAttoResult (Atto.Fail _remainBstr context err) = Left $ unwords ["Parse failed:", intercalate "." context, err]
fromAttoResult (Atto.Done _remainBstr a)  = Right a
fromAttoResult (Atto.Partial _cont)       = Left "Parse failed: Partial"

fromAesonResult :: Aeson.Result a -> Either String a
fromAesonResult (Aeson.Error err) = Left err
fromAesonResult (Aeson.Success a) = Right a

toValue :: ByteString -> Atto.Result Value
toValue = Atto.parse json'

fromArgonaut :: (FromArgonaut a) => Value -> Aeson.Result a
fromArgonaut = Aeson.parse parseArgonaut

decodeArgonaut :: (FromArgonaut a) => ByteString -> Either String a
decodeArgonaut bstr = do
    val <- fromAttoResult $ toValue bstr
    fromAesonResult $ fromArgonaut $ argonautValueToAesonValue val

-- Decode

-- value :: Value -> Encoding
-- type Encoding = Encoding' Value
-- encodingToLazyByteString :: Encoding' a -> BSL.ByteString

class ToArgonaut a where
    toArgonaut :: a -> Value

    default toArgonaut :: (Generic a, GToJSON' Value Zero (Rep a)) => a -> Value
    toArgonaut = genericToJSON customOptions

fromValue :: Value -> LB.ByteString
fromValue = encodingToLazyByteString . value

encodeArgonaut :: (ToArgonaut a) => a -> LB.ByteString
encodeArgonaut = fromValue . aesonValueToArgonautValue . toArgonaut

--- Aeson to Argonaut
tagKey :: Text
tagKey = "tag"
valueKey :: Text
valueKey = "values"

aesonValueToArgonautValue :: Value -> Value
aesonValueToArgonautValue (Object hash) = Object $ fromMaybe hash $ do
    String tag <- HM.lookup tagKey hash
    let records = HM.delete tagKey hash
    let newVal = case HM.toList records of
            [("values", Array vec)] -> Array vec
            [("values", val)]       -> Array $ V.singleton val
            []                      -> Array V.empty
            _nonEmpty               -> Array $ V.singleton $ Object records
    return $ HM.fromList [ (tagKey, String tag), (valueKey, newVal)]
aesonValueToArgonautValue otherVal = otherVal
--- Argonaut to Aeson

argonautValueToAesonValue :: Value -> Value
argonautValueToAesonValue (Object hashmap) = Object $ flattenRecords $ HM.update unArrayfy valueKey hashmap
argonautValueToAesonValue otherVal = otherVal

flattenRecords :: HashMap Text Value -> HashMap Text Value
flattenRecords hash = case HM.lookup valueKey hash of
    Just (Object records) -> delete valueKey hash `HM.union` records
    _otherCase            -> hash
 
unArrayfy :: Value -> Maybe Value
unArrayfy (Array vec) = case V.toList vec of
    []        -> Nothing
    [val]     -> Just val
    _vals      -> Just $ Array vec
unArrayfy otherVal = Just otherVal

-- Object (fromList [("tag",String "MyRecordType"),("myRec1",Number 1.0),("myRec2",String "aaa")])
-- Object (fromList [("tag",String "MyEnum1"),("values",Array [Number 2.0,String "bbb"])])
-- Object (fromList [("tag",String "MyEnum2"),("values",Number -3.0)])
-- Object (fromList [("tag",String "MyEnum3"),("values",Number 4.3)])
-- Object (fromList [("tag",String "MyEnum4")])
-- Object (fromList [("tag",String "MyNonRecordType"),("values",Array [Number 6.0,String "ddd"])])
-- Object (fromList [("tag",String "MyNewtype"),("values",Number 7.0)])
--
-- Object (fromList [("tag",String "MyRecordType"),("values",Array [Object (fromList [("myRec1",Number 1.0),("myRec2",String "aaa")])])])
-- Object (fromList [("tag",String "MyEnum1"),("values",Array [Number 2.0,String "bbb"])])
-- Object (fromList [("tag",String "MyEnum2"),("values",Array [Number -3.0])])
-- Object (fromList [("tag",String "MyEnum3"),("values",Array [Number 4.3])])
-- Object (fromList [("tag",String "MyEnum4"),("values",Array [])])
-- Object (fromList [("tag",String "MyNonRecordType"),("values",Array [Number 6.0,String "ddd"])])
-- Object (fromList [("tag",String "MyNewtype"),("values",Array [Number 7.0])])
--
-- Object (fromList [("tag", String "MyRecordType"), ("values", Array [Object (fromList [("myRec2", String "aaa"),("myRec1", Number 1)])])])
-- Object (fromList [("tag", String "MyEnum1"), ("values", Array [Number 2, String "bbb"])])
-- Object (fromList [("tag", String "MyEnum2"), ("values", Array [Number (-3)])])
-- Object (fromList [("tag", String "MyEnum3"), ("values", Array [Number 4.3])])
-- Object (fromList [("tag", String "MyEnum4"), ("values", Array [])])
-- Object (fromList [("tag", String "MyNonRecordType"), ("values", Array [Number 6, String "ddd"])])
-- Object (fromList [("tag", String "MyNewtype"), ("values", Array [Number 7])])
--
-- {"values":[{"myRec2":"aaa","myRec1":1}],"tag":"MyRecordType"}
-- {"values":[2,"bbb"]                    ,"tag":"MyEnum1"}
-- {"values":[-3]                         ,"tag":"MyEnum2"}
-- {"values":[4.3]                        ,"tag":"MyEnum3"}
-- {"values":[]                           ,"tag":"MyEnum4"}
-- {"values":[6,"ddd"]                    ,"tag":"MyNonRecordType"}
-- {"values":[7]                          ,"tag":"MyNewtype"}

-- {"values":[{"myRec1":1,"myRec2":"aaa"}],"tag":"MyRecordType"}
-- {"values":[2,"bbb"]                    ,"tag":"MyEnum1"}
-- {"values":[-3]                         ,"tag":"MyEnum2"}
-- {"values":[4.3]                        ,"tag":"MyEnum3"}
-- {"values":[]                           ,"tag":"MyEnum4"}
-- {"values":[6,"ddd"]                    ,"tag":"MyNonRecordType"}
-- {"values":[7]                          ,"tag":"MyNewtype"}

