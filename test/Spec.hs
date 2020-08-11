{-# Language DeriveGeneric #-}
{-# Language DeriveAnyClass #-}
{-# Language DataKinds #-}
{-# LANGUAGE OverloadedStrings #-}

module Main where

import GHC.Generics (Generic)
import Data.Proxy (Proxy (..))
import Language.PureScript.Bridge

import Argonaut
import Data.Aeson.Types ( Value )
import Type as HT

import qualified Data.ByteString as B ( putStrLn, ByteString )
import qualified Data.ByteString.Lazy.Char8 as LB ( putStrLn, ByteString, toStrict )
import qualified Data.ByteString.Char8 as B ( putStrLn )

main :: IO ()
main = do
    print $ (decodeArgonaut "{\"tag\":\"UpdateCounterValue\",\"values\":[0]}" :: Either String HT.PureScriptToHaskellMessage)
    print $ check $ toArgonaut $ HT.CurrentCounterValue 1
    print $ check $ toArgonaut $ HT.UpdateCounterValue 1
    LB.putStrLn ""
    
    LB.putStrLn $ encodeArgonaut $ HT.CurrentCounterValue 1
    LB.putStrLn $ "{\"values\":[0],\"tag\":\"UpdateCounterValue\"}"

    print $ check2 $ HT.CurrentCounterValue 1
    print $ check2 $ HT.UpdateCounterValue 0

--     LB.putStrLn $ encodeArgonaut $ MyRecordType { myRec2 = "aaa", myRec1 = 1 }
--     LB.putStrLn $ encodeArgonaut $ MyEnum1 2 "bbb"
--     LB.putStrLn $ encodeArgonaut $ MyEnum2 (-3)
--     LB.putStrLn $ encodeArgonaut $ MyEnum3 4.3
--     LB.putStrLn $ encodeArgonaut $ MyEnum4
--     LB.putStrLn $ encodeArgonaut $ MyNonRecordType 6 "ddd"
--     LB.putStrLn $ encodeArgonaut $ MyNewtype 7
--     writePSTypes "test" (buildBridge defaultBridge) myTypes

check :: Value -> (Bool, Value, Value)
check val = (val == argonautValueToAesonValue (aesonValueToArgonautValue val), val, argonautValueToAesonValue (aesonValueToArgonautValue val))

check2 :: (ToArgonaut a, FromArgonaut a, Eq a) => a -> (Bool, a, Either String a)
check2 val = case decodeArgonaut $ LB.toStrict $ encodeArgonaut val of
    Left str -> (False, val, Left str)
    Right val' -> (val' == val, val, Right val')

data MyRecordType = MyRecordType
    { myRec1 :: Int
    , myRec2 :: String
    } deriving (Generic, FromArgonaut, ToArgonaut)

data MyEnumType
    = MyEnum1 Int String
    | MyEnum2 Int
    | MyEnum3 Double
    | MyEnum4
    deriving (Generic, FromArgonaut, ToArgonaut)

data MyNonRecordType = MyNonRecordType Int String
    deriving (Generic, FromArgonaut, ToArgonaut)

newtype MyNewtype = MyNewtype Int
    deriving stock (Generic)
    deriving anyclass (FromArgonaut, ToArgonaut)

-- | All types will have a `Generic` instance produced in Purescript.
myTypes :: [SumType 'Haskell]
myTypes =
--   [ let p = (Proxy :: Proxy PureScriptToHaskellMessage) in equal p (mkSumType p)  -- Also produce a `Eq` instance.
--   , let p = (Proxy :: Proxy HaskellToPureScriptMessage) in order p (mkSumType p)  -- Produce both `Eq` and `Ord`.
  [ mkSumType (Proxy :: Proxy MyRecordType)  -- Just produce a `Generic` instance.
  , mkSumType (Proxy :: Proxy MyEnumType)  -- Just produce a `Generic` instance.
  , mkSumType (Proxy :: Proxy MyNonRecordType)  -- Just produce a `Generic` instance.
  , mkSumType (Proxy :: Proxy MyNewtype)  -- Just produce a `Generic` instance.
  ]

--  writePSTypes "path/to/your/purescript/project" (buildBridge defaultBridge) myTypes