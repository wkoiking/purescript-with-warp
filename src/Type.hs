{-# Language DeriveGeneric #-}
{-# Language DeriveAnyClass #-}
{-# Language DataKinds #-}

module Type where

import GHC.Generics (Generic)
import Data.Proxy (Proxy (..))
import Language.PureScript.Bridge
import Argonaut

data PureScriptToHaskellMessage
    = UpdateCounterValue Int
    | RequestInitialValue
    deriving (Generic, Eq, Show, FromArgonaut, ToArgonaut)

data HaskellToPureScriptMessage
    = CurrentCounterValue Int
    deriving (Generic, Eq, Show, FromArgonaut, ToArgonaut)

-- | All types will have a `Generic` instance produced in Purescript.
myTypes :: [SumType 'Haskell]
myTypes =
  [ mkSumType (Proxy :: Proxy PureScriptToHaskellMessage)  -- Just produce a `Generic` instance.
  , mkSumType (Proxy :: Proxy HaskellToPureScriptMessage)  -- Just produce a `Generic` instance.
  ]




--   [ let p = (Proxy :: Proxy PureScriptToHaskellMessage) in equal p (mkSumType p)  -- Also produce a `Eq` instance.
--   , let p = (Proxy :: Proxy HaskellToPureScriptMessage) in order p (mkSumType p)  -- Produce both `Eq` and `Ord`.
--  writePSTypes "path/to/your/purescript/project" (buildBridge defaultBridge) myTypes