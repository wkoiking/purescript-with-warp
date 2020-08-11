module Main (main) where

import Oak (App, Maybe(..), View, appendChildNode, button, createApp, div, getElementById, onClick, runApp, text)

import Type as HT
import Prelude hiding (div)
import Effect.Class.Console (log)
import Effect (Effect)
import Effect.Class (liftEffect)
import Data.Argonaut.Core (Json, stringify)
import Affjax as Ajax
import Affjax.RequestBody as RecBody
import Affjax.ResponseFormat as RecFormat
import Data.Argonaut.Decode.Generic.Rep (genericDecodeJson)
import Data.Argonaut.Encode.Generic.Rep (genericEncodeJson)
import Effect.Aff (runAff_)
import Effect.Exception (Error) as Except
import Data.Either (Either(..))

type Model =
    { counter :: Int
    , message :: String
    }

data Msg
    = GotMessageFromHaskell HT.HaskellToPureScriptMessage
    | DecodeErrorDetect String
    | Decrement
    | Increment
    | Submit
    | Refresh

view :: Model -> View Msg
view model = div [] do
    button [onClick Increment] do
        text "+"
    div [] do
        text model.counter
    button [onClick Decrement] do
        text "-"
    div [] do
        text model.message
    button [onClick Submit] do
        text "Submit"
    button [onClick Refresh] do
        text "Refresh"

next :: Msg -> Model -> (Msg -> Effect Unit) -> Effect Unit
next msg model respond = case msg of
    Submit  -> post respond $ HT.UpdateCounterValue $ model.counter
    Refresh -> post respond $ HT.RequestInitialValue
    _       -> mempty 

update :: Msg -> Model -> Model
update msg model = case msg of
    Increment -> model { counter = model.counter + 1 }
    Decrement -> model { counter = model.counter - 1 }
    GotMessageFromHaskell hsMsg -> case hsMsg of
        HT.CurrentCounterValue n -> model { counter = n }
    DecodeErrorDetect err -> model { message = err }
    Submit -> model
    Refresh -> model

init :: Model
init =
    { counter : 0
    , message : "No error detected"
    }

app :: App Msg Model
app = createApp { init, view, update, next }

main :: Effect Unit
main = do
--   log $ stringify $ genericEncodeJson $ HT.MyRecordType { myRec2 : "aaa", myRec1 : 1 }
--   log $ stringify $ genericEncodeJson $ HT.MyEnum1 2 "bbb"
--   log $ stringify $ genericEncodeJson $ HT.MyEnum2 (-3)
--   log $ stringify $ genericEncodeJson $ HT.MyEnum3 4.3
--   log $ stringify $ genericEncodeJson $ HT.MyEnum4
--   log $ stringify $ genericEncodeJson $ HT.MyNonRecordType 6 "ddd"
--   log $ stringify $ genericEncodeJson $ HT.MyNewtype 7
--   log $ stringify $ genericEncodeJson $ ({ some : 1, thing : "ddd"} :: SomeThing)
    rootNode <- runApp app Nothing
    container <- getElementById "app"
    appendChildNode container rootNode

mesh :: forall a err . (err -> String) -> Either err a -> Either String a
mesh showFunc (Left err) = Left $ showFunc err
mesh showFunc (Right a)  = Right a

post :: (Msg -> Effect Unit) -> HT.PureScriptToHaskellMessage -> Effect Unit
post respond psMsg = runAff_ errorHandler do
    let msgJson :: Json
        msgJson = genericEncodeJson psMsg
    eHttpResponse  <- liftM1 (mesh Ajax.printError) $ Ajax.post RecFormat.json "http://localhost:8080" $ Just $ RecBody.Json msgJson
--         case eHttpResponse of
--             Left err -> log $ "Ajax.post error!: " <> show err
--             Right json  -> log $ "Ajax.post received!:"  <> stringify json
    let eHsMsg :: Either String HT.HaskellToPureScriptMessage
        eHsMsg = do
            httpResponse <- eHttpResponse
            mesh show $ genericDecodeJson httpResponse.body
    liftEffect $ case eHsMsg of
        Right hsMsg -> respond $ GotMessageFromHaskell hsMsg
        Left  err    -> respond $ DecodeErrorDetect err
 where errorHandler :: Either Except.Error Unit -> Effect Unit
       errorHandler (Left err) = log $ "Error: " <> show err
       errorHandler (Right _)  = log $ "Success!"
