module TestPusher exposing (suite)

import Expect
import Json.Decode as D
import Json.Encode as E
import Pusher exposing (Error(..), withErrorHandler)
import Test exposing (Test, describe, test, todo)


type TestMsg
    = GotError Pusher.Error
    | GotData String


errorType : TestMsg -> String
errorType msg =
    case msg of
        GotData _ ->
            "no error"

        GotError error ->
            case error of
                JsonError _ ->
                    "JsonError"

                Other ->
                    "Other"


type Data
    = Data String


suite : Test
suite =
    let
        errorHandler : Pusher.Error -> TestMsg
        errorHandler error =
            GotError error

        makeHandler : D.Decoder data -> (data -> TestMsg) -> (D.Value -> TestMsg)
        makeHandler =
            Pusher.withErrorHandler errorHandler

        stringHandler datum =
            GotData datum

        wantString =
            makeHandler D.string stringHandler
    in
    describe "Pusher tests"
        [ test "errorHandler is called for JSON errors" <|
            \_ -> errorType (wantString (E.int 1)) |> Expect.equal "JsonError"
        , test "stringHandler is called for correct JSON" <|
            \_ -> errorType (wantString (E.string "foo")) |> Expect.equal "no error"
        , test "stringHandler returns the given data" <|
            \_ -> wantString (E.string "foo") |> Expect.equal (GotData "foo")
        ]



{-
   test "Error handler is called for JSON errors" <|
     \_ -> Expect.true "Expected a Pusher.JsonError" (isPusherJsonError (handler badData))
-}
