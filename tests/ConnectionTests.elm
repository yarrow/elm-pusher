module ConnectionTests exposing (stateChangeTests)

import Expect
import Json.Decode as D exposing (Decoder)
import Json.Encode as E
import Pusher.Connection as Connection exposing (..)
import Test exposing (Test, describe, test)


type alias Translation =
    { string : String, variant : State }


stateChangeTests : Test
stateChangeTests =
    describe "State Change tests" <|
        let
            states =
                [ { string = "initialized", variant = Initialized }
                , { string = "connecting", variant = Connecting }
                , { string = "connected", variant = Connected }
                , { string = "unavailable", variant = Unavailable }
                , { string = "failed", variant = Failed }
                , { string = "disconnected", variant = Disconnected }
                , { string = "unknown", variant = Unknown }
                ]
        in
        [ describe "fromString" <|
            let
                testFromString state =
                    test state.string <|
                        \_ -> Connection.fromString state.string |> Expect.equal state.variant

                testCases =
                    { string = "Arf!Meow!", variant = Unknown } :: states
            in
            List.map testFromString testCases
        , describe "toString" <|
            let
                testToString state =
                    test state.string <|
                        \_ ->
                            Connection.toString state.variant
                                |> Expect.equal state.string
            in
            List.map testToString states
        , describe "Decoding" <|
            let
                event =
                    ( "event", E.string "connection!state_change" )

                encode prev cur =
                    E.object [ event, ( "previous", E.string prev ), ( "current", E.string cur ) ]

                testDecode prev cur =
                    let
                        encoded =
                            encode prev.string cur.string

                        expected =
                            StateChange prev.variant cur.variant
                    in
                    test (prev.string ++ " -> " ++ cur.string) <|
                        \_ -> D.decodeValue stateChange encoded |> Expect.equal (Ok expected)
            in
            List.concatMap (\prev -> List.map (testDecode prev) states) states
        ]
