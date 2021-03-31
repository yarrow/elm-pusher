module ConnectionTests exposing (stateChangeTests)

import Expect
import Json.Decode as D
import Json.Encode as E
import Pusher.Connection as Connection exposing (..)
import Test exposing (Test, describe, test)


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
                ]

            encode prev cur =
                E.object
                    [ ( "event", E.string ":state_change" )
                    , ( "previous", E.string prev )
                    , ( "current", E.string cur )
                    ]
        in
        [ describe "toString" <|
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
        , describe "Unknown values result in errors" <|
            [ test "bad `previous` value" <|
                \_ ->
                    D.decodeValue stateChange (encode "arf" "connected") |> Expect.err
            , test "bad `current` value" <|
                \_ ->
                    D.decodeValue stateChange (encode "connected" "meow") |> Expect.err
            ]
        ]
