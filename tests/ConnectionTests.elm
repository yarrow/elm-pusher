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
            glossary =
                [ { string = "initialized", state = Initialized }
                , { string = "connecting", state = Connecting }
                , { string = "connected", state = Connected }
                , { string = "unavailable", state = Unavailable }
                , { string = "failed", state = Failed }
                , { string = "disconnected", state = Disconnected }
                ]

            encode prev cur =
                E.object
                    [ ( "event", E.string ":state_change" )
                    , ( "data"
                      , E.object
                            [ ( "previous", E.string prev )
                            , ( "current", E.string cur )
                            ]
                      )
                    ]
        in
        [ describe "toString" <|
            let
                testToString gloss =
                    test gloss.string <|
                        \_ ->
                            Connection.toString gloss.state
                                |> Expect.equal gloss.string
            in
            List.map testToString glossary
        , describe "Decoding" <|
            let
                testDecode prev cur =
                    let
                        encoded =
                            encode prev.string cur.string

                        expected =
                            StateChange prev.state cur.state
                    in
                    test (prev.string ++ " -> " ++ cur.string) <|
                        \_ -> D.decodeValue stateChange encoded |> Expect.equal (Ok expected)
            in
            List.concatMap (\prev -> List.map (testDecode prev) glossary) glossary
        , describe "Unknown values result in errors" <|
            [ test "bad `previous` value" <|
                \_ ->
                    D.decodeValue stateChange (encode "arf" "connected") |> Expect.err
            , test "bad `current` value" <|
                \_ ->
                    D.decodeValue stateChange (encode "connected" "meow") |> Expect.err
            ]
        ]
