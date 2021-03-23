module TestPusher exposing (suite)

import Expect
import Json.Decode as D
import Json.Decode.Pipeline exposing (required)
import Json.Encode as E
import Pusher exposing (..)
import Test exposing (Test, describe, test)


type alias Data =
    { name : String }


dataEncoder d =
    E.object [ ( "name", E.string d.name ) ]


dataDecoder =
    D.succeed Data |> required "name" D.string


example encoder r =
    E.object
        [ ( "channel", E.string r.channel )
        , ( "event", E.string r.event )
        , ( "uid", E.string r.uid )
        , ( "data", encoder r.data )
        ]


type alias UidData =
    { uid : String
    , data : Data
    }


type alias ChannelEventUidData data =
    { channel : String
    , event : String
    , uid : String
    , data : data
    }


expected : ChannelEventUidData Data
expected =
    { channel = "ABC"
    , event = "Halloween"
    , uid = "a.b"
    , data = { name = "jane" }
    }


encoded : D.Value
encoded =
    example dataEncoder expected


uidData : UidData
uidData =
    { uid = expected.uid, data = expected.data }


type alias UidName =
    { uid : String, name : String }


uidName : UidName
uidName =
    { uid = expected.uid, name = expected.data.name }


type Msg
    = Full (ChannelEventUidData Data)
    | Some UidData
    | WTF String
    | WithName UidName


suite : Test
suite =
    describe "Pusher tests"
        [ test "withEvent returns the value of the `event` field" <|
            \_ ->
                D.decodeValue withEvent encoded |> Expect.equal (Ok expected.event)
        , test "withX for all X returns the whole megilla" <|
            \_ ->
                let
                    decoder =
                        D.map4 ChannelEventUidData
                            withChannel
                            withEvent
                            withUid
                            (withData dataDecoder)
                in
                D.decodeValue decoder encoded |> Expect.equal (Ok expected)
        , test "tagMapN works like Decode.mapN but adds the given variant tag" <|
            \_ ->
                let
                    decoder =
                        tagMap4 Full
                            ChannelEventUidData
                            withChannel
                            withEvent
                            withUid
                            (withData dataDecoder)
                in
                D.decodeValue decoder encoded |> Expect.equal (Ok (Full expected))
        , test "withUid and withData return those fields" <|
            \_ ->
                let
                    decoder =
                        tagMap2 Some UidData withUid (withData dataDecoder)
                in
                D.decodeValue decoder encoded |> Expect.equal (Ok (Some uidData))
        , test "the xIs filters allow the decoders to return Ok when the incoming field meets their specification" <|
            \_ ->
                let
                    decoder =
                        tagMap2 Some UidData withUid (withData dataDecoder)
                            |> channelIs "ABC"
                            |> eventIs "Halloween"
                            |> uidIs "a.b"
                in
                D.decodeValue decoder encoded |> Expect.equal (Ok (Some uidData))
        , describe "when an xIs filter doesn't match the incoming field, the result is an Error" <|
            let
                decoder =
                    tagMap2 Some UidData withUid (withData dataDecoder)

                run filter =
                    D.decodeValue (filter "bwa ha ah!" decoder) encoded
            in
            [ test "chanelIs" <|
                \_ -> run channelIs |> Expect.err
            , test "eventIs" <|
                \_ -> run channelIs |> Expect.err
            , test "uidIs" <|
                \_ -> run channelIs |> Expect.err
            ]
        , test "The xIs filters can be used in oneOf chains" <|
            \_ ->
                let
                    decoder =
                        D.oneOf
                            [ tagMap4 Full
                                ChannelEventUidData
                                withChannel
                                withEvent
                                withUid
                                (withData dataDecoder)
                                |> channelIs "CBS"
                            , tagMap2 Some UidData withUid (withData dataDecoder)
                                |> eventIs "Halloween"
                            , tagMap WTF identity withEvent |> channelIs "ABC"
                            ]
                in
                D.decodeValue decoder encoded |> Expect.equal (Ok (Some uidData))
        , test "The inData decoder reaches into the incoming data field" <|
            \_ ->
                let
                    decoder =
                        tagMap2 WithName UidName withUid (inData "name" D.string)
                in
                D.decodeValue decoder encoded |> Expect.equal (Ok (WithName uidName))
        ]
