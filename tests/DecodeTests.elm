module DecodeTests exposing
    ( errorReportTests
    , extendedFilterExample
    , fieldDecoders
    , filters
    , memberOperations
    , tagMapTests
    )

import Expect
import Json.Decode as D exposing (Decoder)
import Json.Decode.Pipeline exposing (required)
import Json.Encode as E
import Pusher.Decode exposing (..)
import Pusher.Membership exposing (..)
import Test exposing (Test, describe, test)



-- Tests for field decoders (like withEvent, or withData dataDecoder


fieldDecoders : Test
fieldDecoders =
    describe "Field Decoders" <|
        [ test "withChannel, withEvent, withUid, and (withData dataDecoder) decode the relevent fields of the encoded object" <|
            \_ ->
                let
                    decoder =
                        D.map4 ChannelEventUidData
                            withChannel
                            withEvent
                            withUid
                            (withData dataDecoder)

                    dataDecoder =
                        D.succeed Data |> required "name" D.string
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

                    dataDecoder =
                        D.succeed Data |> required "name" D.string
                in
                D.decodeValue decoder encoded |> Expect.equal (Ok (Full expected))
        , test "The inData decoder reaches into the incoming data field" <|
            \_ ->
                let
                    decoder =
                        tagMap2 WithName Member withUid (inData [ "name" ] D.string)
                in
                D.decodeValue decoder encoded |> Expect.equal (Ok (WithName expectedMember))
        , test "The inData decoder can reach deep into the incoming data field" <|
            \_ ->
                let
                    source =
                        """ { "data":{ "name":{ "nombre":"jane" } } } """

                    decoder =
                        D.map Data (inData [ "name", "nombre" ] D.string)
                in
                D.decodeString decoder source |> Expect.equal (Ok { name = "jane" })
        ]


type alias ChannelEventUidData data =
    { channel : String
    , event : String
    , uid : String
    , data : data
    }


type alias Data =
    { name : String }


expected : ChannelEventUidData Data
expected =
    { channel = "ABC"
    , event = "Halloween"
    , uid = "a.b"
    , data = { name = "jane" }
    }


encode r =
    let
        dataEncoder datum =
            E.object [ ( "name", E.string datum.name ) ]
    in
    E.object
        [ ( "channel", E.string r.channel )
        , ( "event", E.string r.event )
        , ( "uid", E.string r.uid )
        , ( "data", dataEncoder r.data )
        ]


encoded : D.Value
encoded =
    encode expected


type Msg
    = Full (ChannelEventUidData Data)
    | Some UidData
    | WTF String


type alias UidData =
    { uid : String
    , data : Data
    }



-- tests for filters (like eventIs "county election")


uidData : UidData
uidData =
    { uid = expected.uid, data = expected.data }


filters : Test
filters =
    describe "Filters" <|
        [ test "Each isX filter allows its decoder to return OK when Ok when the incoming field meets its specification" <|
            \_ ->
                let
                    decoder =
                        tagMap2 Some UidData withUid (withData dataDecoder)
                            |> channelIs "ABC"
                            |> eventIs "Halloween"
                            |> uidIs "a.b"

                    dataDecoder =
                        D.succeed Data |> required "name" D.string
                in
                D.decodeValue decoder encoded |> Expect.equal (Ok (Some uidData))
        , describe "when an xIs filter doesn't match the incoming field, the result is an Error" <|
            let
                decoder =
                    tagMap2 Some UidData withUid (withData dataDecoder)

                dataDecoder =
                    D.succeed Data |> required "name" D.string

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
        , test "When xIs filters are used in oneOf chains, the first decoder with a matching filter is the one that's used" <|
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

                    dataDecoder =
                        D.succeed Data |> required "name" D.string
                in
                D.decodeValue decoder encoded |> Expect.equal (Ok (Some uidData))
        ]



-- An extended filter test


type PetLair
    = Dog Doghouse
    | Cat CatTree
    | Spider SpiderWeb


type alias Doghouse =
    { color : String
    , volume : Int
    }


dogHouse : Decoder PetLair
dogHouse =
    tagMap2 Dog Doghouse (inData [ "color" ] D.string) (inData [ "volume" ] D.int) |> eventIs "Dog"


type alias CatTree =
    { color : String
    , height : Int
    }


catTree : Decoder PetLair
catTree =
    tagMap2 Cat CatTree (inData [ "color" ] D.string) (inData [ "height" ] D.int) |> eventIs "Cat"


type alias SpiderWeb =
    { strength : Int
    , strands : Int
    }


spiderWeb : Decoder PetLair
spiderWeb =
    tagMap2 Spider SpiderWeb (inData [ "strength" ] D.int) (inData [ "strands" ] D.int) |> eventIs "Spider"


extendedFilterExample : Test
extendedFilterExample =
    test "Another decoding example" <|
        \_ ->
            let
                lairs =
                    [ Dog { color = "brown", volume = 96 }
                    , Spider { strength = 42, strands = 1000 }
                    , Cat { color = "tan", height = 8 }
                    ]

                lairString =
                    """
                    [ { "event":"Dog", "data":{ "color":"brown", "volume":96 } }
                    , { "data":{ "strands":1000, "strength":42 }, "event":"Spider" }
                    , { "event":"Cat", "data":{ "color":"tan", "height":8 } }
                    ]
                    """

                decoder =
                    D.list (D.oneOf [ dogHouse, catTree, spiderWeb ])
            in
            D.decodeString decoder lairString |> Expect.equal (Ok lairs)



-- Tests for memberAdded, memberRemoved, withMe and withMembers


memberOperations : Test
memberOperations =
    describe "Member operations" <|
        let
            getMember =
                D.map2 Member withUid (inData [ "name" ] D.string)

            encodeMember m =
                E.object
                    [ ( "uid", E.string m.uid )
                    , ( "data", E.object [ ( "name", E.string m.name ) ] )
                    ]

            memberAdded =
                encode { expected | event = "pusher:member_added" }

            memberRemoved =
                encode { expected | event = "pusher:member_removed" }
        in
        [ test "Ensure getMember works as expected" <|
            \_ ->
                let
                    decoder =
                        D.map WithName getMember
                in
                D.decodeValue decoder encoded |> Expect.equal (Ok (WithName expectedMember))
        , test "isAdded" <|
            \_ ->
                let
                    decoder =
                        D.map MemberAdded getMember |> isAdded
                in
                D.decodeValue decoder memberAdded |> Expect.equal (Ok (MemberAdded expectedMember))
        , test "isAdded with channel" <|
            \_ ->
                let
                    decoder =
                        tagMap2
                            MemberAddedOnChannel
                            MemberOnChannel
                            withChannel
                            getMember
                            |> isAdded

                    expectedChannelMember =
                        MemberOnChannel expected.channel
                            (Member expected.uid expected.data.name)
                in
                D.decodeValue decoder memberAdded
                    |> Expect.equal (Ok (MemberAddedOnChannel expectedChannelMember))
        , test "isRemoved" <|
            \_ ->
                let
                    decoder =
                        D.map MemberRemoved getMember |> isRemoved
                in
                D.decodeValue decoder memberRemoved
                    |> Expect.equal (Ok (MemberRemoved expectedMember))
        , test "withMe{,embers}" <|
            \_ ->
                let
                    decoder =
                        tagMap2 Subscribers
                            MemberData
                            (withMe getMember)
                            (withMembers getMember)

                    theMembers =
                        { me = expectedMember
                        , members =
                            [ { uid = "1", name = "pat" }
                            , expectedMember
                            , { uid = "3", name = "robin" }
                            ]
                        }

                    encodedMembers =
                        E.object
                            [ ( "me", encodeMember theMembers.me )
                            , ( "members", E.list encodeMember theMembers.members )
                            ]
                in
                D.decodeValue decoder encodedMembers |> Expect.equal (Ok (Subscribers theMembers))
        ]


type alias Member =
    { uid : String, name : String }


type alias MemberData =
    { me : Member
    , members : List Member
    }


expectedMember : Member
expectedMember =
    { uid = expected.uid, name = expected.data.name }


type alias MemberOnChannel =
    { channel : String
    , member : Member
    }


type Mbr
    = WithName Member
    | MemberAdded Member
    | MemberRemoved Member
    | Subscribers MemberData
    | MemberAddedOnChannel MemberOnChannel



-- Tests for tagMap, tagMap2, ... tagMap8


type alias Record1 =
    { f1 : Int }


type alias Record2 =
    { f1 : Int, f2 : Int }


type alias Record3 =
    { f1 : Int, f2 : Int, f3 : Int }


type alias Record4 =
    { f1 : Int, f2 : Int, f3 : Int, f4 : Int }


type alias Record5 =
    { f1 : Int, f2 : Int, f3 : Int, f4 : Int, f5 : Int }


type alias Record6 =
    { f1 : Int, f2 : Int, f3 : Int, f4 : Int, f5 : Int, f6 : Int }


type alias Record7 =
    { f1 : Int, f2 : Int, f3 : Int, f4 : Int, f5 : Int, f6 : Int, f7 : Int }


type alias Record8 =
    { f1 : Int, f2 : Int, f3 : Int, f4 : Int, f5 : Int, f6 : Int, f7 : Int, f8 : Int }


type BoilerPlate
    = Variant1 Record1
    | Variant2 Record2
    | Variant3 Record3
    | Variant4 Record4
    | Variant5 Record5
    | Variant6 Record6
    | Variant7 Record7
    | Variant8 Record8


f n =
    -- field name n
    "f" ++ String.fromInt n


d n =
    -- decode an integer from field data.fn
    inData [ f n ] D.int


e n =
    -- encode an integer as field fn
    ( f n, E.int n )


enc1 =
    E.object [ ( "data", E.object [ e 1 ] ) ]


enc2 =
    E.object [ ( "data", E.object [ e 1, e 2 ] ) ]


enc3 =
    E.object [ ( "data", E.object [ e 1, e 2, e 3 ] ) ]


enc4 =
    E.object [ ( "data", E.object [ e 1, e 2, e 3, e 4 ] ) ]


enc5 =
    E.object [ ( "data", E.object [ e 1, e 2, e 3, e 4, e 5 ] ) ]


enc6 =
    E.object [ ( "data", E.object [ e 1, e 2, e 3, e 4, e 5, e 6 ] ) ]


enc7 =
    E.object [ ( "data", E.object [ e 1, e 2, e 3, e 4, e 5, e 6, e 7 ] ) ]


enc8 =
    E.object [ ( "data", E.object [ e 1, e 2, e 3, e 4, e 5, e 6, e 7, e 8 ] ) ]


tagMapTests : Test
tagMapTests =
    describe "tagMapN tests" <|
        let
            decodes encodedValue decoder =
                D.decodeValue decoder encodedValue

            to =
                Expect.equal
        in
        [ test "tagMapUnabbreviated" <|
            \_ ->
                D.decodeValue (tagMap Variant1 Record1 (d 1)) enc1
                    |> Expect.equal (Ok (Variant1 (Record1 1)))
        , test "tagMap" <|
            \_ ->
                tagMap Variant1 Record1 (d 1)
                    |> decodes enc1
                    |> to (Ok (Variant1 (Record1 1)))
        , test "tagMap2" <|
            \_ ->
                tagMap2 Variant2 Record2 (d 1) (d 2)
                    |> decodes enc2
                    |> to (Ok (Variant2 (Record2 1 2)))
        , test "tagMap3" <|
            \_ ->
                tagMap3 Variant3 Record3 (d 1) (d 2) (d 3)
                    |> decodes enc3
                    |> to (Ok (Variant3 (Record3 1 2 3)))
        , test "tagMap4" <|
            \_ ->
                tagMap4 Variant4 Record4 (d 1) (d 2) (d 3) (d 4)
                    |> decodes enc4
                    |> to (Ok (Variant4 (Record4 1 2 3 4)))
        , test "tagMap5" <|
            \_ ->
                tagMap5 Variant5 Record5 (d 1) (d 2) (d 3) (d 4) (d 5)
                    |> decodes enc5
                    |> to (Ok (Variant5 (Record5 1 2 3 4 5)))
        , test "tagMap6" <|
            \_ ->
                tagMap6 Variant6 Record6 (d 1) (d 2) (d 3) (d 4) (d 5) (d 6)
                    |> decodes enc6
                    |> to (Ok (Variant6 (Record6 1 2 3 4 5 6)))
        , test "tagMap7" <|
            \_ ->
                tagMap7 Variant7 Record7 (d 1) (d 2) (d 3) (d 4) (d 5) (d 6) (d 7)
                    |> decodes enc7
                    |> to (Ok (Variant7 (Record7 1 2 3 4 5 6 7)))
        , test "tagMap8" <|
            \_ ->
                tagMap8 Variant8 Record8 (d 1) (d 2) (d 3) (d 4) (d 5) (d 6) (d 7) (d 8)
                    |> decodes enc8
                    |> to (Ok (Variant8 (Record8 1 2 3 4 5 6 7 8)))
        ]


errorReportTests : Test
errorReportTests =
    describe "Error parsing" <|
        let
            annotate report =
                { channel = report.channel
                , event = report.event
                , code = report.code
                , text = report.text
                , json = E.encode 2 report.json
                }

            equivalent input output =
                let
                    report =
                        infallible errorReport input
                in
                annotate report
                    |> Expect.equal (annotate output)

            infallible : Decoder a -> String -> a
            infallible decoder s =
                case D.decodeString decoder s of
                    Ok v ->
                        v

                    Err _ ->
                        Debug.todo "This can't happen: decoding failed"

            encodeThe : String -> D.Value
            encodeThe =
                infallible D.value
        in
        [ test "Subscription Error" <|
            \_ ->
                let
                    source =
                        """{
                        "channel": "presence-main",
                        "event": "pusher:subscription_error",
                        "data": {
                            "type": "AuthError",
                            "error": "Gosh darn it!",
                            "status": 401
                            }
                        }"""

                    wanted : ErrorReport
                    wanted =
                        { channel = "presence-main"
                        , event = SubscriptionError
                        , code = 401
                        , text = "Subscription to presence-main failed: Gosh darn it!"
                        , json = encodeThe source
                        }
                in
                equivalent source wanted
        , test "Pusher connection error" <|
            \_ ->
                let
                    source =
                        """{
                        "channel": ":connection",
                        "event": ":connection_error",
                        "data": { "type": "WebSocketError",
                                "error": {
                                    "type": "PusherError",
                                    "data": {
                                        "code": 4001,
                                        "message": "App key REDACTED not in this cluster. Did you forget to specify the cluster?"
                                    }
                                }
                            }
                        }"""

                    wanted : ErrorReport
                    wanted =
                        { channel = ":connection"
                        , event = ConnectionError
                        , code = 4001
                        , text = "App key REDACTED not in this cluster. Did you forget to specify the cluster?"
                        , json = encodeThe source
                        }
                in
                equivalent source wanted
        , test "Super-minimal error report is decoded to something reasonable" <|
            \_ ->
                let
                    source =
                        """{
                        "channel": ":connection",
                        "event": ":connection_error",
                        "data": {
                            "type": "WebSocketError",
                            "error": { "isTrusted": true }
                            }
                        }"""

                    wanted : ErrorReport
                    wanted =
                        { channel = ":connection"
                        , event = ConnectionError
                        , code = 0
                        , text = "Connection error"
                        , json = encodeThe source
                        }
                in
                equivalent source wanted
        , test "We can handle the case of a code with no text message" <|
            \_ ->
                let
                    source =
                        """{
                            "channel": ":connection",
                            "event": ":connection_error",
                            "data": {
                                "type": "PusherError",
                                "data": { "code": 1006 }
                                }
                            }"""

                    wanted =
                        { channel = ":connection"
                        , event = ConnectionError
                        , code = 1006
                        , text = "Error connecting to the internet (1006)"
                        , json = encodeThe source
                        }
                in
                equivalent source wanted
        , describe "Undecodeable error reports at least get the right channel and event" <|
            [ test "connection" <|
                \_ ->
                    let
                        source =
                            """{ "event": ":connection_error", "channel": ":connection" }"""

                        wanted =
                            { channel = ":connection"
                            , event = ConnectionError
                            , code = 0
                            , text = "Connection error"
                            , json = encodeThe source
                            }
                    in
                    equivalent source wanted
            , test "subscription" <|
                \_ ->
                    let
                        source =
                            """{ "event": "pusher:subscription_error", "channel": "ABC" }"""

                        wanted =
                            { channel = "ABC"
                            , event = SubscriptionError
                            , code = 0
                            , text = "Subscription to ABC failed"
                            , json = encodeThe source
                            }
                    in
                    equivalent source wanted
            ]
        ]
