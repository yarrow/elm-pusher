module DecodeTests exposing
    ( errorReportTests
    , extendedFilterExample
    , fieldDecoders
    , filters
    , memberOperations
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
        , test "The inData decoder reaches into the incoming data field" <|
            \_ ->
                let
                    decoder =
                        D.map WithName (D.map2 Member withUid (inData [ "name" ] D.string))
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
                        D.map Some (D.map2 UidData withUid (withData dataDecoder))
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
                    D.map Some (D.map2 UidData withUid (withData dataDecoder))

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
                            [ D.map Full
                                (D.map4
                                    ChannelEventUidData
                                    withChannel
                                    withEvent
                                    withUid
                                    (withData dataDecoder)
                                )
                                |> channelIs "CBS"
                            , D.map Some (D.map2 UidData withUid (withData dataDecoder))
                                |> eventIs "Halloween"
                            , D.map WTF (D.map identity withEvent) |> channelIs "ABC"
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
    D.map Dog (D.map2 Doghouse (inData [ "color" ] D.string) (inData [ "volume" ] D.int)) |> eventIs "Dog"


type alias CatTree =
    { color : String
    , height : Int
    }


catTree : Decoder PetLair
catTree =
    D.map Cat (D.map2 CatTree (inData [ "color" ] D.string) (inData [ "height" ] D.int) |> eventIs "Cat")


type alias SpiderWeb =
    { strength : Int
    , strands : Int
    }


spiderWeb : Decoder PetLair
spiderWeb =
    D.map Spider (D.map2 SpiderWeb (inData [ "strength" ] D.int) (inData [ "strands" ] D.int) |> eventIs "Spider")


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
                        D.map MemberAddedOnChannel (D.map2 MemberOnChannel withChannel getMember)
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
                        D.map Subscribers (D.map2 MemberData (withMe getMember) (withMembers getMember))

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
