module TestPusher exposing
    ( extendedFilterExample
    , fieldDecoders
    , filters
    , memberOperations
    )

import Expect
import Json.Decode as D exposing (Decoder)
import Json.Decode.Pipeline exposing (required)
import Json.Encode as E
import Pusher exposing (..)
import Test exposing (Test, describe, test)


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
        dataEncoder d =
            E.object [ ( "name", E.string d.name ) ]
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
                        tagMap2 WithName Member withUid (inData "name" D.string)
                in
                D.decodeValue decoder encoded |> Expect.equal (Ok (WithName expectedMember))
        ]


type alias UidData =
    { uid : String
    , data : Data
    }


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
    tagMap2 Dog Doghouse (inData "color" D.string) (inData "volume" D.int) |> eventIs "Dog"


type alias CatTree =
    { color : String
    , height : Int
    }


catTree : Decoder PetLair
catTree =
    tagMap2 Cat CatTree (inData "color" D.string) (inData "height" D.int) |> eventIs "Cat"


type alias SpiderWeb =
    { strength : Int
    , strands : Int
    }


spiderWeb : Decoder PetLair
spiderWeb =
    tagMap2 Spider SpiderWeb (inData "strength" D.int) (inData "strands" D.int) |> eventIs "Spider"


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


memberOperations : Test
memberOperations =
    describe "Member operations" <|
        let
            getMember =
                D.map2 Member withUid (inData "name" D.string)

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
