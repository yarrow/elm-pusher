module TestPusher exposing (suite)

import Expect
import Json.Decode as D
import Json.Decode.Pipeline exposing (required)
import Json.Encode as E
import Pusher exposing (Member, MemberList, memberDecoder, memberListDecoder)
import Test exposing (Test, describe, test)


type alias InfoType =
    { name : String }


infoDecoder : D.Decoder InfoType
infoDecoder =
    D.succeed InfoType |> required "name" D.string


encodeMember : Member InfoType -> D.Value
encodeMember member =
    E.object
        [ ( "id", E.string member.id )
        , ( "info", E.object [ ( "name", E.string member.info.name ) ] )
        ]


jane : Member InfoType
jane =
    { id = "xyz.abc", info = { name = "Jane" } }


badMember : D.Value
badMember =
    E.object
        [ ( "id", E.string "abc" )
        , ( "info", E.int 42 )
        ]


memberList : MemberList InfoType
memberList =
    { me = jane
    , members =
        [ { id = "1", info = { name = "One" } }
        , jane
        , { id = "2", info = { name = "Two" } }
        , { id = "3", info = { name = "Three" } }
        ]
    }


encodedMemberList =
    E.object
        [ ( "me", encodeMember memberList.me )
        , ( "members", E.list encodeMember memberList.members )
        ]


badMemberList =
    E.object
        [ ( "me", badMember )
        , ( "members", E.list encodeMember memberList.members )
        ]


decodeMember =
    D.decodeValue (memberDecoder infoDecoder)


decodeMemberList =
    D.decodeValue (memberListDecoder infoDecoder)


suite : Test
suite =
    describe "Pusher tests"
        [ test "decodeMember returns an error when its argument can't be parsed" <|
            \_ -> decodeMember badMember |> Expect.err
        , test "decodeMember returns Ok when the argument is a Member" <|
            \_ -> decodeMember (encodeMember jane) |> Expect.equal (Ok jane)
        , test "decodeMemberList returns Ok with all .info fields decoded when they are encoded correctly" <|
            \_ -> decodeMemberList encodedMemberList |> Expect.equal (Ok memberList)
        , test "decodeMemberList returns an error when the `me` field can't be parsed" <|
            \_ -> decodeMemberList badMemberList |> Expect.err
        ]
