module TestPusher exposing (suite)

import Expect
import Json.Decode as D
import Json.Decode.Pipeline exposing (required)
import Json.Encode as E
import Pusher exposing (Member, MemberList, decodeMember, decodeMemberList)
import Test exposing (Test, describe, test)


type alias InfoType =
    { name : String }


infoDecoder : D.Decoder InfoType
infoDecoder =
    D.succeed InfoType |> required "name" D.string


encodeMember : Member InfoType -> Member D.Value
encodeMember member =
    { id = member.id, info = E.object [ ( "name", E.string member.info.name ) ] }


jane : Member InfoType
jane =
    { id = "xyz.abc", info = { name = "Jane" } }


encodedJane =
    encodeMember jane


memberWithBadInfo =
    { id = "abc.xyz", info = E.int 2 }


memberList : MemberList InfoType
memberList =
    { me = jane
    , members =
        [ { id = "1", info = { name = "One" } }
        , jane
        , { id = "42", info = { name = "Life, the universe, and everything" } }
        , { id = "3", info = { name = "Three" } }
        ]
    }


encodedMemberList : MemberList D.Value
encodedMemberList =
    { me = encodedJane
    , members = List.map encodeMember memberList.members
    }


badMeInMemberList : MemberList D.Value
badMeInMemberList =
    { encodedMemberList | me = memberWithBadInfo }


badListMemberInMemberList : MemberList D.Value
badListMemberInMemberList =
    let
        m =
            encodedMemberList.members

        oneBad =
            List.concat [ List.take 2 m, [ memberWithBadInfo ], List.drop 2 m ]
    in
    { encodedMemberList | members = oneBad }


suite : Test
suite =
    describe "Pusher tests"
        [ test "decodeMember returns an error when info can't be parsed" <|
            \_ -> decodeMember infoDecoder memberWithBadInfo |> Expect.err
        , test "decodeMember returns Ok with the decoded info when .info is an encoded InfoType" <|
            \_ -> decodeMember infoDecoder encodedJane |> Expect.equal (Ok jane)
        , test "decodeMemberList returns Ok with all .info fields decoded when they are encoded correctly" <|
            \_ -> decodeMemberList infoDecoder encodedMemberList |> Expect.equal (Ok memberList)
        , test "decodeMemberList returns an error when the `me` field can't be parsed" <|
            \_ -> decodeMemberList infoDecoder badMeInMemberList |> Expect.err
        , test "decodeMemberList returns an error when a `members` element can't be parsed" <|
            \_ -> decodeMemberList infoDecoder badListMemberInMemberList |> Expect.err
        ]
