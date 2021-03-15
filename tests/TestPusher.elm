module TestPusher exposing (suite)

import Expect
import Json.Decode as D
import Json.Decode.Pipeline exposing (required)
import Json.Encode as E
import Pusher exposing (Member, decodeMember)
import Test exposing (Test, describe, test)


type alias InfoType =
    { name : String }


jane : Member InfoType
jane =
    { id = "xyz.abc", info = { name = "Jane" } }


encodeMember : Member InfoType -> Member D.Value
encodeMember member =
    { id = member.id, info = E.object [ ( "name", E.string member.info.name ) ] }


infoDecoder : D.Decoder InfoType
infoDecoder =
    D.succeed InfoType |> required "name" D.string


suite : Test
suite =
    let
        badInfo =
            { id = "xzy.abc", info = E.int 2 }

        encodedJane =
            encodeMember jane
    in
    describe "Pusher tests"
        [ test "decodeMember returns an error when info can't be parsed" <|
            \_ -> decodeMember infoDecoder badInfo |> Expect.err
        , test "decodeMember returns a Result with the encoded info when .info is an encoded InfoType" <|
            \_ -> decodeMember infoDecoder encodedJane |> Expect.equal (Ok jane)
        ]
