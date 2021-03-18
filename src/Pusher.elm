module Pusher exposing (Member, EncodedMember, MemberList, EncodedMemberList, decodeMember, decodeMemberList)

{-| An Elm interface to [Pusher Channels](https://pusher.com/channels)


# Incoming Data

Pusher channels can transport arbitrary JSON data, so our Pusher datatypes will generally be only half-parsed, containing `Json.Decode.Value` fields that you must decode yourself. What the Pusher package provides is the boilerplate fields, so to speak: a way to identify the type of event that's being passed in. For Pusher errors, we can go a little further and show a more precise error type.

When you see `Value` below, please mentally substitute `Json.Decode.Value`. Your author doesn't know how to make `Json.Decode.Value` show up fully specified in the documentation.

@docs Member, EncodedMember, MemberList, EncodedMemberList, decodeMember, decodeMemberList

-}

import Json.Decode as Decode exposing (Decoder, Error(..), Value, decodeValue)


{-| Pusher's `Member` objects have an `id` field which is a `String` and an `info` field that can hold arbitrary data. Your application will send that `info` data on login, and receive it again as membership information.
-}
type alias Member infoType =
    { id : String
    , info : infoType
    }


{-| An `EncodedMember` is a `Member Json.Decode.Value`.

When we send a `Member` object to, or receive a `Member` object from, the Pusher server, the object's `info` field can be any JSON-representable type. We represent that in Elm as a record with a `Json.Decode.Value` in its `info` field.

-}
type alias EncodedMember =
    Member Decode.Value


{-| When you subscribe to a Pusher Presence channel, you'll get back a `MemberList` — which is not a simple `List Member` but a record with fields `me` (the current logged in user), and `members` (a list of all users, which is indeed a `List Member`).
-}
type alias MemberList infoType =
    { me : Member infoType
    , members : List (Member infoType)
    }


{-| An `EncodedMemberList` is a `MemberList Json.Decode.Value`
-}
type alias EncodedMemberList =
    MemberList Decode.Value


{-| The `decodeMember` function takes a JSON decoder for some `infoType` and a `Member Json.Decode.Value` and returns a `Result Json.Decoder.Error (Member infoType)`.

For instance:

    import Json.Decode as Decode
    import Json.Encode as Encode

    let
        encodedMember = -- as it might come from Pusher
            { id = "abc.xyz", info = Encode.string "pi > 3" }
        infoDecoder = Decode.string
    in
    decodeMember infoDecoder encodedMember

    --> Ok { id = "abc.xyz"
    -->    , info = "pi > 3"
    -->    }

-}
decodeMember : Decoder infoType -> Member Value -> Result Error (Member infoType)
decodeMember infoDecoder member =
    decodeValue infoDecoder member.info
        |> Result.map (\decoded -> { id = member.id, info = decoded })


memberDecoder : Decoder infoType -> Decoder (Member infoType)
memberDecoder decoder =
    Decode.map2 Member
        (Decode.field "id" Decode.string)
        (Decode.field "info" decoder)


{-| The `decodeMemberList` function takes a JSON decoder for some `infoType` and a `MemberList Json.Decode.Value` and returns a `Result Json.Decoder.Error (MemberList infoType)`.

For instance:

    import Json.Decode as Decode
    import Json.Encode as Encode

    let
        encodeMember: Int -> Member Decode.Value
        encodeMember n =
            { id = "#" ++ (String.fromInt n), info = Encode.int n }
        encodedMemberList = -- as it might come from Pusher
            { me = encodeMember 1
            , members = [encodeMember 0, encodeMember 1, encodeMember 2]
            }
        infoDecoder = Decode.int
    in
    decodeMemberList infoDecoder encodedMemberList

    --> Ok { me = { id = "#1", info = 1 }
    -->    , members =
    -->       [ { id = "#0", info = 0 }
    -->       , { id = "#1", info = 1 }
    -->       , { id = "#2", info = 2 }
    -->       ]
    -->    }

-}
decodeMemberList : Decoder infoType -> MemberList Value -> Result Error (MemberList infoType)
decodeMemberList infoDecoder memberList =
    let
        decode =
            decodeMember infoDecoder

        maybeMe =
            Result.mapError (\error -> Field "me" error) (decode memberList.me)

        memberAt n codedMember =
            Result.mapError (\error -> Field "members" (Index n error)) (decode codedMember)

        maybeMembers =
            List.indexedMap memberAt memberList.members
                |> List.foldr (Result.map2 (::)) (Ok [])
    in
    case ( maybeMe, maybeMembers ) of
        ( Ok me, Ok members ) ->
            Ok { me = me, members = members }

        ( Err badMe, _ ) ->
            Err badMe

        ( _, Err badMember ) ->
            Err badMember
