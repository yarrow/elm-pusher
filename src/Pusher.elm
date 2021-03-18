module Pusher exposing (Member, MemberList, memberDecoder, memberListDecoder)

{-| An Elm interface to [Pusher Channels](https://pusher.com/channels)


# Incoming Data Decoders

Pusher channels can transport arbitrary JSON data, but there are also regularities. The functions in this section take your `Json.Decoder` functions for the arbitrary parts and turn them into decoders incoming Pusher data.

@docs Member, MemberList, memberDecoder, memberListDecoder

-}

import Json.Decode as Decode exposing (Decoder, Error(..))


{-| Pusher's `Member` objects have an `id` field which is a `String` and an `info` field that can hold arbitrary data. Your application will send that `info` data on login, and receive it again as membership information.
-}
type alias Member infoType =
    { id : String
    , info : infoType
    }


{-| The `memberDecoder` function takes a decoder for `InfoType` and turns it into a decoder for `Member InfoType`.
-}
memberDecoder : Decoder infoType -> Decoder (Member infoType)
memberDecoder decoder =
    Decode.map2 Member
        (Decode.field "id" Decode.string)
        (Decode.field "info" decoder)


{-| When you subscribe to a Pusher Presence channel, you'll get back a `MemberList` — which is not a simple `List Member` but a record with fields `me` (the current logged in user), and `members` (a list of all users, which is indeed a `List Member`).
-}
type alias MemberList infoType =
    { me : Member infoType
    , members : List (Member infoType)
    }


{-| The `memberListDecoder` function takes a decoder for `InfoType` and turns it into a decoder for `MemberList InfoType`.
-}
memberListDecoder : Decoder infoType -> Decoder (MemberList infoType)
memberListDecoder decoder =
    let
        member =
            memberDecoder decoder
    in
    Decode.map2 MemberList
        (Decode.field "me" member)
        (Decode.field "members" (Decode.list member))
