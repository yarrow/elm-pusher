module Pusher exposing (Member, decodeMember)

{-| An Elm interface to [Pusher Channels](https://pusher.com/channels)


# Incoming Data

Pusher channels can transport arbitrary JSON data, so our Pusher datatypes will generally be only half-parsed, containing `Json.Decode.Value` fields that you must decode yourself. What the Pusher package provides is the boilerplate fields, so to speak: a way to identify the type of event that's being passed in. For Pusher errors, we can go a little further and show a more precise error type.

When you see `Value` below, please mentally substitute `Json.Decode.Value`. Your author doesn't know how to make `Json.Decode.Value` show up fully specified in the documentation.

@docs Member, decodeMember

-}

import Json.Decode exposing (Decoder, Error, Value, decodeValue)


{-| Pusher's `Member` objects have an `id` field which is a `String` and an `info` field that can hold arbitrary data. Your application will send that `info` data on login, and receive it again as membership information.
-}
type alias Member infoType =
    { id : String
    , info : infoType
    }


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
