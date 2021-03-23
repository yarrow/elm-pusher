module Pusher exposing
    ( withChannel, withEvent, withUid, withData
    , hasChannel, hasEvent, hasUid, hasData, inData
    , channelIs, eventIs, uidIs
    , tagMap, tagMap2, tagMap4
    )

{-| An Elm interface to [Pusher Channels](https://pusher.com/channels)


# Basic Decoders

When a Pusher event arrives in the browser, your Elm app will see a `Value` (`Json.Decode.Value`) with these fields:

  - `channel` — the Pusher channel that sent the event,
  - `event` — the name of the event,
  - `uid` — the server-generated unique ID for your app's user, and
  - `data` — JSON data (in a format that you, or the server, have decided)

This section contains decoders for the above, in the styple of NoRedInk's `Json.Decode.Pipeline`.

@docs withChannel, withEvent, withUid, withData


# Fnord

@docs hasChannel, hasEvent, hasUid, hasData, inData


# Filters

@docs channelIs, eventIs, uidIs


# Tag Maps

@docs tagMap, tagMap2, tagMap4

-}

import Json.Decode as Decode exposing (Decoder, Error(..))
import Json.Decode.Pipeline exposing (required, requiredAt)


{-| Abbreviation for `Decode.field "channel" Decode.string`
-}
withChannel : Decoder String
withChannel =
    Decode.field "channel" Decode.string


{-| Abbreviation for `Decode.field "event" Decode.string`
-}
withEvent : Decoder String
withEvent =
    Decode.field "event" Decode.string


{-| Abbreviation for `Decode.field "uid" Decode.string`
-}
withUid : Decoder String
withUid =
    Decode.field "uid" Decode.string


{-| `withData decoder` is an abbreviation for `Decode.field "data" decoder`
-}
withData : Decoder data -> Decoder data
withData decoder =
    Decode.field "data" decoder


{-| Abbreviation for `required "channel" Decode.string`
-}
hasChannel : Decoder (String -> a) -> Decoder a
hasChannel =
    required "channel" Decode.string


{-| Abbreviation for `required "event" Decode.string`
-}
hasEvent : Decoder (String -> a) -> Decoder a
hasEvent =
    required "event" Decode.string


{-| Abbreviation for `required "channel" Decode.string`
-}
hasUid : Decoder (String -> a) -> Decoder a
hasUid =
    required "uid" Decode.string


{-| `hasData decoder` is equivalent to `required "data" decoder`
-}
hasData : Decoder a -> Decoder (a -> b) -> Decoder b
hasData =
    required "data"


{-| Sometimes it's more convenient to reach into the `data` field and pull out subfields. (If `data` has only one field, for instance.)

`inData "name" decoder`, for instance, is equivalent to `requiredAt [ "data", "name"] decoder`

-}
inData : String -> Decoder a -> Decoder (a -> b) -> Decoder b
inData subfield =
    requiredAt [ "data", subfield ]


{-| `channelIs "ABC"` fails unless the incoming channel is "ABC"
-}
channelIs : String -> Decoder a -> Decoder a
channelIs channel decoder =
    is "channel" channel decoder


{-| `eventIs "Halloween"` fails unless the incoming event is "Halloween"
-}
eventIs : String -> Decoder a -> Decoder a
eventIs event decoder =
    is "event" event decoder


{-| `uidIs "my.crush"` fails unless the incoming uid is "my.crush"
-}
uidIs : String -> Decoder a -> Decoder a
uidIs uid decoder =
    is "uid" uid decoder


is : String -> String -> Decoder a -> Decoder a
is field valueNeeded decoder =
    let
        fieldIs value =
            if value == valueNeeded then
                decoder

            else
                Decode.fail valueNeeded
    in
    Decode.field field Decode.string |> Decode.andThen fieldIs


{-| -}
tagMap : (value -> msg) -> (a -> value) -> Decoder a -> Decoder msg
tagMap tag constructor a =
    Decode.map tag <| Decode.map constructor a


{-| -}
tagMap2 : (value -> msg) -> (a -> b -> value) -> Decoder a -> Decoder b -> Decoder msg
tagMap2 tag constructor a b =
    Decode.map tag <| Decode.map2 constructor a b


{-| -}
tagMap4 : (value -> msg) -> (a -> b -> c -> d -> value) -> Decoder a -> Decoder b -> Decoder c -> Decoder d -> Decoder msg
tagMap4 tag constructor a b c d =
    Decode.map tag <| Decode.map4 constructor a b c d


{-| -}
tagMap8 : (value -> msg) -> (a -> b -> c -> d -> e -> f -> g -> h -> value) -> Decoder a -> Decoder b -> Decoder c -> Decoder d -> Decoder e -> Decoder f -> Decoder g -> Decoder h -> Decoder msg
tagMap8 tag constructor a b c d e f g h =
    Debug.todo "8"
