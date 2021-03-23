module Pusher exposing
    ( withChannel, withEvent, withUid, withData, inData
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

@docs withChannel, withEvent, withUid, withData, inData


# Filters

@docs channelIs, eventIs, uidIs


# Tag Maps

@docs tagMap, tagMap2, tagMap4

-}

import Json.Decode as Decode exposing (Decoder, Error(..))


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
withData =
    Decode.field "data"


{-| Sometimes it's more convenient to reach into the `data` field and pull out subfields. (If `data` has only one field, for instance.)

`inData "name" decoder`, for instance, is equivalent to `Decode.at [ "data", "name"] decoder`

-}
inData : String -> Decoder a -> Decoder a
inData subfield =
    Decode.at [ "data", subfield ]


{-| `channelIs "ABC"` fails unless the incoming channel is "ABC"
-}
channelIs : String -> Decoder a -> Decoder a
channelIs channel =
    is "channel" channel


{-| `eventIs "Halloween"` fails unless the incoming event is "Halloween"
-}
eventIs : String -> Decoder a -> Decoder a
eventIs event =
    is "event" event


{-| `uidIs "my.crush"` fails unless the incoming uid is "my.crush"
-}
uidIs : String -> Decoder a -> Decoder a
uidIs uid =
    is "uid" uid


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
