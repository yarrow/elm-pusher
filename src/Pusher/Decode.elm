module Pusher.Decode exposing
    ( withChannel, withEvent, withUid, withData, inData
    , channelIs, eventIs, uidIs
    , tagMap, tagMap2, tagMap3, tagMap4, tagMap5, tagMap6, tagMap7, tagMap8
    )

{-| An Elm interface to [Pusher Channels](https://pusher.com/channels)

This package provides three things:

  - a convention (implemented by the NPM package **FIXME: INSERT NPM INFO HERE!!**) for communication between Elm and Pusher.js
  - A thin wrapper around `Json.Decode` to simplify decoding Pusher messages
  - Some specific decoders for Pusher Presence channel messages, connection messages, and error messages.


# Basic Decoders

When a Pusher message arrives in the browser, your Elm app will see a `Json.Decode.Value` with these fields:

  - `channel` — the Pusher channel that sent the event,
  - `event` — the name of the event,
  - `uid` — the server-generated unique ID for your app's user, and
  - `data` — JSON data (in a format that you, or the server, have decided)

The `event` field is always present, and the data field is always present with one exception. (See **FIXME: SUBSCRIPTION SUCCEEDED**) Depending on the type of channels you use and what you've asked Pusher to give you, `channel` and `uid` may be missing.

@docs withChannel, withEvent, withUid, withData, inData


# Filters

The `channel` and `event` fields are strings, so you could use them, for instance, as keys for in a `Dict`. But if you have only a few `event` values, for instance, you might prefer to use a custom type rather than strings to distinguish them. For instance:

        Decode.oneOf
            [ Decode.map Dog (withData dogHouse) |> eventIs "Dog"
            , Decode.map Cat (withData catTree) |> eventIs "Cat"
            , Decode.map Spider (withData web) |> eventIs "Spider"
            ]

@docs channelIs, eventIs, uidIs


# Tag Maps

**FIXME: DOCUMENT ME!**

@docs tagMap, tagMap2, tagMap3, tagMap4, tagMap5, tagMap6, tagMap7, tagMap8

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
tagMap3 : (value -> msg) -> (a -> b -> c -> value) -> Decoder a -> Decoder b -> Decoder c -> Decoder msg
tagMap3 tag constructor a b c =
    Decode.map tag <| Decode.map3 constructor a b c


{-| -}
tagMap4 : (value -> msg) -> (a -> b -> c -> d -> value) -> Decoder a -> Decoder b -> Decoder c -> Decoder d -> Decoder msg
tagMap4 tag constructor a b c d =
    Decode.map tag <| Decode.map4 constructor a b c d


{-| -}
tagMap5 : (value -> msg) -> (a -> b -> c -> d -> e -> value) -> Decoder a -> Decoder b -> Decoder c -> Decoder d -> Decoder e -> Decoder msg
tagMap5 tag constructor a b c d e =
    Decode.map tag <| Decode.map5 constructor a b c d e


{-| -}
tagMap6 : (value -> msg) -> (a -> b -> c -> d -> e -> f -> value) -> Decoder a -> Decoder b -> Decoder c -> Decoder d -> Decoder e -> Decoder f -> Decoder msg
tagMap6 tag constructor a b c d e f =
    Decode.map tag <| Decode.map6 constructor a b c d e f


{-| -}
tagMap7 : (value -> msg) -> (a -> b -> c -> d -> e -> f -> g -> value) -> Decoder a -> Decoder b -> Decoder c -> Decoder d -> Decoder e -> Decoder f -> Decoder g -> Decoder msg
tagMap7 tag constructor a b c d e f g =
    Decode.map tag <| Decode.map7 constructor a b c d e f g


{-| -}
tagMap8 : (value -> msg) -> (a -> b -> c -> d -> e -> f -> g -> h -> value) -> Decoder a -> Decoder b -> Decoder c -> Decoder d -> Decoder e -> Decoder f -> Decoder g -> Decoder h -> Decoder msg
tagMap8 tag constructor a b c d e f g h =
    Decode.map tag <| Decode.map8 constructor a b c d e f g h
