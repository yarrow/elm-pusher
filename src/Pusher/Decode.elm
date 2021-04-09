module Pusher.Decode exposing
    ( withChannel, withEvent, withUid, withData, inData
    , channelIs, eventIs, uidIs
    , tagMap, tagMap2, tagMap3, tagMap4, tagMap5, tagMap6, tagMap7, tagMap8
    , ErrorReport, ErrorKind(..), errorReport, messageFor
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


# Error Reports

Pusher subscription errors are received as `pusher:subscription_error` events on the channel where subscription was attempted.

Pusher also reports [connection errors](https://pusher.com/docs/channels/library_auth_reference/pusher-websockets-protocol#connection-closure) (when the internet conection has been interupted, for instance, or when we haven't set up our Pusher connection information properly.) We report those as if they were events, on the fake `:connection` channel. (`:connection` is not a legal channel name.)

@docs ErrorReport, ErrorKind, errorReport, messageFor

-}

import Json.Decode as Decode exposing (Decoder, Error(..), int, maybe, string)


{-| Abbreviation for `Decode.field "channel" Decode.string`
-}
withChannel : Decoder String
withChannel =
    Decode.field "channel" string


{-| Abbreviation for `Decode.field "event" Decode.string`
-}
withEvent : Decoder String
withEvent =
    Decode.field "event" string


{-| Abbreviation for `Decode.field "uid" Decode.string`
-}
withUid : Decoder String
withUid =
    Decode.field "uid" string


{-| `withData decoder` is an abbreviation for `Decode.field "data" decoder`
-}
withData : Decoder data -> Decoder data
withData =
    Decode.field "data"


{-| Sometimes it's more convenient to reach into the `data` field and pull out subfields. (If `data` has only one field, for instance.)

`inData ["name"] decoder`, for instance, is equivalent to `Decode.at [ "data", "name"] decoder`

-}
inData : List String -> Decoder a -> Decoder a
inData subfields =
    Decode.at ("data" :: subfields)


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
    Decode.field field string |> Decode.andThen fieldIs


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


{-| -}
type ErrorKind
    = ConnectionError
    | SubscriptionError


{-| -}
type alias ErrorReport =
    { channel : String
    , event : ErrorKind
    , text : Maybe String
    , code : Maybe Int
    }


{-| We want to give our callers a simpler interface than Pusher gives us.
`toError` tries to handle generalized versions of the Pusher errors I've seen,
with a "throw up our hands and just show the user some JSON" final fallback.

Here are some example errors from the overall Pusher connection:

  - {type: "WebSocketError", error: {isTrusted: true}}
  - {type: "PusherError", data: {code: 1006}}
  - {type: "WebSocketError", error: {type: "PusherError", data: {code: 4001, message: "App key ..."}}}

The `code` field of these errors, when present, should be in the 1000s or
4000s: 1000s for general WebSocket errors, 4000s for Pusher-specific errors.

And here's a pusher:subscription\_error:

  - type: "AuthError", error: "Unable to retrieve auth ...", status: 401}

The `status` field, if present, is an HTTP return code (so less than 1000 and
distinct from the WebSocket codes).

So: we attempt to return a meaningful error number in the `code` field, and a
meaningful error message in the `text` field.

The other option would be to return a custom type with a bunch of very similar
variants. Seems better to simplify it here than make our callers do giant case
statements.

-}
fallback : ErrorKind -> Decoder ErrorReport
fallback event =
    Decode.map4 ErrorReport
        withChannel
        (Decode.succeed event)
        (Decode.succeed Nothing)
        (Decode.succeed Nothing)


has : List String -> Decoder a -> Decoder a
has path decoder =
    inData path string |> Decode.andThen (\_ -> decoder)


{-| -}
connectionError : Decoder ErrorReport
connectionError =
    eventIs ":connection_error" <|
        Decode.oneOf
            [ has [ "error", "type" ] <|
                Decode.map4
                    ErrorReport
                    -- value of the form
                    -- {type: "WebSocketError", error: {type: "PusherError", ... }}
                    withChannel
                    (Decode.succeed ConnectionError)
                    (maybe (inData [ "error", "data", "message" ] string))
                    (maybe (inData [ "error", "data", "code" ] int))
            , has [ "type" ] <|
                Decode.map4 ErrorReport
                    -- value of the form {type: "PusherError", data: { ... }}
                    withChannel
                    (Decode.succeed ConnectionError)
                    (maybe (inData [ "data", "message" ] string))
                    (maybe (inData [ "data", "code" ] int))
            , fallback ConnectionError
            ]


{-| -}
subscriptionError : Decoder ErrorReport
subscriptionError =
    eventIs "pusher:subscription_error" <|
        Decode.oneOf
            [ has [ "type" ] <|
                Decode.map4 ErrorReport
                    withChannel
                    (Decode.succeed SubscriptionError)
                    (maybe (inData [ "error" ] string))
                    (maybe (inData [ "status" ] int))
            , fallback SubscriptionError
            ]


{-| -}
errorReport : Decoder ErrorReport
errorReport =
    Decode.oneOf [ subscriptionError, connectionError ]


{-| -}
messageFor : ErrorReport -> String
messageFor report =
    case report.event of
        SubscriptionError ->
            let
                subscriptionFailed =
                    [ "Subscription to ", report.channel, " failed" ]

                why =
                    case report.code of
                        Just status ->
                            [ ": HTTP status ", String.fromInt status ]

                        Nothing ->
                            []
            in
            List.concat [ subscriptionFailed, why ] |> String.concat

        _ ->
            case report.text of
                Just msg ->
                    msg

                Nothing ->
                    let
                        code =
                            Maybe.withDefault 0 report.code

                        status =
                            case report.code of
                                Nothing ->
                                    ""

                                Just n ->
                                    String.concat [ " (", String.fromInt n, ")" ]
                    in
                    if 1000 <= code && code < 2000 then
                        -- A WebSocket connection error
                        -- See https://developer.mozilla.org/en-US/docs/Web/API/CloseEvent
                        "Error connecting to the internet" ++ status

                    else if 4000 <= code && code < 5000 then
                        -- A Pusher connection error
                        -- See https://pusher.com/docs/channels/library_auth_reference/pusher-websockets-protocol#error-codes
                        "Error connecting to Pusher" ++ status

                    else if 2000 <= code && code < 4000 then
                        -- WebSocket codes reserved for extensions and libararies
                        "Connection error" ++ status

                    else
                        "Connection error" ++ status
