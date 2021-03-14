module Pusher exposing (Error(..), withErrorHandler)

{-| An Elm interface to [Pusher Channels](https://pusher.com/channels)


# Error Handling

@docs Error, withErrorHandler

[pc]: https://pusher.com/channels

-}

import Json.Decode exposing (Decoder, Value, decodeValue)


{-| An Elm Pusher application needs to handle two categories of errors: those from Pusher itself, usually indicating some network error, and JSON errors, usually indicating that the interface between Elm and JavaScript has gone awry.

The `JsonError` variant handles the latter, and includes the `Json.Decode.Error` from the JSON decoder.

-}
type Error
    = JsonError Json.Decode.Error
    | Other


{-| Since every event can have a different data type, every event must have a handler that deals with a JSON error. The `withErrorHandler` function allows you to use a single errror hander for all errors, Pusher errors as well as JSON errors. The idea is to bundle your error handler with ...

    makeHandler =
        withErrorHandler myErrorHandler

    xHandler =
        makeHandler decodeX xToMsg

-}
withErrorHandler : (Error -> msg) -> Decoder data -> (data -> msg) -> (Value -> msg)
withErrorHandler errorHandler decoder dataHandler =
    \value ->
        case decodeValue decoder value of
            Err error ->
                errorHandler (JsonError error)

            Ok datum ->
                dataHandler datum
