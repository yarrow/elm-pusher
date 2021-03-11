module Pusher exposing
    ( Error(..)
    , withErrorHandler
    )

import Json.Decode exposing (Decoder, Value, decodeValue)


type Error
    = JsonError Json.Decode.Error
    | Other


withErrorHandler : (Error -> msg) -> Decoder data -> (data -> msg) -> (Value -> msg)
withErrorHandler errorHandler decoder dataHandler =
    \value ->
        case decodeValue decoder value of
            Err error ->
                errorHandler (JsonError error)

            Ok datum ->
                dataHandler datum
