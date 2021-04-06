module Pusher.ConnectionState exposing (State(..), fromConnectionState, StateChange, stateChange)

{-| Pusher connections ...

@docs State, fromConnectionState, StateChange, stateChange

-}

import Json.Decode as Decode exposing (Decoder)
import Pusher.Decode exposing (eventIs, inData)


{-| See the [Pusher Connection documentation](https://pusher.com/docs/channels/using_channels/connection#available-states). You can ask a Pusher connection to report when its state changes. The possible states are:

  - Initialized — The connection has not yet been attempted.
  - Connecting — Trying to connect. May be reached from `Initialized` (at startup) or `Connected` (if there is a connection failure).
  - Connected — Connection was successful.
  - Unavailable — The Pusher server can't be reached: internet connection lost, Pusher server down, etc. May be temporary.
  - Failed — The browser doesn't support Pusher.
  - Disconnected — The application has closed the connection.

-}
type State
    = Initialized
    | Connecting
    | Connected
    | Unavailable
    | Failed
    | Disconnected


{-| Change a `State` into its string representation.
-}
fromConnectionState : State -> String
fromConnectionState state =
    case state of
        Initialized ->
            "initialized"

        Connecting ->
            "connecting"

        Connected ->
            "connected"

        Unavailable ->
            "unavailable"

        Failed ->
            "failed"

        Disconnected ->
            "disconnected"


{-| -}
type alias StateChange =
    { previous : State, current : State }


{-| The `stateChange` decoder takes a JSON object with an `event` field of `":state_change"`, and `previous` and `current` fields that are `String`s. It decodes the object into a `StateChange` record.
-}
stateChange : Decoder StateChange
stateChange =
    Decode.map2 StateChange (decodeState "previous") (decodeState "current")
        |> eventIs ":state_change"


{-| Decodes a `State` contained in a named field
-}
decodeState : String -> Decoder State
decodeState fieldName =
    let
        stateFromString : String -> Decoder State
        stateFromString string =
            case string of
                "initialized" ->
                    Decode.succeed Initialized

                "connecting" ->
                    Decode.succeed Connecting

                "connected" ->
                    Decode.succeed Connected

                "unavailable" ->
                    Decode.succeed Unavailable

                "failed" ->
                    Decode.succeed Failed

                "disconnected" ->
                    Decode.succeed Disconnected

                _ ->
                    Decode.fail ("Unexpected connection state " ++ string)
    in
    inData [ fieldName ] (Decode.string |> Decode.andThen stateFromString)
