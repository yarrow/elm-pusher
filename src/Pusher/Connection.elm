module Pusher.Connection exposing (State(..), fromString, toString, StateChange, stateChange)

{-| Pusher connections ...

@docs State, fromString, toString, StateChange, stateChange

-}

import Json.Decode as Decode exposing (Decoder)
import Pusher exposing (eventIs)


{-| See the [Pusher Connection documentation](https://pusher.com/docs/channels/using_channels/connection#available-states). You can ask a Pusher connection to report when its state changes. The possible states are:

  - Initialized — The connection has not yet been attempted.
  - Connecting — Trying to connect. May be reached from `Initialized` (at startup) or `Connected` (if there is a connection failure).
  - Connected — Connection was successful.
  - Unavailable — The Pusher server can't be reached: internet connection lost, Pusher server down, etc. May be temporary.
  - Failed — The browser doesn't support Pusher.
  - Disconnected — The application has closed the connection.
  - Unknown — The connection state reported by Pusher wasn't one of "initialized", "connecting", "connected", "unavailable", "failed", or "disconnected".

-}
type State
    = Initialized
    | Connecting
    | Connected
    | Unavailable
    | Failed
    | Disconnected
    | Unknown


{-| Change a `String` into a `Connection` `State`. If the given string is not one defined by [Pusher](https://pusher.com/docs/channels/using_channels/connection#available-states), then return `Unknown`.
-}
fromString : String -> State
fromString string =
    case string of
        "initialized" ->
            Initialized

        "connecting" ->
            Connecting

        "connected" ->
            Connected

        "unavailable" ->
            Unavailable

        "failed" ->
            Failed

        "disconnected" ->
            Disconnected

        _ ->
            Unknown


{-| Change a `Connection` `State` into its string representation.
-}
toString : State -> String
toString state =
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

        Unknown ->
            "unknown"


{-| -}
type alias StateChange =
    { previous : State, current : State }


{-| The `stateChange` decoder takes a JSON object with an `event` field of `connection!state_change`, and `previous` and `current` fields that are `String`s. It decodes the object into a `StateChange` record.
-}
stateChange : Decoder StateChange
stateChange =
    let
        decodeState fieldName =
            Decode.map fromString (Decode.field fieldName Decode.string)
    in
    Decode.map2 StateChange (decodeState "previous") (decodeState "current")
        |> eventIs "connection!state_change"
