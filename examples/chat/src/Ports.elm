port module Ports exposing (connect, members)

import Json.Decode exposing (Value)


{-| Connect to Pusher. Required before subscribing to channels.
-}



-- Connection


port connect : Authentication -> Cmd msg


port members : (Value -> msg) -> Sub msg


{-| A record with the name and password to send to the Pusher `authEndpoint`
-}
type alias Authentication =
    { uuid : String
    , name : String
    , password : String
    }
