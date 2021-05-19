module Shared exposing
    ( Flags
    , Model
    , Msg
    , init
    , subscriptions
    , update
    )

import Json.Decode as Json
import Request exposing (Request)


type alias Flags =
    String



-- should be Json.Value


type alias Model =
    { uuid : String }


type Msg
    = NoOp


init : Request -> Flags -> ( Model, Cmd Msg )
init _ uuid =
    ( { uuid = uuid }, Cmd.none )


update : Request -> Msg -> Model -> ( Model, Cmd Msg )
update _ msg model =
    case msg of
        NoOp ->
            ( model, Cmd.none )


subscriptions : Request -> Model -> Sub Msg
subscriptions _ _ =
    Sub.none
