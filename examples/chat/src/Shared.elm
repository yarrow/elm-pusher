module Shared exposing
    ( Flags
    , Model
    , Msg(..)
    , User
    , init
    , subscriptions
    , update
    )

import Gen.Route
import Json.Decode as Json exposing (Decoder)
import Ports
import Pusher.Decode exposing (ErrorEvent(..), ErrorReport, errorReport, inData, withUid)
import Pusher.Membership exposing (withMe, withMembers)
import Request exposing (Request)


{-| Should be Json.Value, but a simple String seems safe enough
-}
type alias Flags =
    String


type alias User =
    { uid : String
    , name : String
    }


type alias UserData =
    { me : User
    , members : List User
    }


userDecoder : Decoder User
userDecoder =
    let
        userName =
            inData [ "name" ] Json.string
    in
    Json.map2 User withUid userName


userData : Decoder UserData
userData =
    Json.map2 UserData (withMe userDecoder) (withMembers userDecoder)


type alias Model =
    { uuid : String
    , user : Maybe User
    }


type Msg
    = GotUsers UserData
    | GotError ErrorReport
    | JsonError Json.Error
    | SignOut


init : Request -> Flags -> ( Model, Cmd Msg )
init _ uuid =
    ( { uuid = uuid, user = Nothing }, Cmd.none )


update : Request -> Msg -> Model -> ( Model, Cmd Msg )
update req msg model =
    case msg of
        SignOut ->
            ( { model | user = Nothing }
            , Cmd.none
            )

        GotUsers users ->
            ( { model | user = Just users.me }
            , Request.pushRoute Gen.Route.Home_ req
            )

        GotError report ->
            ( model, Cmd.none )

        JsonError error ->
            ( model, Cmd.none )


decodePusher : Json.Value -> Msg
decodePusher value =
    let
        decode =
            Json.oneOf
                [ Json.map GotUsers userData
                , Json.map GotError errorReport
                ]
    in
    case Json.decodeValue decode value of
        Ok msg ->
            msg

        Err error ->
            JsonError error


subscriptions : Request -> Model -> Sub Msg
subscriptions _ _ =
    Ports.members decodePusher
