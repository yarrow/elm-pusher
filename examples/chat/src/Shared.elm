module Shared exposing
    ( Flags
    , Model
    , Msg(..)
    , User
    , init
    , subscriptions
    , update
    , userName
    )

import Dict exposing (Dict)
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
        name =
            inData [ "name" ] Json.string
    in
    Json.map2 User withUid name


userData : Decoder UserData
userData =
    Json.map2 UserData (withMe userDecoder) (withMembers userDecoder)


type alias Model =
    { uuid : String
    , user : Maybe User
    , members : Dict String User
    , memberList : String
    , error : Maybe String
    }


userName : Model -> Maybe String
userName model =
    Maybe.map .name model.user


type Msg
    = GotUsers UserData
    | MemberAdded User
    | GotError ErrorReport
    | JsonError Json.Error
    | SignOut


init : Request -> Flags -> ( Model, Cmd Msg )
init _ uuid =
    ( { uuid = uuid, error = Nothing, user = Nothing, members = Dict.empty, memberList = "" }, Cmd.none )


update : Request -> Msg -> Model -> ( Model, Cmd Msg )
update req msg model =
    case msg of
        SignOut ->
            ( { model | user = Nothing }
            , Cmd.none
            )

        GotUsers users ->
            let
                keyedMember m =
                    ( m.uid, m )

                memberDict mlist =
                    Dict.fromList (List.map keyedMember mlist)

                user =
                    Just users.me

                members =
                    memberDict users.members

                memberList =
                    List.sort (List.map .name (Dict.values members)) |> String.join ", "
            in
            ( { model | user = user, members = members, memberList = memberList }
            , Request.pushRoute Gen.Route.Home_ req
            )

        MemberAdded member ->
            ( { model | members = Dict.insert member.uid member model.members }
            , Cmd.none
            )

        GotError report ->
            if report.event == SubscriptionError then
                let
                    complaint =
                        if report.code == 401 then
                            "Password did not match"

                        else
                            report.text
                in
                ( { model | user = Nothing, error = Just complaint }, Cmd.none )

            else
                ( { model | error = Just report.text }, Cmd.none )

        JsonError error ->
            ( { model | error = Just (Json.errorToString error) }, Cmd.none )


decodePusher : Json.Value -> Msg
decodePusher value =
    let
        decode =
            Json.oneOf
                [ Json.map GotUsers userData
                , Json.map GotError errorReport
                , Json.map MemberAdded userDecoder
                ]
    in
    case Json.decodeValue decode value of
        Ok msg ->
            msg

        Err error ->
            JsonError error


subscriptions : Request -> Model -> Sub Msg
subscriptions _ _ =
    Ports.pusher decodePusher
