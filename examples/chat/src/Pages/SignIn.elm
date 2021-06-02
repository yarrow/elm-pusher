module Pages.SignIn exposing (Model, Msg, init, page, update, view)

import Effect exposing (Effect, fromCmd)
import Gen.Params.SignIn exposing (Params)
import Html.Styled as Html exposing (Attribute, Html, br, button, div, input, label, p, text)
import Html.Styled.Attributes exposing (attribute, for, id, placeholder, type_, value)
import Html.Styled.Events as Events
import Page
import Ports
import Request
import Shared
import String.Extra exposing (toTitleCase)
import Validate exposing (Validator, validate)
import View exposing (View)


page : Shared.Model -> Request.With Params -> Page.With Model Msg
page shared _ =
    Page.advanced
        { init = init shared
        , update = update shared
        , view = view shared
        , subscriptions = subscriptions
        }



-- INIT


type alias Model =
    { name : String
    , password : String
    , error : Maybe String
    }


init : Shared.Model -> ( Model, Effect Msg )
init shared =
    ( { name = Shared.userName shared |> Maybe.withDefault ""
      , password = ""
      , error = shared.error
      }
    , Effect.none
    )



-- VALIDATION


modelValidator : Validator String Model
modelValidator =
    Validate.firstError
        [ Validate.ifBlank .name "Please type your name"
        , Validate.ifBlank .password "Please enter the password"
        ]


trimAndValidate : Model -> Result String Model
trimAndValidate model =
    let
        trimmedModel =
            { model
                | name = toTitleCase (String.trim model.name)
                , password = String.trim model.password
            }

        result =
            validate modelValidator trimmedModel
    in
    case result of
        Ok trimmed ->
            Ok (Validate.fromValid trimmed)

        Err errors ->
            Err (String.join "; " errors)



-- UPDATE


type Msg
    = ClickedSignIn
    | NameChanged String
    | PasswordChanged String


update : Shared.Model -> Msg -> Model -> ( Model, Effect Msg )
update shared msg model =
    case msg of
        ClickedSignIn ->
            case trimAndValidate model of
                Ok validatedModel ->
                    ( { validatedModel | error = Nothing }
                    , Ports.connect
                        { name = validatedModel.name
                        , password = validatedModel.password
                        , uuid = shared.uuid
                        }
                        |> fromCmd
                    )

                Err error ->
                    ( { model | error = Just error }
                    , Effect.none
                    )

        NameChanged name ->
            ( { model | name = name }, Effect.none )

        PasswordChanged password ->
            ( { model | password = password }, Effect.none )



-- SUBSCRIPTIONS


subscriptions : Model -> Sub Msg
subscriptions _ =
    Sub.none



-- VIEW


errorView : Maybe String -> Maybe String -> Html msg
errorView shared local =
    let
        err =
            if local == Nothing then
                shared

            else
                local
    in
    case err of
        Nothing ->
            div [] []

        Just complaint ->
            div [ testId "error" ] [ text complaint ]


testId : String -> Attribute msg
testId id =
    attribute "data-cy" id


view : Shared.Model -> Model -> View Msg
view shared model =
    { title = "Sign In"
    , body =
        [ div []
            [ p []
                [ label [ for "name" ] [ text "Name: " ]
                , input
                    [ placeholder "Your name"
                    , value model.name
                    , Events.onInput NameChanged
                    , id "name"
                    , testId "name"
                    ]
                    []
                , br [] []
                , label [ for "password" ] [ text "Password: " ]
                , input
                    [ type_ "password"
                    , placeholder "Password"
                    , value model.password
                    , Events.onInput PasswordChanged
                    , id "password"
                    , testId "password"
                    ]
                    []
                , errorView shared.error model.error
                ]
            ]
        , button
            [ testId "sign-in", Events.onClick ClickedSignIn ]
            [ text "Sign in" ]
        ]
    }
