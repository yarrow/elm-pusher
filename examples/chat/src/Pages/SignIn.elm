module Pages.SignIn exposing (Model, Msg, init, page, update, view)

import Effect exposing (Effect)
import Gen.Params.SignIn exposing (Params)
import Html.Styled as Html exposing (Attribute, Html, br, button, div, input, label, p, text)
import Html.Styled.Attributes exposing (attribute, for, id, placeholder, type_, value)
import Html.Styled.Events as Events
import Page
import Request
import Shared
import Validate exposing (Validator, validate)
import View exposing (View)


page : Shared.Model -> Request.With Params -> Page.With Model Msg
page shared _ =
    Page.advanced
        { init = init shared
        , update = update
        , view = view
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
    ( { name = Maybe.withDefault { name = "" } shared.user |> .name
      , password = ""
      , error = Nothing
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
                | name = String.trim model.name
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


update : Msg -> Model -> ( Model, Effect Msg )
update msg model =
    case msg of
        ClickedSignIn ->
            case trimAndValidate model of
                Ok validatedModel ->
                    ( { validatedModel | error = Nothing }
                    , Effect.fromShared (Shared.SignIn { name = model.name })
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


errorView : Maybe String -> Html msg
errorView err =
    case err of
        Nothing ->
            div [] []

        Just complaint ->
            div [ testId "error" ] [ text complaint ]


testId : String -> Attribute msg
testId id =
    attribute "data-cy" id


view : Model -> View Msg
view model =
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
                , errorView model.error
                ]
            ]
        , button
            [ testId "sign-in", Events.onClick ClickedSignIn ]
            [ text "Sign in" ]
        ]
    }
