module Pages.SignIn exposing (Model, Msg, init, page, update, view)

import Effect exposing (Effect)
import Gen.Params.SignIn exposing (Params)
import Html.Styled as Html exposing (button, div, input, label, p, text)
import Html.Styled.Attributes exposing (for, id, placeholder, value)
import Html.Styled.Events as Events
import Page
import Request
import Shared
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
    }


init : Shared.Model -> ( Model, Effect Msg )
init shared =
    ( { name = Maybe.withDefault { name = "" } shared.user |> .name }, Effect.none )



-- UPDATE


type Msg
    = ClickedSignIn
    | UpdateName String


update : Msg -> Model -> ( Model, Effect Msg )
update msg model =
    case msg of
        ClickedSignIn ->
            ( model
            , Effect.fromShared (Shared.SignIn { name = model.name })
            )

        UpdateName name ->
            ( { model | name = name }, Effect.none )



-- SUBSCRIPTIONS


subscriptions : Model -> Sub Msg
subscriptions _ =
    Sub.none



-- VIEW


view : Model -> View Msg
view model =
    { title = "Sign In"
    , body =
        [ div []
            [ p []
                [ label [ for "Name" ] [ text "Name: " ]
                , input
                    [ placeholder "Your name"
                    , value model.name
                    , Events.onInput UpdateName
                    , id "Name"
                    ]
                    []
                ]
            ]
        , button
            [ Events.onClick ClickedSignIn ]
            [ text "Sign in" ]
        ]
    }
