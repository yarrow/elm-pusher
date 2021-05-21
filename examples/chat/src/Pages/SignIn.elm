module Pages.SignIn exposing (Model, Msg, init, page, update, view)

import Effect exposing (Effect)
import Gen.Params.SignIn exposing (Params)
import Html
import Html.Events as Events
import Page
import Request
import Shared
import View exposing (View)


page : Shared.Model -> Request.With Params -> Page.With Model Msg
page shared _ =
    Page.advanced
        { init = init
        , update = update
        , view = view shared
        , subscriptions = subscriptions
        }



-- INIT


type alias Model =
    {}


init : ( Model, Effect Msg )
init =
    ( {}, Effect.none )



-- UPDATE


type Msg
    = ClickedSignIn


update : Msg -> Model -> ( Model, Effect Msg )
update msg model =
    case msg of
        ClickedSignIn ->
            ( model
            , Effect.fromShared (Shared.SignIn { name = "Yarrow" })
            )



-- SUBSCRIPTIONS


subscriptions : Model -> Sub Msg
subscriptions model =
    Sub.none



-- VIEW


view : Shared.Model -> Model -> View Msg
view shared _ =
    { title = "Sign In"
    , body =
        [ Html.text ("Hello, " ++ shared.uuid)
        , Html.button
            [ Events.onClick ClickedSignIn ]
            [ Html.text "Sign in" ]
        ]
    }
