module Pages.Home_ exposing (Model, Msg, init, page, update, view)

import Effect exposing (Effect)
import Gen.Params.Home_ exposing (Params)
import Html.Styled as Html
import Page
import Request
import Shared exposing (User)
import View exposing (View)


page : Shared.Model -> Request.With Params -> Page.With Model Msg
page shared _ =
    Page.protected.advanced
        (\user ->
            { init = init
            , update = update
            , view = view user
            , subscriptions = subscriptions
            }
        )



-- INIT


type alias Model =
    {}


init : ( Model, Effect Msg )
init =
    ( {}, Effect.none )



-- UPDATE


type Msg
    = ReplaceMe


update : Msg -> Model -> ( Model, Effect Msg )
update msg model =
    case msg of
        ReplaceMe ->
            ( model, Effect.none )



-- SUBSCRIPTIONS


subscriptions : Model -> Sub Msg
subscriptions model =
    Sub.none



-- VIEW


view : Shared.User -> Model -> View Msg
view user _ =
    { title = "Homepage"
    , body = [ Html.text ("Hello, " ++ user.name) ]
    }
