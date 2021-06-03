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
        (\_ ->
            { init = init
            , update = update
            , view = view shared
            , subscriptions = \_ -> Sub.none
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



-- VIEW


view : Shared.Model -> Model -> View Msg
view shared _ =
    { title = "Gather"
    , body = [ Html.text ("Present: " ++ shared.memberList) ]
    }
