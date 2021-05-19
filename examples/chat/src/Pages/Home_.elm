module Pages.Home_ exposing (page)

import Html
import Page exposing (Page)
import Request exposing (Request)
import Shared
import View exposing (View)


page : Shared.Model -> Request -> Page
page shared _ =
    Page.static
        { view = view shared
        }


view : Shared.Model -> View msg
view shared =
    { title = "Homepage"
    , body = [ Html.text ("Hello, " ++ shared.uuid) ]
    }
