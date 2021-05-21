module Auth exposing
    ( User
    , beforeProtectedInit
    )

{-|

@docs User
@docs beforeProtectedInit

-}

import ElmSpa.Page as ElmSpa
import Gen.Route exposing (Route)
import Request exposing (Request)
import Shared


{-| Replace the "()" with your actual User type
-}
type alias User =
    Shared.User


{-| This function will run before any `protected` pages.
-}
beforeProtectedInit : Shared.Model -> Request -> ElmSpa.Protected User Route
beforeProtectedInit shared _ =
    case shared.user of
        Just user ->
            ElmSpa.Provide user

        Nothing ->
            ElmSpa.RedirectTo Gen.Route.SignIn
