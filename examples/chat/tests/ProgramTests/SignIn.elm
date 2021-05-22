module ProgramTests.SignIn exposing (all)

import Browser
import Effect
import Html.Styled as Html
import Pages.SignIn
import ProgramTest exposing (ProgramTest, clickButton, expectViewHas)
import Shared
import Test exposing (Test, describe, test)
import Test.Html.Selector exposing (text)
import View exposing (View)


vanilla : (model -> View msg) -> (model -> Browser.Document msg)
vanilla view model =
    view model |> View.toBrowserDocument


start : Shared.Model -> ProgramTest Pages.SignIn.Model Pages.SignIn.Msg (Effect.Effect Pages.SignIn.Msg)
start shared =
    ProgramTest.createDocument
        { init = \_ -> Pages.SignIn.init
        , update = Pages.SignIn.update
        , view = vanilla (Pages.SignIn.view shared)
        }
        |> ProgramTest.start ()


all : Test
all =
    describe "Pages.Login"
        [ test "Polite greeting" <|
            \() ->
                start { uuid = "Jaberwocky", user = Nothing }
                    |> expectViewHas
                        [ text "Hello, Jaberwocky"
                        ]
        ]
