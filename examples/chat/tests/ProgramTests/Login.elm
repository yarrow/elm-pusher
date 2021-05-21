module ProgramTests.Login exposing (all)

import Effect
import Pages.Home_
import ProgramTest exposing (ProgramTest, clickButton, expectViewHas)
import Shared
import Test exposing (Test, describe, test)
import Test.Html.Selector exposing (text)


start : Shared.Model -> ProgramTest Pages.Home_.Model Pages.Home_.Msg (Effect.Effect Pages.Home_.Msg)
start shared =
    ProgramTest.createDocument
        { init = \_ -> Pages.Home_.init
        , update = Pages.Home_.update
        , view = Pages.Home_.view shared
        }
        |> ProgramTest.start ()


all : Test
all =
    describe "Pages.Login"
        [ test "Polite greeting" <|
            \() ->
                start { uuid = "Jaberwocky" }
                    |> expectViewHas
                        [ text "Hello, Jaberwocky"
                        ]
        ]
