module PacMan exposing (main)

import Browser
import Html exposing (..)



-- Model


type alias Model =
    Int


initialModel : Model
initialModel =
    0



--Update


type Msg
    = Test


update : Msg -> Model -> Model
update msg model =
    case msg of
        Test ->
            model + 1



--View


view : Model -> Html Msg
view model =
    text "PacMan"



-- main


main : Program () Model Msg
main =
    Browser.sandbox { init = initialModel, view = view, update = update }
