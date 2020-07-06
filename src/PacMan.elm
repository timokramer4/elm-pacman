module PacMan exposing (main)

import Browser
import Browser.Events exposing (onKeyDown)
import Dict exposing (Dict, member)
import Eatable exposing (..)
import Html exposing (Html, div, img, node, text)
import Html.Attributes exposing (class, height, id, src, style, width)
import Json.Decode exposing (..)
import List exposing (..)
import List.Unique exposing (filterDuplicates)
import Movement exposing (..)
import Settings exposing (..)
import String exposing (toInt)
import Style exposing (..)
import Svg exposing (line, path, polygon, svg)
import Svg.Attributes exposing (d, fill, points, stroke, strokeWidth, x1, x2, y1, y2)
import Time exposing (every)
import Types.GameModels exposing (..)
import Types.Ghost exposing (..)
import Types.Line exposing (LineType(..))
import Types.Point exposing (Point)



----------
-- INIT --
----------


initialModel : Game
initialModel =
    { pPosition = { x = 250, y = 280 }
    , state = Running Right
    , nextDir = Right
    , pRotation = 0
    , score = 0
    , items = substractList pillsList (filterDuplicates (List.foldl createPoints [] (Dict.values runMesh)))
    , pills = pillsList
    , itemCounter = 0
    , secondCounter = 0
    , fruitAvailable = False
    , redGhost = { position = { x = 250, y = 190 }, dir = None, lastDir = None }
    , pinkGhost = { position = { x = 250, y = 235 }, dir = None, lastDir = None }
    , blueGhost = { position = { x = 220, y = 235 }, dir = None, lastDir = None }
    , yellowGhost = { position = { x = 280, y = 235 }, dir = None, lastDir = None }
    }



------------
-- UPDATE --
------------


update : Msg -> Game -> ( Game, Cmd Msg )
update msg game =
    case msg of
        MoveDirection d ->
            case d of
                Left ->
                    if outOfBounds game then
                        ( { game | pPosition = changeXPosition fieldSettings.width game, state = Running d, pRotation = 180 }, Cmd.none )

                    else if checkDir game.pPosition d Pacman || checkDir game.pPosition game.nextDir Pacman then
                        if checkDir game.pPosition game.nextDir Pacman && game.nextDir /= d then
                            ( { game | state = Running game.nextDir, nextDir = None }, Cmd.none )

                        else
                            ( checkEatable { game | pPosition = changeXPosition (game.pPosition.x - movement) game, state = Running d, pRotation = 180 }, Cmd.none )

                    else
                        update NoMoving game

                Right ->
                    if outOfBounds game then
                        ( { game | pPosition = changeXPosition 0 game, state = Running d, pRotation = 0 }, Cmd.none )

                    else if checkDir game.pPosition d Pacman || checkDir game.pPosition game.nextDir Pacman then
                        if checkDir game.pPosition game.nextDir Pacman && game.nextDir /= d then
                            ( { game | state = Running game.nextDir, nextDir = None }, Cmd.none )

                        else
                            ( checkEatable { game | pPosition = changeXPosition (game.pPosition.x + movement) game, state = Running d, pRotation = 0 }, Cmd.none )

                    else
                        update NoMoving game

                Up ->
                    if outOfBounds game then
                        ( { game | pPosition = changeYPosition fieldSettings.height game, state = Running d, pRotation = -90 }, Cmd.none )

                    else if checkDir game.pPosition d Pacman || checkDir game.pPosition game.nextDir Pacman then
                        if checkDir game.pPosition game.nextDir Pacman && game.nextDir /= d then
                            ( { game | state = Running game.nextDir, nextDir = None }, Cmd.none )

                        else
                            ( checkEatable { game | pPosition = changeYPosition (game.pPosition.y - movement) game, state = Running d, pRotation = -90 }, Cmd.none )

                    else
                        update NoMoving game

                Down ->
                    if outOfBounds game then
                        ( { game | pPosition = changeYPosition 0 game, state = Running d, pRotation = 90 }, Cmd.none )

                    else if checkDir game.pPosition d Pacman || checkDir game.pPosition game.nextDir Pacman then
                        if checkDir game.pPosition game.nextDir Pacman && game.nextDir /= d then
                            ( { game | state = Running game.nextDir, nextDir = None }, Cmd.none )

                        else
                            ( checkEatable { game | pPosition = changeYPosition (game.pPosition.y + movement) game, state = Running d, pRotation = 90 }, Cmd.none )

                    else
                        update NoMoving game

                _ ->
                    update NoMoving game

        Types.GameModels.Nothing ->
            case game.state of
                Running d ->
                    ( { game | state = Stopped d }, Cmd.none )

                Stopped d ->
                    ( { game | state = Running d }, Cmd.none )

        NoMoving ->
            ( game, Cmd.none )

        ChangeDirection d ->
            ( { game | nextDir = d }, Cmd.none )

        Fruit ->
            if game.secondCounter == 10 then
                ( { game | fruitAvailable = False }, Cmd.none )

            else
                ( { game | secondCounter = game.secondCounter + 1 }, Cmd.none )

        GhostMove ->
            ( { game | redGhost = moveGhost game.redGhost (getGhostNextDir game.pPosition game.redGhost 0 game.nextDir) }, Cmd.none )



----------
-- VIEW --
----------


view : Game -> Html Msg
view game =
    node "main"
        []
        [ node "style" [] [ text styleContents ]
        , div (class "wrapper" :: wrapperCss)
            [ div (class "headline" :: headlineCss)
                [ div (textCss ++ [ Html.Attributes.style "text-transform" "uppercase" ]) [ Html.text "High score" ]
                , div textCss [ Html.text (String.fromInt game.score) ]
                , div textCss [ Html.text "500x500" ]
                ]
            , div
                gameCss
                [ svg
                    (gameChildCss
                        ++ [ id "gameField" ]
                    )
                    (pointsToSvg game.items 1
                        ++ pointsToSvg game.pills 2
                        ++ createFruit game.fruitAvailable
                        ++ [ path [ fill fieldSettings.borderColor, d "M94,70.7H43.7c-2.8,0-5-2.3-5-5V42.3c0-2.8,2.3-5,5-5H94c2.8,0,5,2.3,5,5v23.3C99,68.4,96.8,70.7,94,70.7z" ] []
                           , path [ fill fieldSettings.borderColor, d "M200.3,70.7h-66c-2.8,0-5-2.3-5-5V42.3c0-2.8,2.2-5,5-5h66c2.8,0,5,2.3,5,5v23.3 C205.3,68.4,203.1,70.7,200.3,70.7z" ] []
                           , path [ fill fieldSettings.borderColor, d "M366.1,71.6h-67.5c-3,0-5.3-2.3-5.3-5V43.2c0-2.8,2.5-5,5.3-5h67.5c3,0,5.3,2.3,5.3,5v23.3 C371.5,69.3,369.1,71.6,366.1,71.6z" ] []
                           , path [ fill fieldSettings.borderColor, d "M94.6,131.6H42.4c-2.8,0-5-2.6-5-5.6v-21.3c0-3.2,2.3-5.6,5-5.6h52.3c2.8,0,5,2.6,5,5.6v21.3 C99.6,129.1,97.5,131.6,94.6,131.6z" ] []
                           , path [ fill fieldSettings.borderColor, d "M457,70.1h-52.5c-3,0-5.3-2.3-5.3-5V42.7c0-2.8,2.5-5,5.3-5H457c3,0,5.3,2.3,5.3,5V65 C462.3,67.8,460,70.1,457,70.1z" ] []
                           , path [ fill fieldSettings.borderColor, d "M199.7,158.3h-35.4c-2.4,0-4.3-2.2-4.3-5v-49.8c0-2.8-2-5-4.3-5h-22.1c-2.4,0-4.3,2.3-4.3,5v112.8 c0,2.8,2,5,4.3,5h22.1c2.4,0,4.3-2.3,4.3-5v-34.7c0-2.8,1.9-5,4.3-5h35.4c2.4,0,4.3-2.3,4.3-5v-8.3 C204,160.6,202.1,158.3,199.7,158.3z" ] []
                           , path [ fill fieldSettings.borderColor, d "M305.8,98.6H193.2c-2.8,0-5,2.3-5,5v22.7c0,2.8,2.3,5,5,5h36.3c2.8,0,5,2.2,5,5v35.3c0,2.8,2.3,5,5,5h18.3 c2.8,0,5-2.3,5-5v-35.3c0-2.8,2.2-5,5-5h38c2.8,0,5-2.3,5-5v-22.7C310.8,100.8,308.6,98.6,305.8,98.6z" ] []
                           , path [ fill fieldSettings.borderColor, d "M298.2,175.7h36.4c2.4,0,4.2,2.2,4.2,5v33.7c0,2.8,1.9,5,4.2,5h24.8c2.4,0,4.2-2.3,4.2-5l-0.9-111.9 c0-2.8-1.9-5-4.2-5h-23.8c-2.4,0-4.2,2.3-4.2,5l-0.1,49.9c0,2.8-1.8,5-4.2,5h-36.4c-2.4,0-4.2,2.3-4.2,5v8.3 C294,173.4,295.9,175.7,298.2,175.7z" ] []
                           , path [ fill fieldSettings.borderColor, d "M457,131.7h-50.5c-3,0-5.3-2.3-5.3-5v-22.4c0-2.8,2.5-5,5.3-5H457c3,0,5.3,2.3,5.3,5v22.3 C462.3,129.4,460,131.7,457,131.7z" ] []
                           , path [ fill fieldSettings.borderColor, d "M155,311.7h-21c-2.8,0-5-2.3-5-5V255c0-2.8,2.3-5,5-5h21c2.8,0,5,2.3,5,5v51.7 C160,309.4,157.8,311.7,155,311.7z" ] []
                           , path [ fill fieldSettings.borderColor, d "M307.3,294H193.7c-2.8,0-5,2.3-5,5v7.7c0,2.8,2.3,5,5,5H228c2.8,0,5,2.2,5,5V351c0,2.8,2.3,5,5,5h22.3 c2.8,0,5-2.3,5-5v-34.3c0-2.8,2.2-5,5-5h37c2.8,0,5-2.3,5-5V299C312.3,296.2,310.1,294,307.3,294z" ] []
                           , path [ fill fieldSettings.borderColor, d "M366,312.7h-22c-2.8,0-5-2.3-5-5V255c0-2.8,2.3-5,5-5h22c2.8,0,5,2.3,6,5v52.7   C371,310.4,368.8,312.7,366,312.7z" ] []
                           , path [ fill fieldSettings.borderColor, d "M94,340.3H81.3h-1H43c-2.8,0-5,2.2-5,5v7.3c0,2.8,2.3,5,5,5h21.3c2.8,0,5,2.2,5,5V411c0,2.8,2.3,5,5,5H94 c2.8,0,5-2.3,5-5v-65.7C99,342.6,96.8,340.3,94,340.3z" ] []
                           , path [ fill fieldSettings.borderColor, d "M202.3,356.7h-69c-2.8,0-5-2.3-5-5v-8.3c0-2.8,2.2-5,5-5h69c2.8,0,5,2.3,5,5v8.3   C207.3,354.4,205.1,356.7,202.3,356.7z" ] []
                           , path [ fill fieldSettings.borderColor, d "M368,355.7h-69c-2.8,0-5-2.3-5-5v-7.3c0-2.8,2.2-5,5-5h69c2.8,0,5,2.3,5,5v7.3   C373,353.4,370.8,355.7,368,355.7z" ] []
                           , path [ fill fieldSettings.borderColor, d "M404.3,338.3h4.7h5h42.3c2.8,0,5,2.2,5,5v7.3c0,2.8-2.3,5-5,5H435c-2.8,0-5,2.2-5,5V412c0,2.8-2.3,5-5,5h-20.7 c-2.8,0-5-2.3-5-5v-68.7C399.3,340.6,401.6,338.3,404.3,338.3z" ] []
                           , path [ fill fieldSettings.borderColor, d "M202,442.7h-37c-2.8,0-5-2.2-5-5v-50.3c0-2.8-2.3-5-5-5h-20.9c-2.8,0-5,2.3-5,5v50.3c0,2.8-2.2,5-5,5H44 c-2.8,0-5,2.2-5,5v9.3c0,2.8,2.3,5,5,5H202c2.8,0,5-2.3,5-5v-9.3C207,444.9,204.8,442.7,202,442.7z" ] []
                           , path [ fill fieldSettings.borderColor, d "M298.9,443.7h35.5c2.7,0,4.9-2.2,4.9-5v-52.3c0-2.8,2.3-5,4.9-5H366c2.7,0,4.9,2.3,4.9,5v52.3 c0,2.8,2.2,5,4.9,5h81.5c2.7,0,4.9,2.2,4.9,5v8.3c0,2.8-2.3,5-4.9,5H298.9c-2.7,0-4.9-2.3-4.9-5v-8.3 C294,445.9,296.3,443.7,298.9,443.7z" ] []
                           , path [ fill fieldSettings.borderColor, d "M304.3,383.3H195.7c-2.8,0-5,2.3-5,5V411c0,2.8,2.3,5,5,5H229c2.8,0,5,3.2,5,6v35.3c0,2.8,2.3,5,5,5h21.3 c2.8,0,5-2.3,5-5V422c0-2.8,2.2-6,5-6h34c2.8,0,5-2.3,5-5v-22.7C309.3,385.6,307.1,383.3,304.3,383.3z" ] []

                           -- Prison barrier
                           , line [ x1 "234.5", y1 "205.5", x2 "263.5", y2 "205.5", stroke "#FFBA00", strokeWidth "2px" ] []

                           -- Outer lines and inner cave
                           , path [ fill fieldSettings.borderColor, d "M403.7,211.2v-43.8c0-2.9,2.2-5.1,5-5.1h84.9c1.5,0,2.7-1.2,2.7-2.7V7.9c0-1.5-1.2-2.7-2.7-2.7H6.8 c-1.5,0-2.7,1.2-2.7,2.7v152.2c0,1.5,1.2,2.7,2.7,2.7h82.8c2.8,0,5,2.2,5,5.1v41.8c0,2.9-2.2,6.1-5,6.1H0v5.5h97.3 c1.5,0,2.7-1.2,2.7-2.7v-58.3c0-1.5-1.2-2.7-2.7-2.7H16.5c-2.8,0-5-2.2-5-5.1V15.9c0-2.9,2.2-5.1,5-5.1h211c2.8,0,5,2.2,5,5.1v51 c0,2.9,2.2,5.1,5,5.1H261c2.8,0,5-2.2,5-5.1v-51c0-2.9,2.2-5.1,5-5.1h212.9c2.8,0,5,2.2,5,5.1v136c0,2.9-2.2,5.1-5,5.1H401 c-1.5,0-2.7,1.2-2.7,2.7v59.1c0,0.1,0,0.1,0,0.2c0,0.1,0,0.1,0,0.2c0,1.5,1.2,2.7,2.7,2.7h99v-5.5h-91.3 C405.9,216.3,403.7,214.1,403.7,211.2z" ] []
                           , polygon [ fill fieldSettings.borderColor, points "306.9,203.5 263.5,203.5 263.5,208.3 306.9,208.3 306.9,261.9 192.4,261.9 192.4,208.3 234.5,208.3 234.5,203.5 192.4,203.5 188.4,203.5 188.4,208.3 188.4,261.9 188.4,266.7 192.4,266.7 306.9,266.7 311,266.7 311,261.9 311,208.3 311,203.5" ] []
                           , path [ fill fieldSettings.borderColor, d "M406.4,254.4H500V249h-99.5c-1.2,0-2.2,1.2-2.2,2.7l0,0l0,0v56.3c0,1.5,1.2,2.7,2.7,2.7h85.4 c1.5,0,2.7,1.2,2.7,2.7v65.9c0,2.9-2.2,5.1-5,5.1h-19.9c-2.8,0-5,2.2-5,5.1v20.8c0,2.9,2.2,5.1,5,5.1h19.9c2.8,0,5,2.2,5,5.1v66.4 c0,1.5-1.2,2.7-2.7,2.7H14.1c-1.5,0-2.7-1.2-2.7-2.7v-66.5c0-2.9,2.2-4.1,5-4.1h20.1c2.8,0,5-2.2,5-5.1v-21.8c0-2.9-2.2-5.1-5-5.1 H16.4c-2.8,0-5-2.2-5-5.1V314c0-1.5,1.2-2.7,2.7-2.7h83.2c0.1,0,0.2,0,0.3,0c0.1,0,0.2,0,0.3,0c1.5,0,2.7-1.2,2.7-2.7v-57 c0-1.5-1.2-2.7-2.7-2.7H0v5.5h92.6c1.5,0,2.7,1.2,2.7,2.7v46c0,1.5-1.2,2.7-2.7,2.7H6.7c-1.5,0-2.7,1.2-2.7,2.7v183.9 c0,1.5,1.2,2.7,2.7,2.7h487.1c1.5,0,2.7-1.2,2.7-2.7V308c0-1.5-1.2-2.7-2.7-2.7h-87.4c-1.5,0-2.7-1.2-2.7-2.7V257 C403.7,255.6,404.9,254.4,406.4,254.4z" ] []
                           ]
                    )
                , div
                    (gameChildCss
                        ++ [ id "pacmanArea" ]
                    )
                    [ img (pacmanSvgCss ++ [ src "https://upload.wikimedia.org/wikipedia/commons/thumb/4/49/Pacman.svg/972px-Pacman.svg.png", Html.Attributes.style "top" (String.fromInt (game.pPosition.y - round (toFloat pacSettings.ratio / 2)) ++ "px"), Html.Attributes.style "left" (String.fromInt (game.pPosition.x - round (toFloat pacSettings.ratio / 2)) ++ "px"), Html.Attributes.style "transform" ("rotate(" ++ String.fromInt game.pRotation ++ "deg)") ])
                        []
                    ]
                , div
                    (gameChildCss
                        ++ [ id "ghostArea" ]
                    )
                    [ img (ghostSvgCss ++ [ src "Assets/img/ghosts/blinky.svg", Html.Attributes.style "top" (String.fromInt (game.redGhost.position.y - round (toFloat ghostSettings.ratio / 2)) ++ "px"), Html.Attributes.style "left" (String.fromInt (game.redGhost.position.x - round (toFloat ghostSettings.ratio / 2)) ++ "px") ])
                        []
                    , img (ghostSvgCss ++ [ src "Assets/img/ghosts/pinky.svg", Html.Attributes.style "top" (String.fromInt (game.pinkGhost.position.y - round (toFloat ghostSettings.ratio / 2)) ++ "px"), Html.Attributes.style "left" (String.fromInt (game.pinkGhost.position.x - round (toFloat ghostSettings.ratio / 2)) ++ "px") ])
                        []
                    , img (ghostSvgCss ++ [ src "Assets/img/ghosts/inky.svg", Html.Attributes.style "top" (String.fromInt (game.blueGhost.position.y - round (toFloat ghostSettings.ratio / 2)) ++ "px"), Html.Attributes.style "left" (String.fromInt (game.blueGhost.position.x - round (toFloat ghostSettings.ratio / 2)) ++ "px") ])
                        []
                    , img (ghostSvgCss ++ [ src "Assets/img/ghosts/clyde.svg", Html.Attributes.style "top" (String.fromInt (game.yellowGhost.position.y - round (toFloat ghostSettings.ratio / 2)) ++ "px"), Html.Attributes.style "left" (String.fromInt (game.yellowGhost.position.x - round (toFloat ghostSettings.ratio / 2)) ++ "px") ])
                        []
                    ]
                ]
            , div (class "headline" :: headlineCss)
                [ div (textCss ++ [ Html.Attributes.style "text-transform" "uppercase" ]) [ Html.text "Leben:" ]
                , div (textCss ++ [ Html.Attributes.style "text-align" "left" ]) [ Html.text "3" ]
                , div (textCss ++ [ Html.Attributes.style "text-transform" "uppercase" ]) [ Html.text "Früchte:" ]
                , div textCss
                    [ img [ src "Assets/img/fruits/apple.svg", width fruitSettings.ratio, height fruitSettings.ratio ]
                        []
                    , img [ src "Assets/img/fruits/orange.svg", width fruitSettings.ratio, height fruitSettings.ratio ]
                        []
                    , img [ src "Assets/img/fruits/strawberry.svg", width fruitSettings.ratio, height fruitSettings.ratio ]
                        []
                    , img [ src "Assets/img/fruits/cherry.svg", width fruitSettings.ratio, height fruitSettings.ratio ]
                        []
                    ]
                ]
            ]
        ]



-------------------
-- Subscriptions --
-------------------


subscriptions : Game -> Sub Msg
subscriptions game =
    Sub.batch
        [ Browser.Events.onKeyDown keyDecoder
        , case game.state of
            Running d ->
                Time.every 20 (\_ -> MoveDirection d)

            _ ->
                Sub.none
        , if game.fruitAvailable then
            Time.every 1000 (\_ -> Fruit)

          else
            Sub.none
        , Time.every 20 (\_ -> GhostMove)
        ]



-------------------
-- MAIN PROGRAMM --
-------------------


main : Program () Game Msg
main =
    Browser.element
        { init = \_ -> ( initialModel, Cmd.none )
        , subscriptions = subscriptions
        , view = view
        , update = update
        }



---------------
-- FUNCTIONS --
---------------
-- key functions


keyDecoder : Json.Decode.Decoder Msg
keyDecoder =
    Json.Decode.map toKey (Json.Decode.field "key" Json.Decode.string)


toKey : String -> Msg
toKey string =
    case string of
        "ArrowUp" ->
            ChangeDirection Up

        "ArrowDown" ->
            ChangeDirection Down

        "ArrowLeft" ->
            ChangeDirection Left

        "ArrowRight" ->
            ChangeDirection Right

        _ ->
            Types.GameModels.Nothing


changeXPosition : Int -> Game -> Point
changeXPosition value game =
    let
        oldPosition =
            game.pPosition
    in
    { oldPosition | x = value }


changeYPosition : Int -> Game -> Point
changeYPosition value game =
    let
        oldPosition =
            game.pPosition
    in
    { oldPosition | y = value }


substractList : List Point -> List Point -> List Point
substractList a b =
    List.filter (\x -> not (List.member x a)) b
