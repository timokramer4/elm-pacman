module Eatable exposing (..)

import Html exposing (..)
import Html.Attributes exposing (src)
import Settings exposing (fruitSettings, itemSettings, pillSettings)
import Svg exposing (Svg, circle, image, rect)
import Svg.Attributes exposing (cx, cy, fill, height, r, width, x, xlinkHref, y)
import Types.GameModels exposing (Game, Msg)
import Types.Line exposing (Line, LineType(..))
import Types.Point exposing (Point)


createPoints : Line -> List Point -> List Point
createPoints line pointList =
    let
        startPoint : Point
        startPoint =
            { x = min line.start.x line.end.x, y = min line.start.y line.end.y }

        endPoint : Point
        endPoint =
            { x = max line.start.x line.end.x, y = max line.start.y line.end.y }

        currentPoint : Point
        currentPoint =
            moveToWards startPoint endPoint itemSettings.step
    in
    if line.linetype == Both then
        if startPoint /= endPoint then
            startPoint :: createPoints (Line currentPoint endPoint line.linetype) pointList

        else
            currentPoint :: pointList

    else
        pointList


moveToWards : Point -> Point -> Int -> Point
moveToWards from to lenght =
    { x = from.x + min lenght (to.x - from.x), y = from.y + min lenght (to.y - from.y) }


checkEatable : Game -> Game
checkEatable game =
    let
        checkCurrentPoint : List Point -> List Point
        checkCurrentPoint lp =
            case lp of
                [] ->
                    []

                x :: xs ->
                    if x == game.pPosition then
                        checkCurrentPoint xs

                    else
                        x :: checkCurrentPoint xs

        localListItems : List Point
        localListItems =
            checkCurrentPoint game.items

        localListPills : List Point
        localListPills =
            checkCurrentPoint game.pills
    in
    if List.length game.pills == List.length localListPills && List.length game.items == List.length localListItems then
        if game.pPosition == fruitSettings.position && game.fruitAvailable then
            { game | score = game.score + fruitSettings.xp, fruitAvailable = False }

        else
            game

    else if List.length game.pills /= List.length localListPills then
        if game.pPosition == fruitSettings.position && game.fruitAvailable then
            { game | pills = localListPills, score = game.score + fruitSettings.xp + pillSettings.xp, fruitAvailable = False }

        else
            { game | pills = localListPills, score = game.score + pillSettings.xp }

    else if game.pPosition == fruitSettings.position && game.fruitAvailable then
        { game | items = localListItems, score = game.score + fruitSettings.xp + itemSettings.xp, fruitAvailable = False }

    else if game.itemCounter == fruitSettings.itemNumber1 || game.itemCounter == fruitSettings.itemNumber2 then
        { game | items = localListItems, score = game.score + itemSettings.xp, itemCounter = game.itemCounter + 1, fruitAvailable = True }

    else
        { game | items = localListItems, score = game.score + itemSettings.xp, itemCounter = game.itemCounter + 1 }


pointsToSvg : List Point -> Int -> List (Svg Msg)
pointsToSvg points mode =
    -- Modes
    -- 1 -> items
    -- 2 -> pills
    case mode of
        1 ->
            indexedMap createItemSvg points

        2 ->
            indexedMap createPillSvg points

        _ ->
            []


createItemSvg : Int -> Point -> Svg Msg
createItemSvg _ point =
    rect
        [ x (String.fromInt (point.x - round (toFloat itemSettings.size / 2)))
        , y (String.fromInt (point.y - round (toFloat itemSettings.size / 2)))
        , Svg.Attributes.width (String.fromInt itemSettings.size)
        , Svg.Attributes.height (String.fromInt itemSettings.size)
        , fill itemSettings.fill
        ]
        []


createPillSvg : Int -> Point -> Svg Msg
createPillSvg _ point =
    circle
        [ cx (String.fromInt point.x)
        , cy (String.fromInt point.y)
        , r (String.fromInt pillSettings.radius)
        , fill pillSettings.fill
        ]
        []


createFruit : Bool -> List (Html Msg)
createFruit available =
    if available then
        [ image [ xlinkHref "Assets/img/fruits/cherry.svg", width (String.fromInt fruitSettings.ratio), height (String.fromInt fruitSettings.ratio), x (String.fromInt (fruitSettings.position.x - round (toFloat fruitSettings.ratio / 2))), y (String.fromInt (fruitSettings.position.y - round (toFloat fruitSettings.ratio / 2))) ] [] ]

    else
        []


indexedMap : (Int -> a -> b) -> List a -> List b
indexedMap func list =
    (\( v, _ ) -> v) (List.foldl (\x ( ys, l ) -> ( func l x :: ys, l + 1 )) ( [], 0 ) list)
