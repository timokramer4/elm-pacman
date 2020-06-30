module Eatable exposing (..)

import Settings exposing (itemSettings, pillSettings, scoreSettings)
import Svg exposing (Svg, circle, rect)
import Svg.Attributes exposing (cx, cy, fill, r, x, y)
import Types.GameModels exposing (Game, Msg)
import Types.Line exposing (Line)
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
    if startPoint /= endPoint then
        startPoint :: createPoints (Line currentPoint endPoint) pointList

    else
        currentPoint :: pointList


moveToWards : Point -> Point -> Float -> Point
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
            checkCurrentPoint game.eatablePoints

        localListPills : List Point
        localListPills =
            checkCurrentPoint game.pills
    in
    if List.length game.pills == List.length localListPills && List.length game.eatablePoints == List.length localListItems then
        game

    else if List.length game.pills /= List.length localListPills then
        { game | pills = localListPills, score = game.score + scoreSettings.pill }

    else
        { game | eatablePoints = localListItems, score = game.score + scoreSettings.item }


pointsToSvg : List Point -> Int -> List (Svg Msg)
pointsToSvg points mode =
    -- mode : 1 -> eatable, 2-> pills
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
        [ x (String.fromFloat (point.x - itemSettings.size / 2))
        , y (String.fromFloat (point.y - itemSettings.size / 2))
        , Svg.Attributes.width (String.fromFloat itemSettings.size)
        , Svg.Attributes.height (String.fromFloat itemSettings.size)
        , fill itemSettings.fill
        ]
        []


createPillSvg : Int -> Point -> Svg Msg
createPillSvg _ point =
    circle
        [ cx (String.fromFloat point.x)
        , cy (String.fromFloat point.y)
        , r (String.fromFloat pillSettings.radius)
        , fill pillSettings.fill
        ]
        []


indexedMap : (Int -> a -> b) -> List a -> List b
indexedMap func list =
    (\( v, _ ) -> v) (List.foldl (\x ( ys, l ) -> ( func l x :: ys, l + 1 )) ( [], 0 ) list)
