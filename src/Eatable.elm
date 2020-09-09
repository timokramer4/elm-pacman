module Eatable exposing (checkEatable, createFruit, createPoints, pointsToSvg, setScoreMsg)

import Html exposing (Html)
import Settings exposing (fruitSettings, itemSettings, pillSettings)
import Svg exposing (Svg, circle, image, rect)
import Svg.Attributes exposing (cx, cy, fill, height, r, width, x, xlinkHref, y)
import Types.GameModels exposing (Game, GhostColors(..), Msg, ScoreMessage)
import Types.Ghost exposing (changeGhostSrc)
import Types.Line exposing (Line, LineType(..))
import Types.Point exposing (Point)



-- Creates a list of points from a line


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



-- Returns the next eatable point


moveToWards : Point -> Point -> Int -> Point
moveToWards from to lenght =
    { x = from.x + min lenght (to.x - from.x), y = from.y + min lenght (to.y - from.y) }



-- Checks if there is an eatable item at the current position


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

        -- Remove current point from item list
        localListItems : List Point
        localListItems =
            checkCurrentPoint game.items

        -- Remove current point from pill list
        localListPills : List Point
        localListPills =
            checkCurrentPoint game.pills

        -- Check current level for fruit xp
        fruitXp =
            if game.level == 1 then
                fruitSettings.xpCherry

            else if game.level == 2 then
                fruitSettings.xpStrawberry

            else if game.level == 3 || game.level == 4 then
                fruitSettings.xpOrange

            else if game.level == 5 || game.level == 6 then
                fruitSettings.xpApple

            else if game.level == 7 || game.level == 8 then
                fruitSettings.xpGrape

            else if game.level == 9 || game.level == 10 then
                fruitSettings.xpSpacechip

            else if game.level == 11 || game.level == 1 then
                fruitSettings.xpBell

            else
                fruitSettings.xpKey
    in
    -- When pacman eat no pill and no item
    if
        List.length game.pills
            == List.length localListPills
            && List.length game.items
            == List.length localListItems
    then
        -- When pacMan eat fruit
        if
            game.pPosition
                == fruitSettings.position
                && game.fruitAvailable
        then
            { game
                | mouthMovement = True
                , score = game.score + fruitXp
                , fruitAvailable = False
                , scoreMessage = setScoreMsg fruitSettings.position (String.fromInt fruitXp)
                , showScoreMessage = True
            }
            -- When pacMan eat nothing

        else
            game
        -- When pacMan eat pill

    else if List.length game.pills /= List.length localListPills then
        { game
            | mouthMovement = True
            , pills = localListPills
            , score = game.score + pillSettings.xp
            , pillSecondCounter = 0
            , pillActive = True
            , scoreMessage = setScoreMsg game.pPosition (String.fromInt pillSettings.xp)
            , showScoreMessage = True
            , redGhost = changeGhostSrc game.redGhost Hunted
            , yellowGhost = changeGhostSrc game.yellowGhost Hunted
            , blueGhost = changeGhostSrc game.blueGhost Hunted
            , pinkGhost = changeGhostSrc game.pinkGhost Hunted
        }
        -- When pacMan eat fruit and item, if fruit availeble

    else if
        game.pPosition
            == fruitSettings.position
            && game.fruitAvailable
    then
        { game
            | mouthMovement = True
            , items = localListItems
            , score = game.score + fruitXp + itemSettings.xp
            , fruitAvailable = False
            , scoreMessage = setScoreMsg fruitSettings.position (String.fromInt fruitXp)
            , showScoreMessage = True
            , eatItem = True
            , eatItemSecondCounter = itemSettings.noEatingCooldownMs
        }
        -- When pacMan eat item and unlock fruit

    else if
        game.itemCounter
            == fruitSettings.itemNumber1
            || game.itemCounter
            == fruitSettings.itemNumber2
    then
        { game
            | mouthMovement = True
            , items = localListItems
            , score = game.score + itemSettings.xp
            , itemCounter = game.itemCounter + 1
            , fruitAvailable = True
            , eatItem = True
            , eatItemSecondCounter = itemSettings.noEatingCooldownMs
        }
        -- When pacMan eat only item

    else
        { game
            | mouthMovement = True
            , items = localListItems
            , score = game.score + itemSettings.xp
            , itemCounter = game.itemCounter + 1
            , eatItem = True
            , eatItemSecondCounter = itemSettings.noEatingCooldownMs
        }



-- Converts a list of points to a list of SVGs


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



-- Creates a normal, eatable item as SVG


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



-- Creates a pill as SVG


createPillSvg : Int -> Point -> Svg Msg
createPillSvg _ point =
    circle
        [ cx (String.fromInt point.x)
        , cy (String.fromInt point.y)
        , r (String.fromInt pillSettings.radius)
        , fill pillSettings.fill
        ]
        []



-- Creates a fruit as SVG using the current level


createFruit : Bool -> Int -> List (Html Msg)
createFruit available level =
    let
        fruit =
            if level == 1 then
                "cherry"

            else if level == 2 then
                "strawberry"

            else if level == 3 || level == 4 then
                "orange"

            else if level == 5 || level == 6 then
                "apple"

            else if level == 7 || level == 8 then
                "grape"

            else if level == 9 || level == 10 then
                "spaceship"

            else if level == 11 || level == 12 then
                "bell"

            else
                "key"
    in
    if available then
        [ image
            [ xlinkHref ("Assets/img/fruits/" ++ fruit ++ ".svg")
            , width (String.fromInt fruitSettings.ratio)
            , height (String.fromInt fruitSettings.ratio)
            , x (String.fromInt (fruitSettings.position.x - round (toFloat fruitSettings.ratio / 2)))
            , y (String.fromInt (fruitSettings.position.y - round (toFloat fruitSettings.ratio / 2)))
            ]
            []
        ]

    else
        []



-- Sets the score message of the previously collected item


setScoreMsg : Point -> String -> ScoreMessage
setScoreMsg msgPoint msgText =
    { point = msgPoint
    , msg = msgText
    }



-- Indexed map


indexedMap : (Int -> a -> b) -> List a -> List b
indexedMap func list =
    (\( v, _ ) -> v) (List.foldl (\x ( ys, l ) -> ( func l x :: ys, l + 1 )) ( [], 0 ) list)
