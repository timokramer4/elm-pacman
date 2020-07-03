module Types.Ghoast exposing (..)

import Movement exposing (checkDir)
import Settings exposing (movement)
import Types.GameModels exposing (Direction(..), Game)
import Types.Point exposing (Point)


moveGhoast : Point -> Direction -> Point
moveGhoast point dir =
    case dir of
        Left ->
            { x = point.x - movement, y = point.y }

        Right ->
            { x = point.x + movement, y = point.y }

        Up ->
            { x = point.x, y = point.y - movement }

        Down ->
            { x = point.x, y = point.y + movement }

        _ ->
            point


getGhoastNextDir : Point -> Point -> Int -> Direction -> Direction
getGhoastNextDir pacMan ghoast multiplicator pacDir =
    let
        xDif =
            max pacMan.x ghoast.x - min pacMan.x ghoast.x

        yDif =
            max pacMan.y ghoast.y - min pacMan.y ghoast.y

        moveOptions : { vertical : Direction, horizontal : Direction }
        moveOptions =
            -- pacMan right down
            if pacMan.x > ghoast.x && pacMan.y > ghoast.y then
                { vertical = Down
                , horizontal = Right
                }
                -- pacMan right up

            else if pacMan.x > ghoast.x && pacMan.y < ghoast.y then
                { vertical = Up
                , horizontal = Right
                }
                -- pacMan left up

            else if pacMan.x < ghoast.x && pacMan.y < ghoast.y then
                { vertical = Left
                , horizontal = Up
                }
                -- pacMan left down

            else if pacMan.x < ghoast.x && pacMan.y > ghoast.y then
                { vertical = Down
                , horizontal = Left
                }

            else if pacMan.x == ghoast.x then
                if pacMan.y > ghoast.y then
                    { vertical = Down
                    , horizontal = None
                    }

                else
                    { vertical = Up
                    , horizontal = None
                    }

            else if pacMan.y == ghoast.y then
                if pacMan.x > ghoast.x then
                    { vertical = None
                    , horizontal = Right
                    }

                else
                    { vertical = None
                    , horizontal = Left
                    }

            else
                { vertical = None
                , horizontal = None
                }
    in
    if checkDir ghoast moveOptions.horizontal && checkDir ghoast moveOptions.vertical then
        if xDif > yDif then
            moveOptions.horizontal

        else
            moveOptions.vertical

    else if checkDir ghoast moveOptions.horizontal then
        moveOptions.horizontal

    else
        moveOptions.vertical
