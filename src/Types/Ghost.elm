module Types.Ghost exposing (..)

import Movement exposing (checkDir)
import Settings exposing (ghostSettings, movement)
import Types.GameModels exposing (Direction(..), Ghost)
import Types.Line exposing (LineType(..))
import Types.Point exposing (Point)


moveGhost : Ghost -> Direction -> Ghost
moveGhost ghost dir =
    case dir of
        Left ->
            { position = { x = ghost.position.x - movement, y = ghost.position.y }, dir = Left, lastDir = ghost.dir }

        Right ->
            { position = { x = ghost.position.x + movement, y = ghost.position.y }, dir = Right, lastDir = ghost.dir }

        Up ->
            { position = { x = ghost.position.x, y = ghost.position.y - movement }, dir = Up, lastDir = ghost.dir }

        Down ->
            { position = { x = ghost.position.x, y = ghost.position.y + movement }, dir = Down, lastDir = ghost.dir }

        _ ->
            ghost


getGhostNextDir : Point -> Ghost -> Int -> Direction -> Direction
getGhostNextDir pacMan ghost multiplicator pacDir =
    let
        xDif =
            max pacMan.x ghost.position.x - min pacMan.x ghost.position.x

        yDif =
            max pacMan.y ghost.position.y - min pacMan.y ghost.position.y

        moveOptions : { vertical : Direction, horizontal : Direction, cVertical : Direction, cHorizontal : Direction }
        moveOptions =
            -- pacMan right down
            if pacMan.x > ghost.position.x && pacMan.y > ghost.position.y then
                { vertical = Down
                , horizontal = Right
                , cVertical = Up
                , cHorizontal = Left
                }
                -- pacMan right up

            else if pacMan.x > ghost.position.x && pacMan.y < ghost.position.y then
                { vertical = Up
                , horizontal = Right
                , cVertical = Down
                , cHorizontal = Left
                }
                -- pacMan left up

            else if pacMan.x < ghost.position.x && pacMan.y < ghost.position.y then
                { vertical = Up
                , horizontal = Left
                , cVertical = Down
                , cHorizontal = Right
                }
                -- pacMan left down

            else if pacMan.x < ghost.position.x && pacMan.y > ghost.position.y then
                { vertical = Down
                , horizontal = Left
                , cVertical = Up
                , cHorizontal = Right
                }

            else if pacMan.x == ghost.position.x then
                if pacMan.y > ghost.position.y then
                    { vertical = Down
                    , horizontal = None
                    , cVertical = Up
                    , cHorizontal = None
                    }

                else
                    { vertical = Up
                    , horizontal = None
                    , cVertical = Down
                    , cHorizontal = None
                    }

            else if pacMan.y == ghost.position.y then
                if pacMan.x > ghost.position.x then
                    { vertical = None
                    , horizontal = Right
                    , cVertical = None
                    , cHorizontal = Left
                    }

                else
                    { vertical = None
                    , horizontal = Left
                    , cVertical = None
                    , cHorizontal = Right
                    }

            else
                { vertical = None
                , horizontal = None
                , cVertical = None
                , cHorizontal = None
                }
    in
    if ghost.dir == None || ((ghost.dir == Left || ghost.dir == Right) && (checkDir ghost.position Up Types.Line.Ghost || checkDir ghost.position Down Types.Line.Ghost)) || ((ghost.dir == Up || ghost.dir == Down) && (checkDir ghost.position Left Types.Line.Ghost || checkDir ghost.position Right Types.Line.Ghost)) then
        if (checkDir ghost.position moveOptions.horizontal Types.Line.Ghost && ghost.dir /= moveOptions.cHorizontal) && (checkDir ghost.position moveOptions.vertical Types.Line.Ghost && ghost.dir /= moveOptions.cVertical) then
            if xDif > yDif then
                moveOptions.horizontal

            else
                moveOptions.vertical

        else if checkDir ghost.position moveOptions.horizontal Types.Line.Ghost && ghost.dir /= moveOptions.cHorizontal then
            moveOptions.horizontal

        else if checkDir ghost.position moveOptions.vertical Types.Line.Ghost && ghost.dir /= moveOptions.cVertical then
            moveOptions.vertical

        else if moveOptions.horizontal /= Left && ghost.dir /= Right && checkDir ghost.position Left Types.Line.Ghost then
            Left

        else if moveOptions.horizontal /= Right && ghost.dir /= Left && checkDir ghost.position Right Types.Line.Ghost then
            Right

        else if moveOptions.vertical /= Up && ghost.dir /= Down && checkDir ghost.position Up Types.Line.Ghost then
            Up

        else if moveOptions.vertical /= Down && ghost.dir /= Up && checkDir ghost.position Down Types.Line.Ghost then
            Down

        else
            None

    else
        ghost.dir
