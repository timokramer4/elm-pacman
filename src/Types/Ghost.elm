module Types.Ghost exposing (..)

import Movement exposing (checkDir)
import Settings exposing (ghostSettings, movement)
import Types.GameModels exposing (Direction(..), Ghost)
import Types.Line exposing (LineType(..))
import Types.Point exposing (Point)


moveGhost : Ghost -> Direction -> Ghost
moveGhost ghost dir =
    let
        activeState =
            if not ghost.active then
                not ghost.active && ghost.position == ghostSettings.startPosition

            else
                ghost.active
    in
    case dir of
        Left ->
            { position = { x = ghost.position.x - movement, y = ghost.position.y }, dir = Left, active = activeState }

        Right ->
            { position = { x = ghost.position.x + movement, y = ghost.position.y }, dir = Right, active = activeState }

        Up ->
            { position = { x = ghost.position.x, y = ghost.position.y - movement }, dir = Up, active = activeState }

        Down ->
            { position = { x = ghost.position.x, y = ghost.position.y + movement }, dir = Down, active = activeState }

        _ ->
            ghost


getGhostNextDir : Point -> Ghost -> Int -> Direction -> Direction
getGhostNextDir pacMan ghost multiplicator pacDir =
    let
        xDif =
            max pacMan.x ghost.position.x - min pacMan.x ghost.position.x

        yDif =
            max pacMan.y ghost.position.y - min pacMan.y ghost.position.y

        currentType =
            if not ghost.active then
                GhostStartLine

            else
                Types.Line.Ghost

        targetPos =
            if not ghost.active then
                ghostSettings.startPosition

            else
                pacMan

        moveOptions : { vertical : Direction, horizontal : Direction, cVertical : Direction, cHorizontal : Direction }
        moveOptions =
            -- targetPos right down
            if targetPos.x > ghost.position.x && targetPos.y > ghost.position.y then
                { vertical = Down
                , horizontal = Right
                , cVertical = Up
                , cHorizontal = Left
                }
                -- targetPos right up

            else if targetPos.x > ghost.position.x && targetPos.y < ghost.position.y then
                { vertical = Up
                , horizontal = Right
                , cVertical = Down
                , cHorizontal = Left
                }
                -- targetPos left up

            else if targetPos.x < ghost.position.x && targetPos.y < ghost.position.y then
                { vertical = Up
                , horizontal = Left
                , cVertical = Down
                , cHorizontal = Right
                }
                -- targetPos left down

            else if targetPos.x < ghost.position.x && targetPos.y > ghost.position.y then
                { vertical = Down
                , horizontal = Left
                , cVertical = Up
                , cHorizontal = Right
                }

            else if targetPos.x == ghost.position.x then
                if targetPos.y > ghost.position.y then
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

            else if targetPos.y == ghost.position.y then
                if targetPos.x > ghost.position.x then
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
    if ghost.dir == None || ((ghost.dir == Left || ghost.dir == Right) && (checkDir ghost.position Up currentType || checkDir ghost.position Down currentType)) || ((ghost.dir == Up || ghost.dir == Down) && (checkDir ghost.position Left currentType || checkDir ghost.position Right currentType)) then
        if (checkDir ghost.position moveOptions.horizontal currentType && ghost.dir /= moveOptions.cHorizontal) && (checkDir ghost.position moveOptions.vertical currentType && ghost.dir /= moveOptions.cVertical) then
            if xDif > yDif then
                moveOptions.horizontal

            else
                moveOptions.vertical

        else if checkDir ghost.position moveOptions.horizontal currentType && ghost.dir /= moveOptions.cHorizontal then
            moveOptions.horizontal

        else if checkDir ghost.position moveOptions.vertical currentType && ghost.dir /= moveOptions.cVertical then
            moveOptions.vertical

        else if moveOptions.horizontal /= Left && ghost.dir /= Right && checkDir ghost.position Left currentType then
            Left

        else if moveOptions.horizontal /= Right && ghost.dir /= Left && checkDir ghost.position Right currentType then
            Right

        else if moveOptions.vertical /= Up && ghost.dir /= Down && checkDir ghost.position Up currentType then
            Up

        else if moveOptions.vertical /= Down && ghost.dir /= Up && checkDir ghost.position Down currentType then
            Down

        else
            None

    else
        ghost.dir
