module Types.Ghost exposing (..)

import Movement exposing (checkDir)
import Settings exposing (ghostSettings, movement)
import Types.GameModels exposing (Direction(..), Ghost)
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
    if ghost.dir == None || ((ghost.dir == Left || ghost.dir == Right) && (checkDir ghost.position Up True || checkDir ghost.position Down True)) || ((ghost.dir == Up || ghost.dir == Down) && (checkDir ghost.position Left True || checkDir ghost.position Right True)) then
        if (checkDir ghost.position moveOptions.horizontal True && ghost.dir /= moveOptions.cHorizontal) && (checkDir ghost.position moveOptions.vertical True && ghost.dir /= moveOptions.cVertical) then
            if xDif > yDif then
                moveOptions.horizontal

            else
                moveOptions.vertical

        else if checkDir ghost.position moveOptions.horizontal True && ghost.dir /= moveOptions.cHorizontal then
            moveOptions.horizontal

        else if checkDir ghost.position moveOptions.vertical True && ghost.dir /= moveOptions.cVertical then
            moveOptions.vertical

        else if moveOptions.horizontal /= Left && ghost.dir /= Right && checkDir ghost.position Left True then
            Left

        else if moveOptions.horizontal /= Right && ghost.dir /= Left && checkDir ghost.position Right True then
            Right

        else if moveOptions.vertical /= Up && ghost.dir /= Down && checkDir ghost.position Up True then
            Up

        else if moveOptions.vertical /= Down && ghost.dir /= Up && checkDir ghost.position Down True then
            Down

        else
            None
        -- else if xDif > yDif then
        --     if checkDir ghost.position Left True && ghost.dir /= Right then
        --         Left
        --     else if checkDir ghost.position Right True && ghost.dir /= Left then
        --         Right
        --     else
        --         None
        -- else if checkDir ghost.position Up True && ghost.dir /= Down then
        --     Up
        -- else if checkDir ghost.position Down True && ghost.dir /= Up then
        --     Down
        -- else
        --     None
        -- else if moveOptions.vertical /= None then
        --     if checkDir ghost.position Left True then
        --         Left
        --     else if checkDir ghost.position Right True then
        --         Right
        --     else
        --         None
        -- else
        --     None

    else
        ghost.dir
