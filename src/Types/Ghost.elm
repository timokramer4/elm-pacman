module Types.Ghost exposing (..)

import Arithmetic exposing (..)
import Html.Attributes exposing (target)
import Movement exposing (checkDir)
import Settings exposing (ghostSettings, movement, pacSettings)
import Types.GameModels exposing (Direction(..), Game, Ghost, State(..))
import Types.Line exposing (LineType(..))
import Types.Point exposing (Point)


moveGhost : Ghost -> Direction -> Ghost
moveGhost ghost dir =
    let
        ghostNextPos =
            case dir of
                Left ->
                    { x = ghost.position.x - movement, y = ghost.position.y }

                Right ->
                    { x = ghost.position.x + movement, y = ghost.position.y }

                Up ->
                    { x = ghost.position.x, y = ghost.position.y - movement }

                Down ->
                    { x = ghost.position.x, y = ghost.position.y + movement }

                _ ->
                    ghost.position

        activeState =
            if not ghost.active then
                not ghost.active && ghostNextPos == ghostSettings.startPosition

            else
                ghost.active
    in
    { position = ghostNextPos, dir = dir, active = activeState, offset = ghost.offset }


getGhostNextDir : Game -> Ghost -> Direction
getGhostNextDir game ghost =
    let
        currentType =
            if not ghost.active then
                GhostStartLine

            else
                Types.Line.Ghost
    in
    -- check if cross availeble
    if ghost.dir == None || ((ghost.dir == Left || ghost.dir == Right) && (checkDir ghost.position Up currentType || checkDir ghost.position Down currentType)) || ((ghost.dir == Up || ghost.dir == Down) && (checkDir ghost.position Left currentType || checkDir ghost.position Right currentType)) then
        let
            targetPos =
                if not ghost.active then
                    ghostSettings.startPosition

                else
                    case game.state of
                        Running dir ->
                            case dir of
                                Left ->
                                    { x = game.pPosition.x - ghost.offset * pacSettings.ratio, y = game.pPosition.y }

                                Right ->
                                    { x = game.pPosition.x + ghost.offset * pacSettings.ratio, y = game.pPosition.y }

                                Down ->
                                    { x = game.pPosition.x, y = game.pPosition.y + ghost.offset * pacSettings.ratio }

                                Up ->
                                    { x = game.pPosition.x, y = game.pPosition.y - ghost.offset * pacSettings.ratio }

                                _ ->
                                    game.pPosition

                        _ ->
                            game.pPosition

            xDif =
                max targetPos.x ghost.position.x - min targetPos.x ghost.position.x

            yDif =
                max targetPos.y ghost.position.y - min targetPos.y ghost.position.y

            -- save neares crosses
            nextHorizontalCross =
                if getVectorLength (checkNextCross ghost.position Left) targetPos > getVectorLength (checkNextCross ghost.position Right) targetPos then
                    Left

                else
                    Right

            nextVerticalCross =
                if getVectorLength (checkNextCross ghost.position Up) targetPos > getVectorLength (checkNextCross ghost.position Down) targetPos then
                    Up

                else
                    Down

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

                else if targetPos == ghost.position then
                    { vertical = None
                    , horizontal = None
                    , cVertical = None
                    , cHorizontal = None
                    }

                else if targetPos.x == ghost.position.x then
                    if targetPos.y > ghost.position.y then
                        { vertical = Down
                        , horizontal = nextHorizontalCross
                        , cVertical = Up
                        , cHorizontal = None
                        }

                    else
                        { vertical = Up
                        , horizontal = nextHorizontalCross
                        , cVertical = Down
                        , cHorizontal = None
                        }

                else if targetPos.y == ghost.position.y then
                    if targetPos.x > ghost.position.x then
                        { vertical = nextVerticalCross
                        , horizontal = Right
                        , cVertical = None
                        , cHorizontal = Left
                        }

                    else
                        { vertical = nextVerticalCross
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
        -- if ghost.position == targetPos && ghost.active then
        --     None
        -- else
        if (checkDir ghost.position moveOptions.horizontal currentType && ghost.dir /= moveOptions.cHorizontal) && (checkDir ghost.position moveOptions.vertical currentType && ghost.dir /= moveOptions.cVertical) then
            if xDif > yDif then
                moveOptions.horizontal

            else
                moveOptions.vertical

        else if checkDir ghost.position moveOptions.horizontal currentType && ghost.dir /= moveOptions.cHorizontal then
            moveOptions.horizontal

        else if checkDir ghost.position moveOptions.vertical currentType && ghost.dir /= moveOptions.cVertical then
            moveOptions.vertical

        else if (checkDir ghost.position moveOptions.cHorizontal currentType && ghost.dir /= moveOptions.horizontal) && (checkDir ghost.position moveOptions.cVertical currentType && ghost.dir /= moveOptions.vertical) then
            let
                horizontalNextCross =
                    checkNextCross ghost.position moveOptions.cHorizontal

                verticalNextCross =
                    checkNextCross ghost.position moveOptions.cVertical
            in
            if getVectorLength verticalNextCross targetPos < getVectorLength horizontalNextCross targetPos then
                moveOptions.cVertical

            else
                moveOptions.cHorizontal

        else if checkDir ghost.position moveOptions.cHorizontal currentType && ghost.dir /= moveOptions.horizontal then
            moveOptions.cHorizontal

        else if checkDir ghost.position moveOptions.cVertical currentType && ghost.dir /= moveOptions.vertical then
            moveOptions.cVertical

        else
            None

    else
        ghost.dir


checkNextCross : Point -> Direction -> Point
checkNextCross pos dir =
    if checkDir pos dir Types.Line.Ghost && (not (checkDir pos Right Types.Line.Ghost) && not (checkDir pos Up Types.Line.Ghost) && not (checkDir pos Down Types.Line.Ghost)) || (not (checkDir pos Left Types.Line.Ghost) && not (checkDir pos Right Types.Line.Ghost) && not (checkDir pos Down Types.Line.Ghost)) || (not (checkDir pos Left Types.Line.Ghost) && not (checkDir pos Up Types.Line.Ghost) && not (checkDir pos Down Types.Line.Ghost)) || (not (checkDir pos Left Types.Line.Ghost) && not (checkDir pos Right Types.Line.Ghost) && not (checkDir pos Up Types.Line.Ghost)) then
        case dir of
            Left ->
                checkNextCross { x = pos.x + movement, y = pos.y } dir

            Right ->
                checkNextCross { x = pos.x - movement, y = pos.y } dir

            Down ->
                checkNextCross { x = pos.x, y = pos.y + movement } dir

            Up ->
                checkNextCross { x = pos.x, y = pos.y - movement } dir

            _ ->
                pos

    else
        pos


getVectorLength : Point -> Point -> Int
getVectorLength p1 p2 =
    let
        dv =
            { x = p1.x - p2.x, y = p1.y - p2.y }
    in
    intSquareRoot (dv.x * dv.x + dv.y * dv.y)
