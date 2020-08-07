module Types.Ghost exposing (changeGhostSrc, getGhostSrc, changeGoBackInPrison, checkGhoastEatingPacMan, getGhostNextDir, huntedColorChange, moveGhoastToPosition, moveGhost, activateGhost)

import Arithmetic exposing (intSquareRoot)
import Movement exposing (checkDir)
import Settings exposing (getPoint, ghostSettings, movement, pacSettings)
import Types.GameModels exposing (Direction(..), Game, Ghost, GhostColors(..), State(..), GhostName(..))
import Types.Line exposing (LineType(..))
import Types.Point exposing (Point)



--------------------------------------
-- Move ghost in specific direction --
--------------------------------------


moveGhost : Ghost -> Direction -> Bool -> Ghost
moveGhost ghost dir goBackInPrison =
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
    if (goBackInPrison && ghost.goBackInPrison) || not ghost.goBackInPrison then
        {name= ghost.name, color = ghost.color, position = ghostNextPos, dir = dir, active = activeState, offset = ghost.offset, src = getGhostSrc ghost.color dir, goBackInPrison = ghost.goBackInPrison, running = ghost.running }

    else
        ghost



-------------------------------------
-- Move ghost in specific position --
-------------------------------------


moveGhoastToPosition : Ghost -> Point -> Ghost
moveGhoastToPosition ghost target =
    { name= ghost.name, color = ghost.color, position = target, dir = Up, active = False, offset = ghost.offset, src = ghost.src, goBackInPrison = ghost.goBackInPrison, running = ghost.running }

changeGoBackInPrison : Ghost -> Bool -> Ghost
changeGoBackInPrison ghost value =
    {name= ghost.name, color = ghost.color, position = ghost.position, dir = Up, active = False, offset = ghost.offset, src = ghost.src, goBackInPrison = value, running = ghost.running }

activateGhost : Ghost -> Ghost
activateGhost ghost =
    {name= ghost.name, color = ghost.color, position = ghost.position, dir = Up, active = False, offset = ghost.offset, src = ghost.src, goBackInPrison = ghost.goBackInPrison, running = True }    

-------------------------------------
-- Change ghost color --
-------------------------------------


changeGhostSrc : Ghost -> GhostColors -> Ghost
changeGhostSrc ghost color =
  
    { name= ghost.name, color = color, position = ghost.position, dir = Up, active = False, offset = ghost.offset, src = getGhostSrc color ghost.dir, goBackInPrison = ghost.goBackInPrison, running = ghost.running }


getGhostSrc : GhostColors -> Direction -> String
getGhostSrc color dir =
  let
        ghostSrcDir = 
           case dir of
                Left ->
                    "_left"

                Right ->
                    "_right"

                Up ->
                    "_up"

                Down ->
                    "_down"

                _ ->
                    ""
        in
        case color of
            Red ->
                "blinky/blinky" ++ ghostSrcDir

            Pink ->
                "pinky/pinky" ++ ghostSrcDir

            Yellow ->
                "clyde/clyde" ++ ghostSrcDir

            Blue ->
                "inky/inky" ++ ghostSrcDir

            Hunted ->
                "hunted"

            White ->
                "hunted_white"
            GoBackInPrison ->
                "eyes/eyes" ++ ghostSrcDir    


-----------------------------------------
---------- change hunted color ----------
-----------------------------------------


huntedColorChange : Ghost -> Ghost
huntedColorChange ghost =
    if ghost.src == "hunted" then
        changeGhostSrc ghost White

    else if ghost.src == "hunted_white" then
        changeGhostSrc ghost Hunted

    else
        ghost



-----------------------------------------
-- Calculate next ghost move direction --
-----------------------------------------


getGhostNextDir : Game -> Ghost -> Bool -> Direction
getGhostNextDir game ghost goBackToPrison =
    let
        currentType =
            if not ghost.active then
                GhostStartLine

            else
                Types.Line.Ghost

        targetPos =
            if goBackToPrison then
                ghostSettings.startPosition

            else if ghost.name == Clyde && getVectorLength ghost.position game.pPosition > 8 * pacSettings.ratio && currentType /= GhostStartLine then
                getPoint 44

            else if not ghost.active then
                ghostSettings.startPosition

            else
                case game.state of
                    Running dir ->
                        case dir of
                            Left ->
                                let
                                    pos =
                                        { x = game.pPosition.x - ghost.offset * pacSettings.ratio, y = game.pPosition.y }
                                in
                                if ghost.name == Inky then
                                    { x = game.pPosition.x - getBlueGhoastOffset game.redGhost.position pos, y = game.pPosition.y }

                                else
                                    pos

                            Right ->
                                let
                                    pos =
                                        { x = game.pPosition.x + ghost.offset * pacSettings.ratio, y = game.pPosition.y }
                                in
                                if ghost.name == Inky then
                                    { x = game.pPosition.x + getBlueGhoastOffset game.redGhost.position pos, y = game.pPosition.y }

                                else
                                    pos

                            Down ->
                                let
                                    pos =
                                        { x = game.pPosition.x, y = game.pPosition.y + ghost.offset * pacSettings.ratio }
                                in
                                if ghost.name == Inky then
                                    { x = game.pPosition.x, y = game.pPosition.y + getBlueGhoastOffset game.redGhost.position pos }

                                else
                                    pos

                            Up ->
                                let
                                    pos =
                                        { x = game.pPosition.x, y = game.pPosition.y - ghost.offset * pacSettings.ratio }
                                in
                                if ghost.name == Inky then
                                    { x = game.pPosition.x, y = game.pPosition.y - getBlueGhoastOffset game.redGhost.position pos }

                                else
                                    pos

                            _ ->
                                game.pPosition

                    _ ->
                        game.pPosition
    in
    -- Only check if cross available
    if ghost.dir == None || ((ghost.dir == Left || ghost.dir == Right) && (checkDir ghost.position Up currentType || checkDir ghost.position Down currentType)) || ((ghost.dir == Up || ghost.dir == Down) && (checkDir ghost.position Left currentType || checkDir ghost.position Right currentType)) then
        let
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
                        , horizontal = nextHorizontalCross
                        , cVertical = Up
                        , cHorizontal = nextHorizontalCross
                        }

                    else
                        { vertical = Up
                        , horizontal = nextHorizontalCross
                        , cVertical = Down
                        , cHorizontal = nextHorizontalCross
                        }

                else if targetPos.y == ghost.position.y then
                    if targetPos.x > ghost.position.x then
                        { vertical = nextVerticalCross
                        , horizontal = Right
                        , cVertical = nextVerticalCross
                        , cHorizontal = Left
                        }

                    else
                        { vertical = nextVerticalCross
                        , horizontal = Left
                        , cVertical = nextVerticalCross
                        , cHorizontal = Right
                        }

                else
                    { vertical = None
                    , horizontal = None
                    , cVertical = None
                    , cHorizontal = None
                    }

            -- save neares crosses
            nextHorizontalCross =
                if getVectorLength (getNextCross ghost.position Left) targetPos > getVectorLength (getNextCross ghost.position Right) targetPos then
                    Left

                else
                    Right

            nextVerticalCross =
                if getVectorLength (getNextCross ghost.position Up) targetPos > getVectorLength (getNextCross ghost.position Down) targetPos then
                    Up

                else
                    Down

            xDif =
                max targetPos.x ghost.position.x - min targetPos.x ghost.position.x

            yDif =
                max targetPos.y ghost.position.y - min targetPos.y ghost.position.y
        in
        -- Both directions are available
        if (checkDir ghost.position moveOptions.horizontal currentType && ghost.dir /= moveOptions.cHorizontal) && (checkDir ghost.position moveOptions.vertical currentType && ghost.dir /= moveOptions.cVertical) then
            if xDif < yDif then
                moveOptions.vertical

            else
                moveOptions.horizontal
            -- Only horizontal direction available

        else if checkDir ghost.position moveOptions.horizontal currentType && ghost.dir /= moveOptions.cHorizontal then
            moveOptions.horizontal
            -- Only vertical direction available

        else if checkDir ghost.position moveOptions.vertical currentType && ghost.dir /= moveOptions.cVertical then
            moveOptions.vertical
            -- Both negative directions are available

        else if (checkDir ghost.position moveOptions.cHorizontal currentType && ghost.dir /= moveOptions.horizontal) && (checkDir ghost.position moveOptions.cVertical currentType && ghost.dir /= moveOptions.vertical) then
            let
                horizontalNextCross =
                    getNextCross ghost.position moveOptions.cHorizontal

                verticalNextCross =
                    getNextCross ghost.position moveOptions.cVertical
            in
            if getVectorLength verticalNextCross targetPos < getVectorLength horizontalNextCross targetPos then
                moveOptions.cVertical

            else
                moveOptions.cHorizontal
            -- Only negative horizontal direction available

        else if checkDir ghost.position moveOptions.cHorizontal currentType && ghost.dir /= moveOptions.horizontal then
            moveOptions.cHorizontal
            -- Only negative vertical direction available

        else if checkDir ghost.position moveOptions.cVertical currentType && ghost.dir /= moveOptions.vertical then
            moveOptions.cVertical

        else
            None

    else
        ghost.dir



----------------------------------
-- Get nearest cross from point --
----------------------------------


getNextCross : Point -> Direction -> Point
getNextCross pos dir =
    if checkDir pos dir Types.Line.Ghost && (not (checkDir pos Right Types.Line.Ghost) && not (checkDir pos Up Types.Line.Ghost) && not (checkDir pos Down Types.Line.Ghost)) || (not (checkDir pos Left Types.Line.Ghost) && not (checkDir pos Right Types.Line.Ghost) && not (checkDir pos Down Types.Line.Ghost)) || (not (checkDir pos Left Types.Line.Ghost) && not (checkDir pos Up Types.Line.Ghost) && not (checkDir pos Down Types.Line.Ghost)) || (not (checkDir pos Left Types.Line.Ghost) && not (checkDir pos Right Types.Line.Ghost) && not (checkDir pos Up Types.Line.Ghost)) then
        case dir of
            Left ->
                getNextCross { x = pos.x + movement, y = pos.y } dir

            Right ->
                getNextCross { x = pos.x - movement, y = pos.y } dir

            Down ->
                getNextCross { x = pos.x, y = pos.y + movement } dir

            Up ->
                getNextCross { x = pos.x, y = pos.y - movement } dir

            _ ->
                pos

    else
        pos



------------------------------------------------
-- Calculate blue ghost offset with red ghost --
------------------------------------------------


getBlueGhoastOffset : Point -> Point -> Int
getBlueGhoastOffset redGhostPos pacPos =
    getVectorLength redGhostPos pacPos * 2



-------------------------------------------
-- Calculate two-dimension vector length --
-------------------------------------------


getVectorLength : Point -> Point -> Int
getVectorLength p1 p2 =
    let
        dv =
            { x = p1.x - p2.x, y = p1.y - p2.y }
    in
    intSquareRoot (dv.x * dv.x + dv.y * dv.y)



-------------------------------------------
-------- Check if ghost eat pacMan  -------
-------------------------------------------


checkGhoastEatingPacMan : Point -> Point -> Bool
checkGhoastEatingPacMan pacPos ghostPos =
    pacPos /= ghostPos && { x = pacPos.x + 1, y = pacPos.y } /= ghostPos && { x = pacPos.x - 1, y = pacPos.y } /= ghostPos && { x = pacPos.x, y = pacPos.y + 1 } /= ghostPos && { x = pacPos.x, y = pacPos.y - 1 } /= ghostPos
