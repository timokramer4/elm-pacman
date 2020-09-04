module Movement exposing (checkDir, checkPath, outOfBounds)

import Dict exposing (Dict)
import Settings exposing (fieldSettings, movement, runMesh)
import Types.GameModels exposing (Direction(..), Game)
import Types.Line exposing (Line, LineType(..))
import Types.Point exposing (Point)



-- Checks if PacMan is outside the paths


outOfBounds : Game -> Bool
outOfBounds game =
    game.pPosition.x < 0 || game.pPosition.x > fieldSettings.width || game.pPosition.y < 0 || game.pPosition.y > fieldSettings.height



-- Checks if the path is possible in a certain direction


checkDir : Point -> Direction -> LineType -> Bool
checkDir point dir ghost =
    case dir of
        Left ->
            List.foldl ((\x y -> checkPath x y)  { x = point.x - movement, y = point.y } ghost) False (Dict.values runMesh)

        Right ->
            List.foldl ((\x y -> checkPath x y)  { x = point.x + movement, y = point.y } ghost) False (Dict.values runMesh)

        Up ->
            List.foldl ((\x y -> checkPath x y)  { x = point.x, y = point.y - movement } ghost) False (Dict.values runMesh)

        Down ->
            List.foldl ((\x y -> checkPath x y)  { x = point.x, y = point.y + movement } ghost) False (Dict.values runMesh)

        _ ->
            False


-- Checks if a point is on a path
checkPath : Point -> LineType -> Line -> Bool -> Bool
checkPath pos lType line e =
    (pos.x >= min line.start.x line.end.x && pos.x <= max line.start.x line.end.x && pos.y >= min line.start.y line.end.y && pos.y <= max line.start.y line.end.y && (line.linetype == lType || line.linetype == Both)) || e
