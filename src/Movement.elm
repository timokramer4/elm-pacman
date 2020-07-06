module Movement exposing (..)

import Dict exposing (Dict)
import Settings exposing (fieldSettings, movement, runMesh)
import Types.GameModels exposing (Direction(..), Game)
import Types.Line exposing (Line, LineType(..))
import Types.Point exposing (Point)


outOfBounds : Game -> Bool
outOfBounds game =
    game.pPosition.x < 0 || game.pPosition.x > fieldSettings.width || game.pPosition.y < 0 || game.pPosition.y > fieldSettings.height


checkDir : Point -> Direction -> LineType -> Bool
checkDir point dir ghost =
    case dir of
        Left ->
            getMesh runMesh { x = point.x - movement, y = point.y } ghost

        Right ->
            getMesh runMesh { x = point.x + movement, y = point.y } ghost

        Up ->
            getMesh runMesh { x = point.x, y = point.y - movement } ghost

        Down ->
            getMesh runMesh { x = point.x, y = point.y + movement } ghost

        _ ->
            False


getMesh : Dict Int Line -> Point -> LineType -> Bool
getMesh mesh pos ghost =
    List.foldl ((\x y -> checkPath x y) pos ghost) False (Dict.values mesh)


checkPath : Point -> LineType -> Line -> Bool -> Bool
checkPath pos lType line e =
    (pos.x >= min line.start.x line.end.x && pos.x <= max line.start.x line.end.x && pos.y >= min line.start.y line.end.y && pos.y <= max line.start.y line.end.y && (line.linetype == lType || line.linetype == Both)) || e
