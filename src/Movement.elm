module Movement exposing (..)
import Types.Point exposing (Point)
import Types.GameModels exposing (Direction(..), Game)
import Settings exposing (movement, fieldSettings, runMesh)
import Dict exposing (Dict)
import Types.Line exposing (Line)

outOfBounds : Game -> Bool
outOfBounds game =
    game.pPosition.x < 0 || game.pPosition.x > fieldSettings.width || game.pPosition.y < 0 || game.pPosition.y > fieldSettings.height


checkDir : Point -> Direction -> Bool
checkDir point dir =
    case dir of
        Left ->
            getMesh runMesh { x = point.x - movement, y = point.y }

        Right ->
            getMesh runMesh { x = point.x + movement, y = point.y }

        Up ->
            getMesh runMesh { x = point.x, y = point.y - movement }

        Down ->
            getMesh runMesh { x = point.x, y = point.y + movement }

        _ ->
            False

getMesh : Dict Int Line -> Point -> Bool
getMesh mesh pos =
    List.foldl ((\x -> checkPath x) pos) False (Dict.values mesh)


checkPath : Point -> Line -> Bool -> Bool
checkPath pos line e =
    (pos.x >= min line.start.x line.end.x && pos.x <= max line.start.x line.end.x && pos.y >= min line.start.y line.end.y && pos.y <= max line.start.y line.end.y) || e