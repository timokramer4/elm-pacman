module Types.Line exposing (Line, LineType(..))

import Types.Point exposing (Point)


type alias Line =
    { start : Point
    , end : Point
    , linetype : LineType
    }


type LineType
    = Pacman
    | Ghost
    | GhostStartLine
    | Both
