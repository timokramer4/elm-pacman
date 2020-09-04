module Types.Line exposing (Line, LineType(..))

import Types.Point exposing (Point)



-- Line alias


type alias Line =
    { start : Point
    , end : Point
    , linetype : LineType
    }



-- LineType


type LineType
    = Pacman
    | Ghost
    | GhostStartLine
    | Both
