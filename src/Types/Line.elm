module Types.Line exposing (Line)

import Types.Point exposing (Point)


type alias Line =
    { start : Point
    , end : Point
    }
