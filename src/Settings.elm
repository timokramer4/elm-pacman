module Settings exposing (fieldSettings, fruitSettings, itemSettings, movement, pacSettings, pillSettings, pillsList, pointMesh, runMesh)

import Dict exposing (Dict, get)
import Types.Line exposing (Line)
import Types.Point exposing (Point)



-----------------------
--   GAME SETTINGS   --
-----------------------


fieldSettings : { width : Int, height : Int, borderColor : String }
fieldSettings =
    { width = 500
    , height = 500
    , borderColor = "#3498DB"
    }


pacSettings : { ratio : Int }
pacSettings =
    { ratio = 25
    }


movement : Int
movement =
    1


itemSettings : { fill : String, step : Int, size : Int, xp : Int }
itemSettings =
    { fill = "#FFAAA5"
    , step = 15
    , size = 5
    , xp = 10
    }


pillSettings : { fill : String, radius : Int, xp : Int }
pillSettings =
    { fill = "#FFAAA5"
    , radius = 6
    , xp = 50
    }


fruitSettings : { position : Point, ratio : Float, xp : Float, itemNumber1 : Int, itemNumber2 : Int }
fruitSettings =
    { position = { x = 250, y = 280 }
    , ratio = 20
    , xp = 100
    , itemNumber1 = 10
    , itemNumber2 = 170
    }


pointMesh : Dict Int Point
pointMesh =
    Dict.fromList
        [ ( 1, { x = 25, y = 25 } )
        , ( 2, { x = 115, y = 25 } )
        , ( 3, { x = 220, y = 25 } )
        , ( 4, { x = 280, y = 25 } )
        , ( 5, { x = 385, y = 25 } )
        , ( 6, { x = 475, y = 25 } )
        , ( 7, { x = 25, y = 85 } )
        , ( 8, { x = 175, y = 85 } )
        , ( 9, { x = 220, y = 85 } )
        , ( 10, { x = 280, y = 85 } )
        , ( 11, { x = 325, y = 85 } )
        , ( 12, { x = 475, y = 85 } )
        , ( 13, { x = 25, y = 145 } )
        , ( 14, { x = 115, y = 145 } )
        , ( 15, { x = 175, y = 145 } )
        , ( 16, { x = 220, y = 145 } )
        , ( 17, { x = 280, y = 145 } )
        , ( 18, { x = 325, y = 145 } )
        , ( 19, { x = 385, y = 145 } )
        , ( 20, { x = 475, y = 145 } )
        , ( 21, { x = 175, y = 190 } )
        , ( 22, { x = 220, y = 190 } )
        , ( 23, { x = 280, y = 190 } )
        , ( 24, { x = 325, y = 190 } )
        , ( 25, { x = -5, y = 235 } )
        , ( 26, { x = 175, y = 235 } )
        , ( 27, { x = 325, y = 235 } )
        , ( 28, { x = 505, y = 235 } )
        , ( 29, { x = 175, y = 280 } )
        , ( 30, { x = 325, y = 280 } )
        , ( 31, { x = 25, y = 325 } )
        , ( 32, { x = 175, y = 325 } )
        , ( 33, { x = 220, y = 325 } )
        , ( 34, { x = 280, y = 325 } )
        , ( 35, { x = 325, y = 325 } )
        , ( 36, { x = 475, y = 325 } )
        , ( 37, { x = 25, y = 370 } )
        , ( 38, { x = 55, y = 370 } )
        , ( 39, { x = 115, y = 370 } )
        , ( 40, { x = 175, y = 370 } )
        , ( 41, { x = 220, y = 370 } )
        , ( 42, { x = 280, y = 370 } )
        , ( 43, { x = 325, y = 370 } )
        , ( 44, { x = 385, y = 370 } )
        , ( 45, { x = 445, y = 370 } )
        , ( 46, { x = 475, y = 370 } )
        , ( 47, { x = 25, y = 430 } )
        , ( 48, { x = 55, y = 430 } )
        , ( 49, { x = 115, y = 430 } )
        , ( 50, { x = 175, y = 430 } )
        , ( 51, { x = 220, y = 430 } )
        , ( 52, { x = 280, y = 430 } )
        , ( 53, { x = 325, y = 430 } )
        , ( 54, { x = 385, y = 430 } )
        , ( 55, { x = 445, y = 430 } )
        , ( 56, { x = 475, y = 430 } )
        , ( 57, { x = 25, y = 475 } )
        , ( 58, { x = 220, y = 475 } )
        , ( 59, { x = 280, y = 475 } )
        , ( 60, { x = 475, y = 475 } )
        ]


runMesh : Dict Int Line
runMesh =
    Dict.fromList
        [ ( 1, Line (getPoint 29) (getPoint 30) )
        , ( 2, Line (getPoint 21) (getPoint 32) )
        , ( 3, Line (getPoint 21) (getPoint 24) )
        , ( 4, Line (getPoint 24) (getPoint 35) )
        , ( 5, Line (getPoint 1) (getPoint 3) )
        , ( 6, Line (getPoint 1) (getPoint 13) )
        , ( 7, Line (getPoint 7) (getPoint 12) )
        , ( 8, Line (getPoint 2) (getPoint 49) )
        , ( 9, Line (getPoint 3) (getPoint 9) )
        , ( 10, Line (getPoint 4) (getPoint 10) )
        , ( 11, Line (getPoint 4) (getPoint 6) )
        , ( 12, Line (getPoint 5) (getPoint 54) )
        , ( 13, Line (getPoint 6) (getPoint 20) )
        , ( 14, Line (getPoint 19) (getPoint 20) )
        , ( 15, Line (getPoint 11) (getPoint 18) )
        , ( 16, Line (getPoint 17) (getPoint 18) )
        , ( 17, Line (getPoint 17) (getPoint 23) )
        , ( 18, Line (getPoint 8) (getPoint 15) )
        , ( 19, Line (getPoint 15) (getPoint 16) )
        , ( 20, Line (getPoint 16) (getPoint 22) )
        , ( 21, Line (getPoint 25) (getPoint 26) )
        , ( 22, Line (getPoint 27) (getPoint 28) )
        , ( 23, Line (getPoint 31) (getPoint 33) )
        , ( 24, Line (getPoint 34) (getPoint 36) )
        , ( 25, Line (getPoint 39) (getPoint 44) )
        , ( 26, Line (getPoint 33) (getPoint 41) )
        , ( 27, Line (getPoint 34) (getPoint 42) )
        , ( 28, Line (getPoint 31) (getPoint 37) )
        , ( 29, Line (getPoint 37) (getPoint 38) )
        , ( 30, Line (getPoint 38) (getPoint 48) )
        , ( 31, Line (getPoint 47) (getPoint 49) )
        , ( 32, Line (getPoint 57) (getPoint 60) )
        , ( 33, Line (getPoint 40) (getPoint 50) )
        , ( 34, Line (getPoint 50) (getPoint 51) )
        , ( 35, Line (getPoint 51) (getPoint 58) )
        , ( 36, Line (getPoint 57) (getPoint 47) )
        , ( 37, Line (getPoint 43) (getPoint 53) )
        , ( 38, Line (getPoint 52) (getPoint 53) )
        , ( 39, Line (getPoint 52) (getPoint 59) )
        , ( 40, Line (getPoint 36) (getPoint 46) )
        , ( 41, Line (getPoint 45) (getPoint 46) )
        , ( 42, Line (getPoint 45) (getPoint 55) )
        , ( 43, Line (getPoint 54) (getPoint 56) )
        , ( 44, Line (getPoint 56) (getPoint 60) )
        , ( 45, Line (getPoint 13) (getPoint 14) )
        ]


pillsList : List Point
pillsList =
    [ { x = 25, y = 115 }
    , { x = 55, y = 400 }
    , { x = 385, y = 55 }
    , { x = 325, y = 430 }
    ]



-----------------------
-- SETTING FUNCTIONS --
-----------------------


getPoint : Int -> Point
getPoint i =
    case get i pointMesh of
        Just point ->
            point

        _ ->
            { x = 0, y = 0 }
