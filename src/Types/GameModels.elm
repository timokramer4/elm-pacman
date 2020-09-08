module Types.GameModels exposing
    ( Direction(..)
    , Game
    , Ghost
    , GhostColors(..)
    , GhostName(..)
    , Msg(..)
    , ScoreMessage
    , SoundModel(..)
    , SoundState(..)
    , StartMode(..)
    , State(..)
    )

import Audio
import Time
import Types.Point exposing (Point)



-----------------------
-- GLOBAL GAME MODEL --
-----------------------


type alias Game =
    { pPosition : Point
    , pRotation : Int
    , pacmanSrc : String
    , mouthMovement : Bool
    , lifes : Int
    , state : State
    , nextDir : Direction
    , score : Int
    , message : String
    , items : List Point
    , totalItemCount : Int
    , pills : List Point
    , itemCounter : Int
    , fruitSecondCounter : Int
    , fruitAvailable : Bool
    , redGhost : Ghost
    , pinkGhost : Ghost
    , blueGhost : Ghost
    , yellowGhost : Ghost
    , pillActive : Bool
    , pillSecondCounter : Int
    , eatenGhostsCounter : Int
    , level : Int
    , sound : SoundModel
    , eatItem : Bool
    , eatItemSecondCounter : Int
    , nextGhostCanGoOut : Bool
    , scoreMessage : ScoreMessage
    , showScoreMessage : Bool
    }



-----------------------------
-- LOADED SOUND FILE/STATE --
-----------------------------


type alias LoadedModel_ =
    { sound : Audio.Source
    , soundState : SoundState
    }



------------------
-- SOUND STATES --
------------------


type SoundState
    = NotPlaying
    | Playing Time.Posix
    | FadingOut Time.Posix Time.Posix



------------------------------------------------
-- DIFFERENT STATES WHEN LOADING A SOUND FILE --
------------------------------------------------


type SoundModel
    = LoadingModel
    | LoadedModel LoadedModel_
    | LoadFailedModel



-----------------
-- GHOST MODEL --
-----------------


type alias Ghost =
    { color : GhostColors
    , position : Point
    , dir : Direction
    , active : Bool
    , offset : Int
    , src : String
    , goBackInPrison : Bool
    , name : GhostName
    , running : Bool
    }



-------------------
-- SCORE MESSAGE --
-------------------


type alias ScoreMessage =
    { point : Point
    , msg : String
    }



---------------------------
-- GLOBAL MESSAGE STRUCT --
---------------------------


type Msg
    = MoveDirection Direction
    | ChangeState
    | DoNothing
    | ChangeDirection Direction
    | Fruit
    | Pill
    | GhostMove GhostName
    | ChangeColor
    | ChangePacmanSrc
    | ResetGame StartMode
    | StartGame
    | SoundLoaded (Result Audio.LoadError Audio.Source)
    | GetCurrentTime Audio.Source Time.Posix
    | EatWaiter
    | ClearScoreMsg



-----------------
-- START MODES --
-----------------


type StartMode
    = NewLevel
    | NormalReset
    | Init



-----------------
-- GAME STATES --
-----------------


type State
    = Running Direction
    | Stopped Direction
    | Waiting



-------------------------------
-- AVAILABLE MOVE DIRECTIONS --
-------------------------------


type Direction
    = Up
    | Down
    | Left
    | Right
    | None



------------------
-- GHOST COLORS --
------------------


type GhostColors
    = Red
    | Pink
    | Blue
    | Yellow
    | Hunted
    | White
    | GoBackInPrison



-----------------
-- GHOST NAMES --
-----------------


type GhostName
    = Blinky
    | Pinky
    | Inky
    | Clyde
