port module Main exposing (init, main)

import Audio
import Browser.Events exposing (onKeyDown)
import Delay exposing (TimeUnit(..), after)
import Dict exposing (member)
import Eatable
    exposing
        ( checkEatable
        , createFruit
        , createPoints
        , pointsToSvg
        , setScoreMsg
        )
import Html exposing (Html, div, img, node, text)
import Html.Attributes exposing (class, height, id, src, style, width)
import Json.Decode exposing (Decoder, Value)
import Json.Encode exposing (Value)
import List exposing (length)
import List.Unique exposing (filterDuplicates)
import Movement exposing (checkDir, outOfBounds)
import Settings
    exposing
        ( fieldSettings
        , fruitSettings
        , gameMessages
        , ghostSettings
        , itemSettings
        , movement
        , pacSettings
        , pillsList
        , runMesh
        )
import Style exposing (..)
import Svg
    exposing
        ( Svg
        , line
        , path
        , polygon
        , svg
        )
import Svg.Attributes
    exposing
        ( d
        , fill
        , points
        , stroke
        , strokeWidth
        , x1
        , x2
        , y1
        , y2
        )
import Task exposing (perform)
import Time exposing (every)
import Types.GameModels
    exposing
        ( Direction(..)
        , Game
        , GhostColors(..)
        , GhostName(..)
        , Msg(..)
        , SoundModel(..)
        , SoundState(..)
        , StartMode(..)
        , State(..)
        )
import Types.Ghost
    exposing
        ( changeGhostSrc
        , changeGoBackInPrison
        , checkGhoastEatingPacMan
        , getGhostNextDir
        , getGhostSrc
        , huntedColorChange
        , moveGhoastToPosition
        , moveGhost
        , setActiveState
        , setGhostRunning
        )
import Types.Line exposing (LineType(..))
import Types.Point exposing (Point)



----------
-- INIT --
----------


initialModel : Game
initialModel =
    resetGame 3 0 [] [] 0 1 Init



------------
-- UPDATE --
------------


update : Msg -> Game -> ( Game, Cmd Msg, Audio.AudioCmd Msg )
update msg game =
    case msg of
        -- Default movement
        MoveDirection d ->
            case d of
                Left ->
                    if outOfBounds game then
                        ( { game
                            | pPosition = changeXPosition fieldSettings.width game
                            , state = Running d
                            , pRotation = 180
                          }
                        , Cmd.none
                        , Audio.cmdNone
                        )

                    else if
                        checkDir game.pPosition d Pacman
                            || checkDir game.pPosition
                                game.nextDir
                                Pacman
                    then
                        if
                            checkDir game.pPosition game.nextDir Pacman
                                && game.nextDir
                                /= d
                        then
                            ( { game
                                | state = Running game.nextDir
                                , nextDir = None
                              }
                            , Cmd.none
                            , Audio.cmdNone
                            )

                        else
                            ( checkEatable
                                { game
                                    | pPosition =
                                        changeXPosition
                                            (game.pPosition.x - movement)
                                            game
                                    , state = Running d
                                    , pRotation = 180
                                }
                            , Cmd.none
                            , Audio.cmdNone
                            )

                    else
                        update DoNothing game

                Right ->
                    if outOfBounds game then
                        ( { game
                            | pPosition = changeXPosition 0 game
                            , state = Running d
                            , pRotation = 0
                          }
                        , Cmd.none
                        , Audio.cmdNone
                        )

                    else if
                        checkDir game.pPosition d Pacman
                            || checkDir game.pPosition game.nextDir Pacman
                    then
                        if checkDir game.pPosition game.nextDir Pacman && game.nextDir /= d then
                            ( { game | state = Running game.nextDir, nextDir = None }
                            , Cmd.none
                            , Audio.cmdNone
                            )

                        else
                            ( checkEatable
                                { game
                                    | pPosition =
                                        changeXPosition
                                            (game.pPosition.x + movement)
                                            game
                                    , state = Running d
                                    , pRotation = 0
                                }
                            , Cmd.none
                            , Audio.cmdNone
                            )

                    else
                        update DoNothing game

                Up ->
                    if outOfBounds game then
                        ( { game
                            | pPosition = changeYPosition fieldSettings.height game
                            , state = Running d
                            , pRotation = -90
                          }
                        , Cmd.none
                        , Audio.cmdNone
                        )

                    else if
                        checkDir game.pPosition d Pacman
                            || checkDir game.pPosition game.nextDir Pacman
                    then
                        if checkDir game.pPosition game.nextDir Pacman && game.nextDir /= d then
                            ( { game
                                | state = Running game.nextDir
                                , nextDir = None
                              }
                            , Cmd.none
                            , Audio.cmdNone
                            )

                        else
                            ( checkEatable
                                { game
                                    | pPosition =
                                        changeYPosition
                                            (game.pPosition.y - movement)
                                            game
                                    , state = Running d
                                    , pRotation = -90
                                }
                            , Cmd.none
                            , Audio.cmdNone
                            )

                    else
                        update DoNothing game

                Down ->
                    if outOfBounds game then
                        ( { game
                            | pPosition = changeYPosition 0 game
                            , state = Running d
                            , pRotation = 90
                          }
                        , Cmd.none
                        , Audio.cmdNone
                        )

                    else if
                        checkDir game.pPosition d Pacman
                            || checkDir game.pPosition game.nextDir Pacman
                    then
                        if checkDir game.pPosition game.nextDir Pacman && game.nextDir /= d then
                            ( { game
                                | state = Running game.nextDir
                                , nextDir = None
                              }
                            , Cmd.none
                            , Audio.cmdNone
                            )

                        else
                            ( checkEatable
                                { game
                                    | pPosition =
                                        changeYPosition
                                            (game.pPosition.y + movement)
                                            game
                                    , state = Running d
                                    , pRotation = 90
                                }
                            , Cmd.none
                            , Audio.cmdNone
                            )

                    else
                        update DoNothing game

                _ ->
                    update DoNothing game

        -- Pressed play/pause button
        Types.GameModels.ChangeState ->
            case game.state of
                Running d ->
                    ( { game
                        | state = Stopped d
                        , message = gameMessages.pause
                      }
                    , Cmd.none
                    , Audio.loadAudio SoundLoaded "Assets/sounds/start_music.wav"
                    )

                Stopped d ->
                    ( { game
                        | state = Running d
                        , message = gameMessages.noText
                      }
                    , Cmd.none
                    , Audio.cmdNone
                    )

                -- Waiting before start the game
                Waiting ->
                    ( game, Cmd.none, Audio.cmdNone )

        -- Do nothing
        DoNothing ->
            ( game, Cmd.none, Audio.cmdNone )

        -- Change next direction
        ChangeDirection d ->
            ( { game | nextDir = d }, Cmd.none, Audio.cmdNone )

        -- When a fruit is shown
        Fruit ->
            if game.fruitSecondCounter == 10 then
                ( { game
                    | fruitAvailable = False
                    , fruitSecondCounter = 0
                  }
                , Cmd.none
                , Audio.cmdNone
                )

            else
                ( { game
                    | fruitSecondCounter = game.fruitSecondCounter + 1
                  }
                , Cmd.none
                , Audio.cmdNone
                )

        -- When the pill is active
        Pill ->
            if game.pillSecondCounter == 10 then
                ( { game
                    | pillActive = False
                    , pillSecondCounter = 0
                    , eatenGhostsCounter = 0
                    , redGhost = changeGhostSrc game.redGhost Red
                    , yellowGhost = changeGhostSrc game.yellowGhost Yellow
                    , blueGhost = changeGhostSrc game.blueGhost Blue
                    , pinkGhost = changeGhostSrc game.pinkGhost Pink
                  }
                , Cmd.none
                , Audio.cmdNone
                )

            else
                ( { game
                    | pillSecondCounter = game.pillSecondCounter + 1
                  }
                , Cmd.none
                , Audio.cmdNone
                )

        -- Counts the time in which nothing was eaten
        EatWaiter ->
            if game.eatItemSecondCounter == 0 then
                -- activate next ghost, if not all running
                if
                    not game.blueGhost.running
                        || not game.yellowGhost.running
                        || not game.pinkGhost.running
                then
                    ( { game
                        | nextGhostCanGoOut = True
                        , eatItemSecondCounter = itemSettings.noEatingCooldownMs
                      }
                    , Cmd.none
                    , Audio.cmdNone
                    )

                else
                    ( game, Cmd.none, Audio.cmdNone )

            else if itemSettings.noEatingCooldownMs - 1000 == game.eatItemSecondCounter then
                ( { game
                    | mouthMovement = False
                    , pacmanSrc = pacSettings.openedMouthSrc
                    , eatItemSecondCounter = game.eatItemSecondCounter - 1000
                  }
                , Cmd.none
                , Audio.cmdNone
                )

            else
                ( { game
                    | eatItemSecondCounter = game.eatItemSecondCounter - 1000
                  }
                , Cmd.none
                , Audio.cmdNone
                )

        -- Ghost movement
        GhostMove ghostName ->
            let
                -- Red ghost (Blinky)
                newRedGhost =
                    case ghostName of
                        Blinky ->
                            -- Change ghost source, if ghost not at start
                            -- position and go back in prison
                            if
                                game.redGhost.goBackInPrison
                                    && game.redGhost.position
                                    /= ghostSettings.startPosition
                            then
                                changeGhostSrc
                                    (moveGhost game.redGhost
                                        (getGhostNextDir game game.redGhost)
                                    )
                                    GoBackInPrison
                                -- Change ghost source, if ghost at start position
                                -- and go back in prison

                            else if
                                game.redGhost.goBackInPrison
                                    && game.redGhost.position
                                    == ghostSettings.startPosition
                            then
                                changeGhostSrc
                                    (moveGhoastToPosition
                                        (changeGoBackInPrison
                                            (setActiveState game.redGhost False)
                                            False
                                        )
                                        ghostSettings.pinkStartPos
                                    )
                                    Red

                            else
                                moveGhost game.redGhost (getGhostNextDir game game.redGhost)

                        _ ->
                            game.redGhost

                -- Pink ghost (Pinky)
                newPinkGhost =
                    case ghostName of
                        Pinky ->
                            -- Change ghost source, if ghost not at start
                            -- position and go back in prison
                            if
                                game.pinkGhost.goBackInPrison
                                    && game.pinkGhost.position
                                    /= ghostSettings.startPosition
                            then
                                changeGhostSrc
                                    (moveGhost game.pinkGhost
                                        (getGhostNextDir game game.pinkGhost)
                                    )
                                    GoBackInPrison
                                -- Change ghost source, if ghost at start position
                                -- and go back in prison

                            else if
                                game.pinkGhost.goBackInPrison
                                    && game.pinkGhost.position
                                    == ghostSettings.startPosition
                            then
                                changeGhostSrc
                                    (moveGhoastToPosition
                                        (changeGoBackInPrison
                                            (setActiveState game.pinkGhost False)
                                            False
                                        )
                                        ghostSettings.pinkStartPos
                                    )
                                    Pink

                            else
                                moveGhost game.pinkGhost (getGhostNextDir game game.pinkGhost)

                        _ ->
                            game.pinkGhost

                -- Blue ghost (Inky)
                newBlueGhost =
                    case ghostName of
                        Inky ->
                            -- Change ghost source, if ghost not at start
                            -- position and go back in prison
                            if
                                game.blueGhost.goBackInPrison
                                    && game.blueGhost.position
                                    /= ghostSettings.startPosition
                            then
                                changeGhostSrc
                                    (moveGhost game.blueGhost
                                        (getGhostNextDir game game.blueGhost)
                                    )
                                    GoBackInPrison
                                -- Change ghost source, if ghost at start
                                -- position and go back in prison

                            else if
                                game.blueGhost.goBackInPrison
                                    && game.blueGhost.position
                                    == ghostSettings.startPosition
                            then
                                changeGhostSrc
                                    (moveGhoastToPosition
                                        (changeGoBackInPrison
                                            (setActiveState game.blueGhost False)
                                            False
                                        )
                                        ghostSettings.blueStartPos
                                    )
                                    Blue

                            else
                                moveGhost game.blueGhost (getGhostNextDir game game.blueGhost)

                        _ ->
                            game.blueGhost

                -- Yellow ghost (Clyde)
                newYellowGhost =
                    case ghostName of
                        Clyde ->
                            -- Change ghost source, if ghost not at start position and go back in prison
                            if
                                game.yellowGhost.goBackInPrison
                                    && game.yellowGhost.position
                                    /= ghostSettings.startPosition
                            then
                                changeGhostSrc
                                    (moveGhost game.yellowGhost
                                        (getGhostNextDir game game.yellowGhost)
                                    )
                                    GoBackInPrison
                                -- Change ghost source, if ghost at start position and go back in prison

                            else if
                                game.yellowGhost.goBackInPrison
                                    && game.yellowGhost.position
                                    == ghostSettings.startPosition
                            then
                                changeGhostSrc
                                    (moveGhoastToPosition
                                        (changeGoBackInPrison
                                            (setActiveState game.yellowGhost False)
                                            False
                                        )
                                        ghostSettings.yellowStartPos
                                    )
                                    Yellow

                            else
                                moveGhost game.yellowGhost (getGhostNextDir game game.yellowGhost)

                        _ ->
                            game.yellowGhost
            in
            -- In the first round Pinky (pink) leaves the prison after one, Inky (blue)
            -- after 30 and Clyde (yellow) after 60 items. The counter is reset each time.
            -- After PacMan has been eaten once, Pinky (pink) leaves the prison after 7,
            -- Inky (blue) after 17 and Clyde (yellow) after 32 items. The counter is not
            -- reset like in the beginning.
            if
                checkGhoastEatingPacMan game.pPosition game.redGhost.position
                    && checkGhoastEatingPacMan game.pPosition game.blueGhost.position
                    && checkGhoastEatingPacMan game.pPosition game.yellowGhost.position
                    && checkGhoastEatingPacMan game.pPosition game.pinkGhost.position
                    && game.state
                    /= Stopped None
            then
                if game.level < 3 then
                    -- Activate Clyde (yellow) starts moving
                    if
                        ((game.itemCounter > 91 && game.lifes == 3)
                            && not game.yellowGhost.running
                        )
                            || ((game.itemCounter > 32 && game.lifes /= 3)
                                    && not game.yellowGhost.running
                               )
                            || (game.nextGhostCanGoOut
                                    && game.blueGhost.running
                                    && not game.yellowGhost.running
                               )
                    then
                        ( { game
                            | nextGhostCanGoOut = False
                            , redGhost = newRedGhost
                            , pinkGhost = newPinkGhost
                            , blueGhost = newBlueGhost
                            , yellowGhost = setGhostRunning newYellowGhost
                          }
                        , Cmd.none
                        , Audio.cmdNone
                        )
                        -- Activate Inky (blue) starts moving

                    else if
                        ((game.itemCounter > 31 && game.lifes == 3)
                            && not game.blueGhost.running
                        )
                            || ((game.itemCounter > 17 && game.lifes /= 3)
                                    && not game.blueGhost.running
                               )
                            || (game.nextGhostCanGoOut
                                    && game.pinkGhost.running
                                    && not game.blueGhost.running
                               )
                    then
                        ( { game
                            | nextGhostCanGoOut = False
                            , redGhost = newRedGhost
                            , pinkGhost = newPinkGhost
                            , blueGhost = setGhostRunning newBlueGhost
                          }
                        , Cmd.none
                        , Audio.cmdNone
                        )
                        -- Activate Pinky (pink) and start moving

                    else if
                        ((game.itemCounter > 1 && game.lifes == 3)
                            && not game.pinkGhost.running
                        )
                            || ((game.itemCounter > 7 && game.lifes /= 3)
                                    && not game.pinkGhost.running
                               )
                            || (game.nextGhostCanGoOut
                                    && game.redGhost.running
                                    && not game.pinkGhost.running
                               )
                    then
                        ( { game
                            | nextGhostCanGoOut = False
                            , redGhost = newRedGhost
                            , pinkGhost = setGhostRunning newPinkGhost
                          }
                        , Cmd.none
                        , Audio.cmdNone
                        )
                        -- No ghost can be activated then only move ghosts

                    else
                    -- Move activatedGhosts
                    if
                        game.redGhost.running
                            && game.blueGhost.running
                            && game.yellowGhost.running
                            && game.pinkGhost.running
                    then
                        ( { game
                            | redGhost = newRedGhost
                            , blueGhost = newBlueGhost
                            , yellowGhost = newYellowGhost
                            , pinkGhost = newPinkGhost
                          }
                        , Cmd.none
                        , Audio.cmdNone
                        )
                        -- Inky (blue) starts

                    else if
                        game.redGhost.running
                            && game.blueGhost.running
                            && game.pinkGhost.running
                    then
                        ( { game
                            | redGhost = newRedGhost
                            , blueGhost = newBlueGhost
                            , pinkGhost = newPinkGhost
                          }
                        , Cmd.none
                        , Audio.cmdNone
                        )
                        -- Pinky (pink) start

                    else if
                        game.redGhost.running
                            && game.pinkGhost.running
                    then
                        ( { game
                            | redGhost = newRedGhost
                            , pinkGhost = newPinkGhost
                          }
                        , Cmd.none
                        , Audio.cmdNone
                        )
                        -- Blinky (red) start

                    else
                        ( { game
                            | redGhost = newRedGhost
                          }
                        , Cmd.none
                        , Audio.cmdNone
                        )

                else
                    -- Acvtivate all ghosts and move all one step when level grather than 3
                    ( { game
                        | redGhost = setGhostRunning newRedGhost
                        , blueGhost = setGhostRunning newBlueGhost
                        , yellowGhost = setGhostRunning newYellowGhost
                        , pinkGhost = setGhostRunning newPinkGhost
                      }
                    , Cmd.none
                    , Audio.cmdNone
                    )

            else if not game.pillActive then
                case game.state of
                    Running d ->
                        if game.lifes == 0 then
                            ( { game
                                | state = Stopped d
                                , message = gameMessages.gameOver
                              }
                            , Cmd.none
                            , Audio.cmdNone
                            )

                        else
                            ( { game
                                | state = Stopped d
                                , lifes = game.lifes - 1
                              }
                            , Delay.after 3000 Millisecond (ResetGame NormalReset)
                            , Audio.cmdNone
                            )

                    -- PacMan loose animation
                    _ ->
                        ( game, Cmd.none, Audio.cmdNone )

            else
                let
                    ghostScore =
                        (game.eatenGhostsCounter + 1) * 200

                    newScore =
                        game.score + ghostScore
                in
                -- PacMan eat redGhost
                if
                    not (checkGhoastEatingPacMan game.pPosition game.redGhost.position)
                        && not game.redGhost.goBackInPrison
                then
                    ( { game
                        | redGhost = changeGoBackInPrison game.redGhost True
                        , eatenGhostsCounter = game.eatenGhostsCounter + 1
                        , score = newScore
                        , scoreMessage =
                            setScoreMsg game.redGhost.position
                                (String.fromInt ghostScore)
                        , showScoreMessage = True
                      }
                    , Cmd.none
                    , Audio.cmdNone
                    )
                    -- PacMan eat blueGhost

                else if
                    not (checkGhoastEatingPacMan game.pPosition game.blueGhost.position)
                        && not game.blueGhost.goBackInPrison
                then
                    ( { game
                        | blueGhost = changeGoBackInPrison game.blueGhost True
                        , eatenGhostsCounter = game.eatenGhostsCounter + 1
                        , score = newScore
                        , scoreMessage =
                            setScoreMsg game.blueGhost.position
                                (String.fromInt ghostScore)
                        , showScoreMessage = True
                      }
                    , Cmd.none
                    , Audio.cmdNone
                    )
                    -- PacMan eat yellowGhost

                else if
                    not (checkGhoastEatingPacMan game.pPosition game.yellowGhost.position)
                        && not game.yellowGhost.goBackInPrison
                then
                    ( { game
                        | yellowGhost = changeGoBackInPrison game.yellowGhost True
                        , eatenGhostsCounter = game.eatenGhostsCounter + 1
                        , score = newScore
                        , scoreMessage =
                            setScoreMsg game.yellowGhost.position
                                (String.fromInt ghostScore)
                        , showScoreMessage = True
                      }
                    , Cmd.none
                    , Audio.cmdNone
                    )
                    -- PacMan eat pinkGhost

                else if
                    not (checkGhoastEatingPacMan game.pPosition game.pinkGhost.position)
                        && not game.pinkGhost.goBackInPrison
                then
                    ( { game
                        | pinkGhost = changeGoBackInPrison game.pinkGhost True
                        , eatenGhostsCounter = game.eatenGhostsCounter + 1
                        , score = newScore
                        , scoreMessage =
                            setScoreMsg game.pinkGhost.position
                                (String.fromInt ghostScore)
                        , showScoreMessage = True
                      }
                    , Cmd.none
                    , Audio.cmdNone
                    )

                else
                    ( game, Cmd.none, Audio.cmdNone )

        -- Changes the color of a ghost
        ChangeColor ->
            ( { game
                | redGhost = huntedColorChange game.redGhost
                , yellowGhost = huntedColorChange game.yellowGhost
                , blueGhost = huntedColorChange game.blueGhost
                , pinkGhost = huntedColorChange game.pinkGhost
              }
            , Cmd.none
            , Audio.cmdNone
            )

        -- Resets the game to its default values
        ResetGame mode ->
            ( resetGame game.lifes game.score game.items game.pills game.itemCounter game.level mode
            , Delay.after 4500 Millisecond StartGame
            , Audio.cmdNone
            )

        -- PacMan wait to start
        StartGame ->
            ( { game | message = gameMessages.noText, state = Running Right }
            , Cmd.none
            , Audio.loadAudio SoundLoaded "Assets/sounds/start_music.wav"
            )

        -- When a sound file is loaded
        SoundLoaded x ->
            case x of
                Ok sound ->
                    ( game
                    , Task.perform (GetCurrentTime sound) Time.now
                    , Audio.cmdNone
                    )

                Err _ ->
                    ( { game | sound = LoadFailedModel }
                    , Cmd.none
                    , Audio.cmdNone
                    )

        -- Adds the LoadedModel the current posix time
        GetCurrentTime sound posix ->
            ( { game
                | sound =
                    LoadedModel
                        { sound = sound
                        , soundState = Playing posix
                        }
              }
            , Cmd.none
            , Audio.cmdNone
            )

        -- Changes the source of PacMan
        ChangePacmanSrc ->
            if game.pacmanSrc == pacSettings.openedMouthSrc then
                ( { game
                    | pacmanSrc = pacSettings.closedMouthSrc
                    , mouthMovement = False
                  }
                , Delay.after 200 Millisecond ChangePacmanSrc
                , Audio.cmdNone
                )

            else
                ( { game
                    | pacmanSrc = pacSettings.openedMouthSrc
                    , mouthMovement = False
                  }
                , Cmd.none
                , Audio.cmdNone
                )

        -- Clears the score message
        ClearScoreMsg ->
            ( { game
                | scoreMessage = setScoreMsg { x = 0, y = 0 } ""
                , showScoreMessage = False
              }
            , Cmd.none
            , Audio.cmdNone
            )



----------
-- VIEW --
----------


view : Game -> Html Msg
view game =
    node "main"
        []
        [ node "style" [] [ text styleContents ]
        , div (class "wrapper" :: wrapperCss)
            [ div (class "headline" :: headlineCss)
                [ div (textCss ++ [ Html.Attributes.style "text-transform" "uppercase" ])
                    [ Html.text ("Lvl " ++ String.fromInt game.level) ]
                , div textCss [ Html.text (String.fromInt game.score) ]
                , div textCss [ Html.text "500x500" ]
                ]
            , div
                gameCss
                [ svg
                    (gameChildCss
                        ++ [ id "gameField" ]
                    )
                    (pointsToSvg game.items 1
                        ++ pointsToSvg game.pills 2
                        ++ createFruit game.fruitAvailable game.level
                        ++ [ path [ fill fieldSettings.borderColor, d "M94,70.7H43.7c-2.8,0-5-2.3-5-5V42.3c0-2.8,2.3-5,5-5H94c2.8,0,5,2.3,5,5v23.3C99,68.4,96.8,70.7,94,70.7z" ] []
                           , path [ fill fieldSettings.borderColor, d "M200.3,70.7h-66c-2.8,0-5-2.3-5-5V42.3c0-2.8,2.2-5,5-5h66c2.8,0,5,2.3,5,5v23.3 C205.3,68.4,203.1,70.7,200.3,70.7z" ] []
                           , path [ fill fieldSettings.borderColor, d "M366.1,71.6h-67.5c-3,0-5.3-2.3-5.3-5V43.2c0-2.8,2.5-5,5.3-5h67.5c3,0,5.3,2.3,5.3,5v23.3 C371.5,69.3,369.1,71.6,366.1,71.6z" ] []
                           , path [ fill fieldSettings.borderColor, d "M94.6,131.6H42.4c-2.8,0-5-2.6-5-5.6v-21.3c0-3.2,2.3-5.6,5-5.6h52.3c2.8,0,5,2.6,5,5.6v21.3 C99.6,129.1,97.5,131.6,94.6,131.6z" ] []
                           , path [ fill fieldSettings.borderColor, d "M457,70.1h-52.5c-3,0-5.3-2.3-5.3-5V42.7c0-2.8,2.5-5,5.3-5H457c3,0,5.3,2.3,5.3,5V65 C462.3,67.8,460,70.1,457,70.1z" ] []
                           , path [ fill fieldSettings.borderColor, d "M199.7,158.3h-35.4c-2.4,0-4.3-2.2-4.3-5v-49.8c0-2.8-2-5-4.3-5h-22.1c-2.4,0-4.3,2.3-4.3,5v112.8 c0,2.8,2,5,4.3,5h22.1c2.4,0,4.3-2.3,4.3-5v-34.7c0-2.8,1.9-5,4.3-5h35.4c2.4,0,4.3-2.3,4.3-5v-8.3 C204,160.6,202.1,158.3,199.7,158.3z" ] []
                           , path [ fill fieldSettings.borderColor, d "M305.8,98.6H193.2c-2.8,0-5,2.3-5,5v22.7c0,2.8,2.3,5,5,5h36.3c2.8,0,5,2.2,5,5v35.3c0,2.8,2.3,5,5,5h18.3 c2.8,0,5-2.3,5-5v-35.3c0-2.8,2.2-5,5-5h38c2.8,0,5-2.3,5-5v-22.7C310.8,100.8,308.6,98.6,305.8,98.6z" ] []
                           , path [ fill fieldSettings.borderColor, d "M298.2,175.7h36.4c2.4,0,4.2,2.2,4.2,5v33.7c0,2.8,1.9,5,4.2,5h24.8c2.4,0,4.2-2.3,4.2-5l-0.9-111.9 c0-2.8-1.9-5-4.2-5h-23.8c-2.4,0-4.2,2.3-4.2,5l-0.1,49.9c0,2.8-1.8,5-4.2,5h-36.4c-2.4,0-4.2,2.3-4.2,5v8.3 C294,173.4,295.9,175.7,298.2,175.7z" ] []
                           , path [ fill fieldSettings.borderColor, d "M457,131.7h-50.5c-3,0-5.3-2.3-5.3-5v-22.4c0-2.8,2.5-5,5.3-5H457c3,0,5.3,2.3,5.3,5v22.3 C462.3,129.4,460,131.7,457,131.7z" ] []
                           , path [ fill fieldSettings.borderColor, d "M155,311.7h-21c-2.8,0-5-2.3-5-5V255c0-2.8,2.3-5,5-5h21c2.8,0,5,2.3,5,5v51.7 C160,309.4,157.8,311.7,155,311.7z" ] []
                           , path [ fill fieldSettings.borderColor, d "M307.3,294H193.7c-2.8,0-5,2.3-5,5v7.7c0,2.8,2.3,5,5,5H228c2.8,0,5,2.2,5,5V351c0,2.8,2.3,5,5,5h22.3 c2.8,0,5-2.3,5-5v-34.3c0-2.8,2.2-5,5-5h37c2.8,0,5-2.3,5-5V299C312.3,296.2,310.1,294,307.3,294z" ] []
                           , path [ fill fieldSettings.borderColor, d "M366,312.7h-22c-2.8,0-5-2.3-5-5V255c0-2.8,2.3-5,5-5h22c2.8,0,5,2.3,6,5v52.7   C371,310.4,368.8,312.7,366,312.7z" ] []
                           , path [ fill fieldSettings.borderColor, d "M94,340.3H81.3h-1H43c-2.8,0-5,2.2-5,5v7.3c0,2.8,2.3,5,5,5h21.3c2.8,0,5,2.2,5,5V411c0,2.8,2.3,5,5,5H94 c2.8,0,5-2.3,5-5v-65.7C99,342.6,96.8,340.3,94,340.3z" ] []
                           , path [ fill fieldSettings.borderColor, d "M202.3,356.7h-69c-2.8,0-5-2.3-5-5v-8.3c0-2.8,2.2-5,5-5h69c2.8,0,5,2.3,5,5v8.3   C207.3,354.4,205.1,356.7,202.3,356.7z" ] []
                           , path [ fill fieldSettings.borderColor, d "M368,355.7h-69c-2.8,0-5-2.3-5-5v-7.3c0-2.8,2.2-5,5-5h69c2.8,0,5,2.3,5,5v7.3   C373,353.4,370.8,355.7,368,355.7z" ] []
                           , path [ fill fieldSettings.borderColor, d "M404.3,338.3h4.7h5h42.3c2.8,0,5,2.2,5,5v7.3c0,2.8-2.3,5-5,5H435c-2.8,0-5,2.2-5,5V412c0,2.8-2.3,5-5,5h-20.7 c-2.8,0-5-2.3-5-5v-68.7C399.3,340.6,401.6,338.3,404.3,338.3z" ] []
                           , path [ fill fieldSettings.borderColor, d "M202,442.7h-37c-2.8,0-5-2.2-5-5v-50.3c0-2.8-2.3-5-5-5h-20.9c-2.8,0-5,2.3-5,5v50.3c0,2.8-2.2,5-5,5H44 c-2.8,0-5,2.2-5,5v9.3c0,2.8,2.3,5,5,5H202c2.8,0,5-2.3,5-5v-9.3C207,444.9,204.8,442.7,202,442.7z" ] []
                           , path [ fill fieldSettings.borderColor, d "M298.9,443.7h35.5c2.7,0,4.9-2.2,4.9-5v-52.3c0-2.8,2.3-5,4.9-5H366c2.7,0,4.9,2.3,4.9,5v52.3 c0,2.8,2.2,5,4.9,5h81.5c2.7,0,4.9,2.2,4.9,5v8.3c0,2.8-2.3,5-4.9,5H298.9c-2.7,0-4.9-2.3-4.9-5v-8.3 C294,445.9,296.3,443.7,298.9,443.7z" ] []
                           , path [ fill fieldSettings.borderColor, d "M304.3,383.3H195.7c-2.8,0-5,2.3-5,5V411c0,2.8,2.3,5,5,5H229c2.8,0,5,3.2,5,6v35.3c0,2.8,2.3,5,5,5h21.3 c2.8,0,5-2.3,5-5V422c0-2.8,2.2-6,5-6h34c2.8,0,5-2.3,5-5v-22.7C309.3,385.6,307.1,383.3,304.3,383.3z" ] []

                           -- Prison barrier
                           , line [ x1 "234.5", y1 "205.5", x2 "263.5", y2 "205.5", stroke "#FFBA00", strokeWidth "2px" ] []

                           -- Outer lines and inner cave
                           , path [ fill fieldSettings.borderColor, d "M403.7,211.2v-43.8c0-2.9,2.2-5.1,5-5.1h84.9c1.5,0,2.7-1.2,2.7-2.7V7.9c0-1.5-1.2-2.7-2.7-2.7H6.8 c-1.5,0-2.7,1.2-2.7,2.7v152.2c0,1.5,1.2,2.7,2.7,2.7h82.8c2.8,0,5,2.2,5,5.1v41.8c0,2.9-2.2,6.1-5,6.1H0v5.5h97.3 c1.5,0,2.7-1.2,2.7-2.7v-58.3c0-1.5-1.2-2.7-2.7-2.7H16.5c-2.8,0-5-2.2-5-5.1V15.9c0-2.9,2.2-5.1,5-5.1h211c2.8,0,5,2.2,5,5.1v51 c0,2.9,2.2,5.1,5,5.1H261c2.8,0,5-2.2,5-5.1v-51c0-2.9,2.2-5.1,5-5.1h212.9c2.8,0,5,2.2,5,5.1v136c0,2.9-2.2,5.1-5,5.1H401 c-1.5,0-2.7,1.2-2.7,2.7v59.1c0,0.1,0,0.1,0,0.2c0,0.1,0,0.1,0,0.2c0,1.5,1.2,2.7,2.7,2.7h99v-5.5h-91.3 C405.9,216.3,403.7,214.1,403.7,211.2z" ] []
                           , polygon [ fill fieldSettings.borderColor, points "306.9,203.5 263.5,203.5 263.5,208.3 306.9,208.3 306.9,261.9 192.4,261.9 192.4,208.3 234.5,208.3 234.5,203.5 192.4,203.5 188.4,203.5 188.4,208.3 188.4,261.9 188.4,266.7 192.4,266.7 306.9,266.7 311,266.7 311,261.9 311,208.3 311,203.5" ] []
                           , path [ fill fieldSettings.borderColor, d "M406.4,254.4H500V249h-99.5c-1.2,0-2.2,1.2-2.2,2.7l0,0l0,0v56.3c0,1.5,1.2,2.7,2.7,2.7h85.4 c1.5,0,2.7,1.2,2.7,2.7v65.9c0,2.9-2.2,5.1-5,5.1h-19.9c-2.8,0-5,2.2-5,5.1v20.8c0,2.9,2.2,5.1,5,5.1h19.9c2.8,0,5,2.2,5,5.1v66.4 c0,1.5-1.2,2.7-2.7,2.7H14.1c-1.5,0-2.7-1.2-2.7-2.7v-66.5c0-2.9,2.2-4.1,5-4.1h20.1c2.8,0,5-2.2,5-5.1v-21.8c0-2.9-2.2-5.1-5-5.1 H16.4c-2.8,0-5-2.2-5-5.1V314c0-1.5,1.2-2.7,2.7-2.7h83.2c0.1,0,0.2,0,0.3,0c0.1,0,0.2,0,0.3,0c1.5,0,2.7-1.2,2.7-2.7v-57 c0-1.5-1.2-2.7-2.7-2.7H0v5.5h92.6c1.5,0,2.7,1.2,2.7,2.7v46c0,1.5-1.2,2.7-2.7,2.7H6.7c-1.5,0-2.7,1.2-2.7,2.7v183.9 c0,1.5,1.2,2.7,2.7,2.7h487.1c1.5,0,2.7-1.2,2.7-2.7V308c0-1.5-1.2-2.7-2.7-2.7h-87.4c-1.5,0-2.7-1.2-2.7-2.7V257 C403.7,255.6,404.9,254.4,406.4,254.4z" ] []
                           ]
                    )
                , div
                    (gameChildCss
                        ++ [ id "pacmanArea" ]
                    )
                    [ img
                        (pacmanSvgCss
                            ++ [ src game.pacmanSrc
                               , Html.Attributes.style "top" (String.fromInt (game.pPosition.y - round (toFloat pacSettings.ratio / 2)) ++ "px")
                               , Html.Attributes.style "left" (String.fromInt (game.pPosition.x - round (toFloat pacSettings.ratio / 2)) ++ "px")
                               , Html.Attributes.style "transform" ("rotate(" ++ String.fromInt game.pRotation ++ "deg)")
                               ]
                        )
                        []
                    ]
                , div
                    (gameChildCss
                        ++ [ id "ghostArea" ]
                    )
                    [ img
                        (ghostSvgCss
                            ++ [ src ("Assets/img/ghosts/" ++ game.redGhost.src ++ ".svg")
                               , Html.Attributes.style "top" (String.fromInt (game.redGhost.position.y - round (toFloat ghostSettings.ratio / 2)) ++ "px")
                               , Html.Attributes.style "left" (String.fromInt (game.redGhost.position.x - round (toFloat ghostSettings.ratio / 2)) ++ "px")
                               ]
                        )
                        []
                    , img
                        (ghostSvgCss
                            ++ [ src ("Assets/img/ghosts/" ++ game.pinkGhost.src ++ ".svg")
                               , Html.Attributes.style "top" (String.fromInt (game.pinkGhost.position.y - round (toFloat ghostSettings.ratio / 2)) ++ "px")
                               , Html.Attributes.style "left" (String.fromInt (game.pinkGhost.position.x - round (toFloat ghostSettings.ratio / 2)) ++ "px")
                               ]
                        )
                        []
                    , img
                        (ghostSvgCss
                            ++ [ src ("Assets/img/ghosts/" ++ game.blueGhost.src ++ ".svg")
                               , Html.Attributes.style "top" (String.fromInt (game.blueGhost.position.y - round (toFloat ghostSettings.ratio / 2)) ++ "px")
                               , Html.Attributes.style "left" (String.fromInt (game.blueGhost.position.x - round (toFloat ghostSettings.ratio / 2)) ++ "px")
                               ]
                        )
                        []
                    , img
                        (ghostSvgCss
                            ++ [ src ("Assets/img/ghosts/" ++ game.yellowGhost.src ++ ".svg")
                               , Html.Attributes.style "top" (String.fromInt (game.yellowGhost.position.y - round (toFloat ghostSettings.ratio / 2)) ++ "px")
                               , Html.Attributes.style "left" (String.fromInt (game.yellowGhost.position.x - round (toFloat ghostSettings.ratio / 2)) ++ "px")
                               ]
                        )
                        []
                    ]
                , div
                    (gameChildCss
                        ++ [ id "otherArea" ]
                    )
                    [ div (textCss ++ messageCss) [ Html.text game.message ]
                    , addMsgText game.scoreMessage.point game.scoreMessage.msg
                    ]
                ]
            , div (class "headline" :: headlineCss)
                [ div (textCss ++ [ Html.Attributes.style "text-transform" "uppercase" ])
                    [ Html.text "Leben:" ]
                , div textCss (pacManSvgList [] game.lifes)
                , div (textCss ++ [ Html.Attributes.style "text-transform" "uppercase" ])
                    [ Html.text "Früchte:" ]
                , div textCss (fruitSvgList [] 1 game.level)
                ]
            ]
        ]



-------------------
-- Subscriptions --
-------------------


subscriptions : Game -> Sub Msg
subscriptions game =
    Sub.batch
        [ Browser.Events.onKeyDown keyDecoder
        , case game.state of
            -- Normal running
            Running d ->
                Time.every 20 (\_ -> MoveDirection d)

            -- Start case (waiting till pacMan can run)
            Waiting ->
                Time.every 20 (\_ -> ResetGame NormalReset)

            _ ->
                Sub.none

        -- Timer for fruit, when its availeble (only if game is running)
        , case game.state of
            Running _ ->
                if game.fruitAvailable then
                    Time.every 1000 (\_ -> Fruit)

                else
                    Sub.none

            _ ->
                Sub.none

        -- Timer for pill, when its active (only if game is running)
        , case game.state of
            Running _ ->
                if game.pillActive then
                    Time.every 1000 (\_ -> Pill)

                else
                    Sub.none

            _ ->
                Sub.none

        -- Change ghost color, when pill active for the last 3 seconds (only if game is running)
        , case game.state of
            Running _ ->
                if game.pillActive && game.pillSecondCounter > 7 then
                    Time.every 250 (\_ -> ChangeColor)

                else
                    Sub.none

            _ ->
                Sub.none

        -- Reset game
        , if game.totalItemCount == game.itemCounter then
            Time.every 20 (\_ -> ResetGame NewLevel)

          else
            Sub.none

        -- If pacMan eat Item start Counter, because if pacman eats 4seconds
        -- nothning a ghost comes out of prison (only if game is running)
        , case game.state of
            Running _ ->
                if game.eatItem then
                    Time.every 1000 (\_ -> EatWaiter)

                else
                    Sub.none

            _ ->
                Sub.none

        -- If pacman move mouth
        , case game.state of
            Running _ ->
                if game.mouthMovement then
                    Time.every 200 (\_ -> ChangePacmanSrc)

                else
                    Sub.none

            _ ->
                Sub.none

        -- Red ghost movement (only if game is running)
        , case game.state of
            Running _ ->
                if game.redGhost.goBackInPrison then
                    Time.every 10 (\_ -> GhostMove Blinky)

                else if game.pillActive then
                    Time.every 30 (\_ -> GhostMove Blinky)

                else
                    Time.every 20 (\_ -> GhostMove Blinky)

            _ ->
                Sub.none

        -- Pink ghost movement (only if game is running)
        , case game.state of
            Running _ ->
                if game.pinkGhost.goBackInPrison then
                    Time.every 10 (\_ -> GhostMove Pinky)

                else if game.pillActive then
                    Time.every 30 (\_ -> GhostMove Pinky)

                else
                    Time.every 20 (\_ -> GhostMove Pinky)

            _ ->
                Sub.none

        -- Blue ghost movement (only if game is running)
        , case game.state of
            Running _ ->
                if game.blueGhost.goBackInPrison then
                    Time.every 10 (\_ -> GhostMove Inky)

                else if game.pillActive then
                    Time.every 30 (\_ -> GhostMove Inky)

                else
                    Time.every 20 (\_ -> GhostMove Inky)

            _ ->
                Sub.none

        -- Yellow ghost movement (only if game is running)
        , case game.state of
            Running _ ->
                if game.yellowGhost.goBackInPrison then
                    Time.every 10 (\_ -> GhostMove Clyde)

                else if game.pillActive then
                    Time.every 30 (\_ -> GhostMove Clyde)

                else
                    Time.every 20 (\_ -> GhostMove Clyde)

            _ ->
                Sub.none

        -- Clear score msg after 1500 ms (only if game is running)
        , case game.state of
            Running _ ->
                if game.showScoreMessage then
                    Time.every 1500 (\_ -> ClearScoreMsg)

                else
                    Sub.none

            _ ->
                Sub.none
        ]



-------------------
-- MAIN PROGRAMM --
-------------------


port audioPortToJS : Value -> Cmd msg


port audioPortFromJS : (Value -> msg) -> Sub msg



-- Initialize model


init : flags -> ( Game, Cmd Msg, Audio.AudioCmd Msg )
init _ =
    ( initialModel
    , Cmd.none
    , Audio.loadAudio SoundLoaded
        "Assets/sounds/start_music.wav"
    )



-- Main programm


main : Program () (Audio.Model Msg Game) (Audio.Msg Msg)
main =
    Audio.elementWithAudio
        { init = init
        , subscriptions = subscriptions
        , view = view
        , update = update
        , audio = gameToAudio
        , audioPort = { toJS = audioPortToJS, fromJS = audioPortFromJS }
        }



-- Changes the status when playing a sound file and returns the audio


gameToAudio : Game -> Audio.Audio
gameToAudio game =
    case game.sound of
        LoadedModel x ->
            case x.soundState of
                NotPlaying ->
                    Audio.silence

                Playing time ->
                    Audio.audio x.sound time

                _ ->
                    Audio.silence

        _ ->
            Audio.silence



---------------
-- FUNCTIONS --
---------------
-------------------
-- Key functions --
-------------------


keyDecoder : Decoder Msg
keyDecoder =
    Json.Decode.map toKey (Json.Decode.field "key" Json.Decode.string)


toKey : String -> Msg
toKey string =
    case string of
        "ArrowUp" ->
            ChangeDirection Up

        "ArrowDown" ->
            ChangeDirection Down

        "ArrowLeft" ->
            ChangeDirection Left

        "ArrowRight" ->
            ChangeDirection Right

        "Escape" ->
            Types.GameModels.ChangeState

        _ ->
            Types.GameModels.DoNothing



----------------------------
-- Change PacMan position --
----------------------------
-- Changes the x-position of PacMan


changeXPosition : Int -> Game -> Point
changeXPosition value game =
    let
        oldPosition =
            game.pPosition
    in
    { oldPosition | x = value }



-- Changes the y-position of PacMan


changeYPosition : Int -> Game -> Point
changeYPosition value game =
    let
        oldPosition =
            game.pPosition
    in
    { oldPosition | y = value }



----------------------
-- Substract lists  --
----------------------
-- Subtracts two lists from each other to avoid double entries within one list


substractList : List Point -> List Point -> List Point
substractList a b =
    List.filter (\x -> not (List.member x a)) b



----------------------
-- getFullItemList  --
----------------------
-- Returns a list of all items distributed on the playing field


getFullItemList : List Point
getFullItemList =
    substractList pillsList
        (filterDuplicates
            (List.foldl createPoints
                []
                (Dict.values runMesh)
            )
        )



-------------------------
-- Reset game function --
-------------------------
-- Resets the whole game and sets passed values as initial values depending on the desired mode


resetGame : Int -> Int -> List Point -> List Point -> Int -> Int -> StartMode -> Game
resetGame newLife newScore prevItemList prevPillsList prevItemCounter prevLevel mode =
    let
        newState =
            case mode of
                Init ->
                    Waiting

                _ ->
                    Stopped None

        newItemList =
            case mode of
                NormalReset ->
                    prevItemList

                _ ->
                    getFullItemList

        newPillsList =
            case mode of
                NormalReset ->
                    prevPillsList

                _ ->
                    pillsList

        newLevel =
            case mode of
                NewLevel ->
                    prevLevel + 1

                _ ->
                    prevLevel

        newItemCounter =
            case mode of
                NormalReset ->
                    prevItemCounter

                _ ->
                    0
    in
    { pPosition = pacSettings.startPosition
    , pacmanSrc = pacSettings.openedMouthSrc
    , mouthMovement = True
    , state = newState
    , nextDir = Right
    , pRotation = 0
    , lifes = newLife
    , score = newScore
    , items = newItemList
    , totalItemCount = length getFullItemList
    , message = gameMessages.ready
    , pills = newPillsList
    , itemCounter = newItemCounter
    , fruitSecondCounter = 0
    , fruitAvailable = False
    , redGhost =
        { name = Blinky
        , color = Red
        , position = ghostSettings.startPosition
        , dir = None
        , active = True
        , offset = 0
        , src = getGhostSrc Red Right
        , goBackInPrison = False
        , running = True
        }
    , pinkGhost =
        { name = Pinky
        , color = Pink
        , position = ghostSettings.pinkStartPos
        , dir = Up
        , active = False
        , offset = 4
        , src = getGhostSrc Pink Up
        , goBackInPrison = False
        , running = False
        }
    , blueGhost =
        { name = Inky
        , color = Blue
        , position = ghostSettings.blueStartPos
        , dir = Right
        , active = False
        , offset = 2
        , src = getGhostSrc Blue Up
        , goBackInPrison = False
        , running = False
        }
    , yellowGhost =
        { name = Clyde
        , color = Yellow
        , position = ghostSettings.yellowStartPos
        , dir = Left
        , active = False
        , offset = 0
        , src = getGhostSrc Yellow Up
        , goBackInPrison = False
        , running = False
        }
    , pillActive = False
    , pillSecondCounter = 0
    , sound = LoadingModel
    , eatenGhostsCounter = 0
    , level = newLevel
    , eatItem = True
    , eatItemSecondCounter = 4
    , nextGhostCanGoOut = False
    , scoreMessage =
        { point =
            { x = 0
            , y = 0
            }
        , msg = ""
        }
    , showScoreMessage = False
    }



-------------------------
-- PacMan life display --
-------------------------
-- Returns a list of SVG depending on the lives still available


pacManSvgList : List (Svg Msg) -> Int -> List (Svg Msg)
pacManSvgList list amount =
    if amount > 0 then
        pacManSvgList
            (img
                [ src initialModel.pacmanSrc
                , width pacSettings.ratio
                , height pacSettings.ratio
                ]
                []
                :: list
            )
            (amount - 1)

    else
        list



-------------------------
----- Fruit display -----
-------------------------
-- Returns a list of SVG depending on the current level


fruitSvgList : List (Svg Msg) -> Int -> Int -> List (Svg Msg)
fruitSvgList list counter level =
    if counter <= level then
        if counter == 1 then
            fruitSvgList
                (img
                    [ src "Assets/img/fruits/cherry.svg"
                    , width fruitSettings.ratio
                    , height fruitSettings.ratio
                    ]
                    []
                    :: list
                )
                (counter + 1)
                level

        else if counter == 2 then
            fruitSvgList
                (img
                    [ src "Assets/img/fruits/strawberry.svg"
                    , width fruitSettings.ratio
                    , height fruitSettings.ratio
                    ]
                    []
                    :: list
                )
                (counter + 1)
                level

        else if counter == 3 then
            fruitSvgList
                (img
                    [ src "Assets/img/fruits/orange.svg"
                    , width fruitSettings.ratio
                    , height fruitSettings.ratio
                    ]
                    []
                    :: list
                )
                (counter + 2)
                level

        else if counter == 5 then
            fruitSvgList
                (img
                    [ src "Assets/img/fruits/apple.svg"
                    , width fruitSettings.ratio
                    , height fruitSettings.ratio
                    ]
                    []
                    :: list
                )
                (counter + 2)
                level

        else if counter == 7 then
            fruitSvgList
                (img
                    [ src "Assets/img/fruits/grape.svg"
                    , width fruitSettings.ratio
                    , height fruitSettings.ratio
                    ]
                    []
                    :: list
                )
                (counter + 2)
                level

        else if counter == 9 then
            fruitSvgList
                (img
                    [ src "Assets/img/fruits/spaceship.svg"
                    , width fruitSettings.ratio
                    , height fruitSettings.ratio
                    ]
                    []
                    :: list
                )
                (counter + 2)
                level

        else if counter == 11 then
            fruitSvgList
                (img
                    [ src "Assets/img/fruits/bell.svg"
                    , width fruitSettings.ratio
                    , height fruitSettings.ratio
                    ]
                    []
                    :: list
                )
                (counter + 2)
                level

        else
            fruitSvgList
                (img
                    [ src "Assets/img/fruits/key.svg"
                    , width fruitSettings.ratio
                    , height fruitSettings.ratio
                    ]
                    []
                    :: list
                )
                (level + 1)
                level

    else
        list
