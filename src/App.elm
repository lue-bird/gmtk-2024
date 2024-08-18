module App exposing (app)

{-| paste in example code and fiddle around
-}

import Color
import Duration exposing (Duration)
import Json.Decode
import Length
import Point2d exposing (Point2d)
import Quantity
import Random.Pcg.Extended
import Set exposing (Set)
import Svg.LocalExtra
import Time
import Vector2d exposing (Vector2d)
import Web
import Web.Dom
import Web.Random
import Web.Svg
import Web.Time
import Web.Window


type alias State =
    { randomness :
        Maybe
            { initial : ( Int, List Int )
            , seed : Random.Pcg.Extended.Seed
            }
    , windowSize : { height : Int, width : Int }
    , lastSimulationTime : Maybe Time.Posix
    , playerLocation : Point2d Length.Meters Float
    , playerVelocity : Vector2d (Quantity.Rate Length.Meters Duration.Seconds) Float
    }


app =
    { initialState =
        { windowSize = { width = 1920, height = 1080 }
        , randomness = Nothing
        , lastSimulationTime = Nothing
        , playerLocation = Point2d.fromMeters { x = 0, y = 0 }
        , playerVelocity =
            Vector2d.fromMeters { x = 2, y = 20 }
                |> Vector2d.per Duration.second
        }
    , interface = interface
    }


interface : State -> Web.Interface State
interface =
    \state ->
        [ [ Web.Window.sizeRequest, Web.Window.resizeListen ]
            |> Web.interfaceBatch
            |> Web.interfaceFutureMap (\size -> { state | windowSize = size })
        , Web.Random.unsignedInt32s 4
            |> Web.interfaceFutureMap
                (\initialRandomness ->
                    case initialRandomness of
                        [] ->
                            state

                        initialRandomnessInt0 :: initialRandomnessInt1Up ->
                            state |> stateWithInitialRandomness ( initialRandomnessInt0, initialRandomnessInt1Up )
                )
        , let
            worldSize : { width : Float, height : Float }
            worldSize =
                let
                    ratioWidthToHeight : Float
                    ratioWidthToHeight =
                        worldSizeCells.x / worldSizeCells.y
                in
                if (state.windowSize.width |> Basics.toFloat) < (state.windowSize.height |> Basics.toFloat) * ratioWidthToHeight then
                    -- disproportional in height
                    { width = state.windowSize.width |> Basics.toFloat
                    , height = (state.windowSize.width |> Basics.toFloat) / ratioWidthToHeight
                    }

                else
                    -- might be disproportional in width
                    { width = (state.windowSize.height |> Basics.toFloat) * ratioWidthToHeight
                    , height = state.windowSize.height |> Basics.toFloat
                    }

            worldUi : Web.Dom.Node state_
            worldUi =
                Web.Svg.element "rect"
                    [ Svg.LocalExtra.fillUniform (Color.rgb 0.03 0.02 0)
                    , Web.Dom.attribute "width" "100%"
                    , Web.Dom.attribute "height" "100%"
                    ]
                    []

            playerUi : Web.Dom.Node state_
            playerUi =
                Svg.LocalExtra.circle
                    { position = state.playerLocation |> Point2d.toRecord Length.inMeters
                    , radius = 1
                    }
                    [ Svg.LocalExtra.fillUniform (Color.rgb 1 0.5 0)
                    ]
          in
          Web.Dom.element "div"
            [ Web.Dom.style "background-color" (Color.rgb 0.05 0.05 0.05 |> Color.toCssString)
            , Web.Dom.style "position" "fixed"
            , Web.Dom.style "top" "0"
            , Web.Dom.style "right" "0"
            , Web.Dom.style "bottom" "0"
            , Web.Dom.style "left" "0"
            ]
            [ Web.Dom.text
                (Debug.toString
                    { playerVelocityPerSecond =
                        state.playerVelocity |> Vector2d.for Duration.second |> Vector2d.toTuple Length.inMeters
                    , playerLocation =
                        state.playerLocation |> Point2d.toTuple Length.inMeters
                    }
                )
            , Web.Svg.element "svg"
                [ Web.Dom.attribute "viewBox" ([ "0 0 ", worldSize.width |> String.fromFloat, " ", worldSize.height |> String.fromFloat ] |> String.concat)
                , Web.Dom.attribute "width" ((worldSize.width |> String.fromFloat) ++ "px")
                , Web.Dom.attribute "height" ((worldSize.height |> String.fromFloat) ++ "px")
                , Web.Dom.style "display" "block"
                , Web.Dom.style "margin" "auto"
                ]
                [ worldUi
                , Web.Svg.element "g"
                    [ Svg.LocalExtra.scaled
                        { x = worldSize.width / worldSizeCells.x
                        , y = -(worldSize.width / worldSizeCells.x)
                        }
                    ]
                    [ Web.Svg.element "g"
                        [ Svg.LocalExtra.translated
                            { x = worldSizeCells.x / 2
                            , y = -worldSizeCells.y / 2
                            }
                        ]
                        [ playerUi ]
                    ]
                ]
            ]
            |> Web.Dom.render
        , Web.Time.periodicallyListen (Duration.milliseconds 16)
            |> Web.interfaceFutureMap
                (\newTime ->
                    let
                        durationToSimulate : Duration
                        durationToSimulate =
                            case state.lastSimulationTime of
                                Nothing ->
                                    Duration.seconds 0

                                Just lastSimulationTime ->
                                    Duration.from lastSimulationTime newTime

                        newPlayerLocation : Point2d Length.Meters Float
                        newPlayerLocation =
                            state.playerLocation
                                |> Point2d.translateBy
                                    (state.playerVelocity |> Vector2d.for durationToSimulate)

                        newPlayerVelocity : Vector2d (Quantity.Rate Length.Meters Duration.Seconds) Float
                        newPlayerVelocity =
                            state.playerVelocity
                                |> Vector2d.plus
                                    (gravity |> Vector2d.for durationToSimulate)
                    in
                    { state
                        | lastSimulationTime = Just newTime
                        , playerLocation = newPlayerLocation
                        , playerVelocity = newPlayerVelocity
                    }
                )
        , Web.Window.listenTo "keydown"
            |> Web.interfaceFutureMap
                (\event ->
                    let
                        keyResult =
                            event
                                |> Json.Decode.decodeValue
                                    (Json.Decode.field "key" Json.Decode.string)
                    in
                    case keyResult of
                        Err _ ->
                            state

                        Ok key ->
                            if releaseActionKeyboardKeys |> Set.member key then
                                state

                            else
                                --TODO release
                                state
                )
        ]
            |> Web.interfaceBatch


gravity =
    -- would be cool to make dynamic based on nearby rocks!
    Vector2d.fromMeters { x = 0, y = -20 }
        |> Vector2d.per Duration.second
        |> Vector2d.per Duration.second


releaseActionKeyboardKeys : Set String
releaseActionKeyboardKeys =
    Set.fromList
        [ "w"
        , "ArrowUp"
        , " "
        , "Enter"
        ]


worldSizeCells : { x : Float, y : Float }
worldSizeCells =
    { x = 80, y = 45 }


stateWithInitialRandomness : ( Int, List Int ) -> (State -> State)
stateWithInitialRandomness ( initialRandomnessInt0, initialRandomnessInt1Up ) =
    \state ->
        let
            initialSeed : Random.Pcg.Extended.Seed
            initialSeed =
                Random.Pcg.Extended.initialSeed initialRandomnessInt0 initialRandomnessInt1Up

            ( generatedDockShapeCompositions, newSeed ) =
                ( (), initialSeed )

            -- Random.Pcg.Extended.step
            --     (dockShapeGeneratorWithSubCount 60)
            --     initialSeed
        in
        { state
            | randomness =
                { initial = ( initialRandomnessInt0, initialRandomnessInt1Up )
                , seed = newSeed
                }
                    |> Just

            --, dockShapeCompositions = generatedDockShapeCompositions
        }
