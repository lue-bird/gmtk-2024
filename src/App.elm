module App exposing (app)

import Angle
import Color exposing (Color)
import Direction2d
import Duration exposing (Duration)
import Json.Decode
import Length
import LineSegment2d exposing (LineSegment2d)
import Mass
import Point2d exposing (Point2d)
import Polygon2d
import Quantity exposing (Quantity)
import Random.Pcg.Extended
import Set exposing (Set)
import Svg.LocalExtra
import Time
import Vector2d exposing (Vector2d)
import Web
import Web.Audio
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
    , playerGrabbed : Maybe { vineIndex : Int, vineNodeIndex : VineNodeIndex }
    , vines : List Vine
    , bushes : List ColoredCircle
    , clouds : List ColoredCircle
    , woodImperfections : List ColoredCircle
    , startTime : Maybe Time.Posix
    , music : Maybe Web.AudioSource
    , hasGrabbedInThePast : Bool
    }


type alias ColoredCircle =
    { color : Color
    , radius : Quantity Float Length.Meters
    , location : Point2d Length.Meters Float
    }


type alias Vine =
    { start : VineNode
    , between : List VineNode
    , end : VineNode
    }


type alias VineNode =
    { location : Point2d Length.Meters Float
    , velocity : Vector2d (Quantity.Rate Length.Meters Duration.Seconds) Float
    , mass : Quantity Float Mass.Kilograms
    }


app =
    { initialState = initialState
    , interface = interface
    }


initialState : State
initialState =
    playingState


playingState =
    { windowSize = { width = 1920, height = 1080 }
    , randomness = Nothing
    , lastSimulationTime = Nothing
    , playerLocation = Point2d.fromMeters { x = -17, y = -2 }
    , playerVelocity =
        Vector2d.fromMeters { x = 19, y = 10 }
            |> Vector2d.per Duration.second
    , playerGrabbed = Nothing
    , vines = []
    , bushes = []
    , clouds = []
    , woodImperfections = []
    , music = Nothing
    , startTime = Nothing
    , hasGrabbedInThePast = False
    }


veryLongVineRandomGenerator =
    Random.Pcg.Extended.andThen (\nodeCount -> vineRandomGenerator nodeCount)
        (Random.Pcg.Extended.int 31 37)


longVineRandomGenerator =
    Random.Pcg.Extended.andThen (\nodeCount -> vineRandomGenerator nodeCount)
        (Random.Pcg.Extended.int 13 16)


shortVineRandomGenerator =
    Random.Pcg.Extended.andThen (\nodeCount -> vineRandomGenerator nodeCount)
        (Random.Pcg.Extended.int 9 10)


mediumVineRandomGenerator : Random.Pcg.Extended.Generator { bushes : List ColoredCircle, vine : Vine }
mediumVineRandomGenerator =
    Random.Pcg.Extended.constant
        (\vine bush ->
            { vine = vine
            , bushes = bush
            }
        )
        |> Random.Pcg.Extended.andMap shortVineRandomGenerator
        |> Random.Pcg.Extended.andMap bushRandomGenerator


longVineFromBushRandomGenerator : Random.Pcg.Extended.Generator { bushes : List ColoredCircle, vine : Vine }
longVineFromBushRandomGenerator =
    Random.Pcg.Extended.constant
        (\vine bush ->
            { vine = vine
            , bushes = bush
            }
        )
        |> Random.Pcg.Extended.andMap longVineRandomGenerator
        |> Random.Pcg.Extended.andMap bushRandomGenerator


vineAndBushesTranslateBy :
    Vector2d Length.Meters Float
    -> { vine : Vine, bushes : List ColoredCircle }
    -> { vine : Vine, bushes : List ColoredCircle }
vineAndBushesTranslateBy offset vineAndBushes =
    { vine = vineAndBushes.vine |> vineTranslateBy offset
    , bushes = vineAndBushes.bushes |> bushTranslateBy offset
    }


interface : State -> Web.Interface State
interface state =
    [ [ Web.Window.sizeRequest, Web.Window.resizeListen ]
        |> Web.interfaceBatch
        |> Web.interfaceFutureMap (\size -> { state | windowSize = size })
    , Web.Time.posixRequest
        |> Web.interfaceFutureMap
            (\startTime -> { state | startTime = Just startTime })
    , case state.music of
        Nothing ->
            Web.Audio.sourceLoad "tarzan-by-iqui.mp3"
                |> Web.interfaceFutureMap
                    (\loadResult ->
                        case loadResult of
                            Ok musicSource ->
                                { state | music = Just musicSource }

                            Err _ ->
                                state
                    )

        Just musicSource ->
            case state.startTime of
                Nothing ->
                    Web.interfaceNone

                Just startTime ->
                    Web.Audio.fromSource musicSource startTime
                        |> Web.Audio.play
    , case state.randomness of
        Just _ ->
            Web.interfaceNone

        Nothing ->
            Web.Random.unsignedInt32s 4
                |> Web.interfaceFutureMap
                    (\initialRandomness ->
                        case initialRandomness of
                            [] ->
                                state

                            initialRandomnessInt0 :: initialRandomnessInt1Up ->
                                let
                                    initialSeed : Random.Pcg.Extended.Seed
                                    initialSeed =
                                        Random.Pcg.Extended.initialSeed initialRandomnessInt0 initialRandomnessInt1Up

                                    ( generated, newSeed ) =
                                        Random.Pcg.Extended.step
                                            (Random.Pcg.Extended.constant
                                                (\bottom right otherVines backgroundBushes woodImperfections cloudsLeft cloudsRight ->
                                                    { onTree =
                                                        (bottom |> vineAndBushesTranslateBy (Vector2d.fromMeters { x = 4, y = 5 }))
                                                            :: (right |> vineAndBushesTranslateBy (Vector2d.fromMeters { x = 20, y = 7 }))
                                                            :: otherVines
                                                    , backgroundBushes = backgroundBushes |> List.concat
                                                    , woodImperfections = woodImperfections |> List.concat
                                                    , clouds =
                                                        (cloudsLeft ++ cloudsRight) |> List.concat
                                                    }
                                                )
                                                |> Random.Pcg.Extended.andMap mediumVineRandomGenerator
                                                |> Random.Pcg.Extended.andMap longVineFromBushRandomGenerator
                                                |> Random.Pcg.Extended.andMap
                                                    (List.range 0 9
                                                        |> List.map
                                                            (\i ->
                                                                Random.Pcg.Extended.constant
                                                                    (\bush x ->
                                                                        bush
                                                                            |> vineAndBushesTranslateBy
                                                                                (Vector2d.fromMeters
                                                                                    { x = x, y = 5 + (i |> Basics.toFloat) * 10 }
                                                                                )
                                                                    )
                                                                    |> Random.Pcg.Extended.andMap
                                                                        (Random.Pcg.Extended.choices
                                                                            mediumVineRandomGenerator
                                                                            [ longVineFromBushRandomGenerator ]
                                                                        )
                                                                    |> Random.Pcg.Extended.andMap
                                                                        (Random.Pcg.Extended.float -22 22)
                                                            )
                                                        |> randomPcgExtendedListAll
                                                    )
                                                |> Random.Pcg.Extended.andMap
                                                    (List.range 0 9
                                                        |> List.map
                                                            (\i ->
                                                                backgroundBushRandomGenerator
                                                                    |> Random.Pcg.Extended.map
                                                                        (bushTranslateBy
                                                                            (Vector2d.fromMeters
                                                                                { x = 0, y = (i |> Basics.toFloat) * 20 }
                                                                            )
                                                                        )
                                                            )
                                                        |> randomPcgExtendedListAll
                                                    )
                                                |> Random.Pcg.Extended.andMap
                                                    (List.range 0 9
                                                        |> List.map
                                                            (\i ->
                                                                woodImperfectionsRandomGenerator
                                                                    |> Random.Pcg.Extended.map
                                                                        (bushTranslateBy
                                                                            (Vector2d.fromMeters
                                                                                { x = 0, y = 5 + (i |> Basics.toFloat) * 20 }
                                                                            )
                                                                        )
                                                            )
                                                        |> randomPcgExtendedListAll
                                                    )
                                                |> Random.Pcg.Extended.andMap
                                                    (List.range 0 9
                                                        |> List.map
                                                            (\i ->
                                                                Random.Pcg.Extended.constant
                                                                    (\cloud x ->
                                                                        cloud
                                                                            |> coloredCirclesTranslateBy
                                                                                (Vector2d.fromMeters
                                                                                    { x = x, y = 12 + (i |> Basics.toFloat) * 18 }
                                                                                )
                                                                    )
                                                                    |> Random.Pcg.Extended.andMap cloudRandomGenerator
                                                                    |> Random.Pcg.Extended.andMap
                                                                        (Random.Pcg.Extended.float -53 -35)
                                                            )
                                                        |> randomPcgExtendedListAll
                                                    )
                                                |> Random.Pcg.Extended.andMap
                                                    (List.range 0 9
                                                        |> List.map
                                                            (\i ->
                                                                Random.Pcg.Extended.constant
                                                                    (\cloud x ->
                                                                        cloud
                                                                            |> coloredCirclesTranslateBy
                                                                                (Vector2d.fromMeters
                                                                                    { x = x, y = 12 + (i |> Basics.toFloat) * 18 }
                                                                                )
                                                                    )
                                                                    |> Random.Pcg.Extended.andMap cloudRandomGenerator
                                                                    |> Random.Pcg.Extended.andMap
                                                                        (Random.Pcg.Extended.float 35 53)
                                                            )
                                                        |> randomPcgExtendedListAll
                                                    )
                                            )
                                            initialSeed
                                in
                                { state
                                    | randomness =
                                        { initial = ( initialRandomnessInt0, initialRandomnessInt1Up )
                                        , seed = newSeed
                                        }
                                            |> Just
                                    , vines =
                                        generated.onTree |> List.map .vine
                                    , bushes =
                                        generated.backgroundBushes
                                            ++ (generated.onTree
                                                    |> List.concatMap .bushes
                                                    |> bushTranslateBy (Vector2d.fromMeters { x = 4, y = 5 })
                                               )
                                    , clouds = generated.clouds
                                    , woodImperfections = generated.woodImperfections
                                }
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
                [ Svg.LocalExtra.fillUniform (Color.rgb 0 0.3 0.7)
                , Web.Dom.attribute "width" "100%"
                , Web.Dom.attribute "height" "100%"
                ]
                []

        controlsUi : Web.Dom.Node state_
        controlsUi =
            if state.hasGrabbedInThePast then
                Web.Svg.element "g" [] []

            else
                Web.Svg.element "g"
                    []
                    [ Web.Svg.element "rect"
                        [ Svg.LocalExtra.fillUniform (Color.rgba 0 0 0 0.5)
                        , Web.Dom.attribute "height" "10%"
                        , Web.Dom.attribute "width" "100%"
                        , Web.Dom.attribute "x" "0%"
                        , Web.Dom.attribute "y" "0%"
                        ]
                        []
                    , Web.Svg.element "text"
                        [ Svg.LocalExtra.fillUniform (Color.rgb 0 1 1)
                        , Web.Dom.style "font-size" "40px"
                        , Web.Dom.style "font-family" "sans-serif"
                        , Web.Dom.style "font-weight" "bold"
                        , Web.Dom.attribute "x" "40%"
                        , Web.Dom.attribute "y" "6%"
                        ]
                        [ Web.Dom.text "hold space to grab"
                        ]
                    ]

        playerEyeUi location =
            let
                eyeCenter =
                    location
                        |> Point2d.translateBy
                            (state.playerVelocity
                                |> Vector2d.for (Duration.seconds 0.01)
                                |> Vector2d.scaleBy -1
                            )
            in
            Web.Svg.element "g"
                []
                [ Svg.LocalExtra.circle
                    { position =
                        eyeCenter
                            |> Point2d.toRecord Length.inMeters
                    , radius = 0.35
                    }
                    [ Svg.LocalExtra.fillUniform (Color.rgba 1 1 1 0.6)
                    ]
                , let
                    eyeDirection =
                        state.playerVelocity
                            |> Vector2d.scaleBy -1
                            |> Vector2d.direction
                            |> Maybe.withDefault Direction2d.negativeY
                  in
                  Svg.LocalExtra.circle
                    { position =
                        eyeCenter
                            |> Point2d.translateBy (Vector2d.fromMeters { x = 0.1, y = 0.1 })
                            |> Point2d.translateBy
                                (Vector2d.withLength (Length.meters 0.1) eyeDirection)
                            |> Point2d.toRecord Length.inMeters
                    , radius = 0.15
                    }
                    [ Svg.LocalExtra.fillUniform (Color.rgb 0 0 0)
                    ]
                ]

        playerArmWidth =
            0.7

        playerLegWidth =
            0.7

        playerArmUi start =
            Svg.LocalExtra.line
                { start =
                    start
                        |> Point2d.toRecord Length.inMeters
                , end =
                    start
                        |> Point2d.translateBy
                            (Vector2d.fromMeters
                                { x =
                                    state.playerVelocity
                                        |> Vector2d.scaleBy -1
                                        |> Vector2d.for (Duration.seconds 0.05)
                                        |> Vector2d.xComponent
                                        |> Length.inMeters
                                , y =
                                    Length.meters 0.3
                                        |> Quantity.plus
                                            (state.playerVelocity
                                                |> Vector2d.scaleBy -1
                                                |> Vector2d.for (Duration.seconds 0.1)
                                                |> Vector2d.yComponent
                                            )
                                        |> Length.inMeters
                                }
                            )
                        |> Point2d.toRecord Length.inMeters
                }
                [ Svg.LocalExtra.strokeWidth playerArmWidth
                , Web.Dom.style "stroke-linecap" "round"
                , Svg.LocalExtra.strokeUniform (Color.rgb 0.5 0.2 0)
                ]

        playerLegUi start =
            Svg.LocalExtra.line
                { start =
                    start
                        |> Point2d.toRecord Length.inMeters
                , end =
                    start
                        |> Point2d.translateBy
                            (Vector2d.fromMeters
                                { x =
                                    state.playerVelocity
                                        |> Vector2d.scaleBy -1
                                        |> Vector2d.for (Duration.seconds 0.05)
                                        |> Vector2d.xComponent
                                        |> Length.inMeters
                                , y =
                                    Length.meters -0.3
                                        |> Quantity.minus
                                            (state.playerVelocity
                                                |> Vector2d.scaleBy -1
                                                |> Vector2d.for (Duration.seconds 0.08)
                                                |> Vector2d.yComponent
                                            )
                                        |> Length.inMeters
                                }
                            )
                        |> Point2d.toRecord Length.inMeters
                }
                [ Svg.LocalExtra.strokeWidth playerArmWidth
                , Web.Dom.style "stroke-linecap" "round"
                , Svg.LocalExtra.strokeUniform (Color.rgb 0.38 0.15 0)
                ]

        playerUi : Web.Dom.Node state_
        playerUi =
            Web.Svg.element "g"
                []
                [ playerLegUi
                    (state.playerLocation
                        |> Point2d.translateBy
                            (Vector2d.fromMeters { x = -0.8 + playerLegWidth / 2, y = -0.5 })
                    )
                , playerLegUi
                    (state.playerLocation
                        |> Point2d.translateBy
                            (Vector2d.fromMeters { x = 0.8 - playerLegWidth / 2, y = -0.5 })
                    )
                , playerArmUi
                    (state.playerLocation
                        |> Point2d.translateBy
                            (Vector2d.fromMeters { x = -1 + playerArmWidth / 2, y = 0.2 })
                    )
                , playerArmUi
                    (state.playerLocation
                        |> Point2d.translateBy
                            (Vector2d.fromMeters { x = 1 - playerArmWidth / 2, y = 0.2 })
                    )
                , Svg.LocalExtra.circle
                    { position =
                        state.playerLocation |> Point2d.toRecord Length.inMeters
                    , radius = 1
                    }
                    [ Svg.LocalExtra.fillUniform (Color.rgb 0.7 0.38 0)
                    ]
                , playerEyeUi
                    (state.playerLocation
                        |> Point2d.translateBy
                            (Vector2d.fromMeters { x = -0.35, y = 0.4 })
                    )
                , playerEyeUi
                    (state.playerLocation
                        |> Point2d.translateBy
                            (Vector2d.fromMeters { x = 0.35, y = 0.4 })
                    )
                ]

        woodImperfectionsUi : Web.Dom.Node state_
        woodImperfectionsUi =
            Web.Svg.element "g"
                []
                (state.woodImperfections |> List.map coloredCircleUi)

        bushesUi : Web.Dom.Node state_
        bushesUi =
            Web.Svg.element "g"
                []
                (state.bushes |> List.map coloredCircleUi)

        cloudsUi : Web.Dom.Node state_
        cloudsUi =
            Web.Svg.element "g"
                []
                (state.clouds |> List.map coloredCircleUi)

        coloredCircleUi : ColoredCircle -> Web.Dom.Node state_
        coloredCircleUi coloredCircle =
            Svg.LocalExtra.circle
                { position = coloredCircle.location |> Point2d.toRecord Length.inMeters
                , radius = coloredCircle.radius |> Length.inMeters
                }
                [ Svg.LocalExtra.fillUniform coloredCircle.color
                , Web.Dom.attribute "filter" "url(#crumsy)"
                ]

        mainTreeUi =
            Svg.LocalExtra.polygon
                (Polygon2d.singleLoop
                    (List.map Point2d.fromMeters
                        [ { y = 170, x = -20 }
                        , { y = -25, x = -20 }
                        , { y = -25, x = 20 }
                        , { y = 170, x = 20 }
                        ]
                    )
                )
                [ Svg.LocalExtra.fillUniform (Color.rgb 0.2 0.1 0)
                , Svg.LocalExtra.strokeWidth 4
                , Svg.LocalExtra.strokeUniform (Color.rgb 0.17 0.09 0)
                ]

        vineUi : Vine -> Web.Dom.Node state_
        vineUi vine =
            Svg.LocalExtra.polyline
                ((vine.start
                    :: vine.between
                    ++ [ vine.end ]
                 )
                    |> List.map (\node -> node.location |> Point2d.toRecord Length.inMeters)
                )
                [ Svg.LocalExtra.strokeWidth 1
                , Svg.LocalExtra.strokeUniform (Color.rgb 0.41 0.28 0.01)
                , Svg.LocalExtra.fillUniform (Color.rgba 0 0 0 0.02)
                , Web.Dom.style "stroke-linejoin" "round"
                , Web.Dom.style "stroke-linecap" "round"
                ]

        vinesUi : Web.Dom.Node state_
        vinesUi =
            Web.Svg.element "g"
                []
                (state.vines
                    |> List.map vineUi
                )
      in
      Web.Dom.element "div"
        [ Web.Dom.style "background-color" (Color.rgb 0.05 0.05 0.05 |> Color.toCssString)
        , Web.Dom.style "position" "fixed"
        , Web.Dom.style "top" "0"
        , Web.Dom.style "right" "0"
        , Web.Dom.style "bottom" "0"
        , Web.Dom.style "left" "0"
        ]
        [ Web.Svg.element "svg"
            [ Web.Dom.attribute "viewBox" ([ "0 0 ", worldSize.width |> String.fromFloat, " ", worldSize.height |> String.fromFloat ] |> String.concat)
            , Web.Dom.attribute "width" ((worldSize.width |> String.fromFloat) ++ "px")
            , Web.Dom.attribute "height" ((worldSize.height |> String.fromFloat) ++ "px")
            , Web.Dom.style "display" "block"
            , Web.Dom.style "margin" "auto"
            ]
            [ svgFilterDefinitions
            , worldUi
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
                    [ let
                        cameraLocation : Point2d Length.Meters Float
                        cameraLocation =
                            -- TODO slightly behind player?
                            -- TODO more narrow/flakes fov when fast?
                            state.playerLocation
                      in
                      Web.Svg.element "g"
                        [ Svg.LocalExtra.translated
                            { x = -(cameraLocation |> Point2d.xCoordinate |> Length.inMeters)
                            , y =
                                -(Basics.max
                                    -1
                                    (cameraLocation |> Point2d.yCoordinate |> Length.inMeters)
                                 )
                            }
                        ]
                        [ mainTreeUi
                        , woodImperfectionsUi
                        , vinesUi
                        , playerUi
                        , bushesUi
                        , cloudsUi
                        ]
                    ]
                ]
            , controlsUi
            ]
        ]
        |> Web.Dom.render
    , Web.Time.periodicallyListen (Duration.milliseconds 16)
        |> Web.interfaceFutureMap
            (\newTime ->
                if state.playerLocation |> Point2d.yCoordinate |> Quantity.lessThan (Length.meters -25) then
                    { playingState
                        | windowSize = state.windowSize
                        , music = state.music
                    }

                else
                    let
                        durationToSimulate : Duration
                        durationToSimulate =
                            case state.lastSimulationTime of
                                Nothing ->
                                    Duration.seconds 0

                                Just lastSimulationTime ->
                                    Duration.from lastSimulationTime newTime

                        newVines : List Vine
                        newVines =
                            state.vines |> List.map vineUpdate

                        vineUpdate : Vine -> Vine
                        vineUpdate vine =
                            let
                                newEndVelocity =
                                    let
                                        connected : VineNode
                                        connected =
                                            case List.reverse vine.between of
                                                [] ->
                                                    vine.start

                                                nextNode :: _ ->
                                                    nextNode
                                    in
                                    vine.end.velocity
                                        |> Vector2d.plus
                                            (accelerationBetweenVinePoints vine.end connected
                                                |> Vector2d.for durationToSimulate
                                            )
                                        |> Vector2d.plus
                                            (gravity |> Vector2d.for durationToSimulate)
                                        |> Vector2d.scaleBy
                                            (1 - (frictionPercentageEachSecond * (durationToSimulate |> Duration.inSeconds)))
                            in
                            { start = vine.start
                            , between =
                                vine.between
                                    |> listMapWithNeighbors
                                        (\maybePreviousNodeInBetween node maybeNextNodeInBetween ->
                                            let
                                                previousConnected : VineNode
                                                previousConnected =
                                                    case maybePreviousNodeInBetween of
                                                        Nothing ->
                                                            vine.start

                                                        Just previousNode ->
                                                            previousNode

                                                nextConnected : VineNode
                                                nextConnected =
                                                    case maybeNextNodeInBetween of
                                                        Nothing ->
                                                            vine.end

                                                        Just nextNode ->
                                                            nextNode

                                                newVelocity =
                                                    node.velocity
                                                        |> Vector2d.plus
                                                            (accelerationBetweenVinePoints node previousConnected
                                                                |> Vector2d.for durationToSimulate
                                                            )
                                                        |> Vector2d.plus
                                                            (accelerationBetweenVinePoints node nextConnected
                                                                |> Vector2d.for durationToSimulate
                                                            )
                                                        |> Vector2d.plus
                                                            (gravity |> Vector2d.for durationToSimulate)
                                                        |> Vector2d.scaleBy
                                                            (1 - (frictionPercentageEachSecond * (durationToSimulate |> Duration.inSeconds)))
                                            in
                                            { location =
                                                node.location
                                                    |> Point2d.translateBy
                                                        (newVelocity |> Vector2d.for durationToSimulate)
                                            , velocity = newVelocity
                                            , mass = node.mass
                                            }
                                        )
                            , end =
                                { location =
                                    vine.end.location
                                        |> Point2d.translateBy
                                            (newEndVelocity |> Vector2d.for durationToSimulate)
                                , velocity = newEndVelocity
                                , mass = vine.end.mass
                                }
                            }
                    in
                    case state.playerGrabbed of
                        Nothing ->
                            let
                                newPlayerVelocity : Vector2d (Quantity.Rate Length.Meters Duration.Seconds) Float
                                newPlayerVelocity =
                                    state.playerVelocity
                                        |> Vector2d.plus
                                            (gravity |> Vector2d.for durationToSimulate)

                                newPlayerLocation : Point2d Length.Meters Float
                                newPlayerLocation =
                                    state.playerLocation
                                        |> Point2d.translateBy
                                            (newPlayerVelocity |> Vector2d.for durationToSimulate)
                            in
                            { state
                                | lastSimulationTime = Just newTime
                                , playerLocation = newPlayerLocation
                                , playerVelocity = newPlayerVelocity
                                , vines = newVines
                            }

                        Just playerGrabNodeIndex ->
                            { state
                                | lastSimulationTime = Just newTime
                                , playerLocation =
                                    case newVines |> vineNodeAtIndex playerGrabNodeIndex of
                                        Just grabbedNode ->
                                            grabbedNode.location

                                        -- should not happen
                                        Nothing ->
                                            state.playerLocation
                                , vines = newVines
                            }
            )
    , case state.playerGrabbed of
        Just _ ->
            Web.interfaceNone

        Nothing ->
            Web.Window.listenTo "keydown"
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
                                if grabActionKeyboardKey == key then
                                    grab state

                                else
                                    state
                    )
    , case state.playerGrabbed of
        Nothing ->
            Web.interfaceNone

        Just playerGrab ->
            Web.Window.listenTo "keyup"
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
                                let
                                    maybeGrabbedNode =
                                        state.vines
                                            |> vineNodeAtIndex playerGrab
                                in
                                if grabActionKeyboardKey == key then
                                    { state
                                        | playerGrabbed = Nothing
                                        , playerVelocity =
                                            case maybeGrabbedNode of
                                                Just grabbedNode ->
                                                    grabbedNode.velocity
                                                        |> Vector2d.plus releaseBoost

                                                -- should not happen
                                                Nothing ->
                                                    state.playerVelocity
                                        , vines =
                                            state.vines
                                                |> vineNodeAtIndexAlter playerGrab
                                                    (\releasedNode ->
                                                        { releasedNode
                                                            | mass = vineNodeMass
                                                        }
                                                    )
                                    }

                                else
                                    state
                    )
    ]
        |> Web.interfaceBatch


svgFilterDefinitions =
    Web.Dom.element "defs"
        []
        [ Web.Dom.element "filter"
            [ Web.Dom.attribute "id" "crumsy" ]
            [ Web.Dom.element "feTurbulence"
                [ Web.Dom.attribute "type" "turbulence"
                , Web.Dom.attribute "baseFrequency" "0.012 0.02"
                , Web.Dom.attribute "numOctaves" "2"
                , Web.Dom.attribute "result" "turbulence"
                , Web.Dom.attribute "seed" "1"
                , Web.Dom.attribute "stitchTiles" "stitch"
                ]
                []
            , Web.Dom.element "feDisplacementMap"
                [ Web.Dom.attribute "in" "SourceGraphic"
                , Web.Dom.attribute "in2" "turbulence"
                , Web.Dom.attribute "scale" "50"
                , Web.Dom.attribute "xChannelSelector" "R"
                , Web.Dom.attribute "yChannelSelector" "G"
                ]
                []
            ]
        ]


releaseBoost : Vector2d (Quantity.Rate Length.Meters Duration.Seconds) Float
releaseBoost =
    Vector2d.fromMeters { x = 0, y = 22 }
        |> Vector2d.per Duration.second


vineNodeAtIndex :
    { vineIndex : Int, vineNodeIndex : VineNodeIndex }
    -> List Vine
    -> Maybe VineNode
vineNodeAtIndex vineNodeIndex vines =
    vines
        |> listElementAtIndex vineNodeIndex.vineIndex
        |> Maybe.andThen
            (\grabbedVine ->
                case vineNodeIndex.vineNodeIndex of
                    VineEnd ->
                        Just grabbedVine.end

                    VineBetweenIndex vineBetweenNodeIndex ->
                        grabbedVine.between
                            |> listElementAtIndex vineBetweenNodeIndex
            )


vineNodeAtIndexAlter :
    { vineIndex : Int, vineNodeIndex : VineNodeIndex }
    -> (VineNode -> VineNode)
    -> List Vine
    -> List Vine
vineNodeAtIndexAlter vineNodeIndex vineNodeChange vines =
    vines
        |> listElementAtIndexAlter vineNodeIndex.vineIndex
            (\grabbedVine ->
                case vineNodeIndex.vineNodeIndex of
                    VineEnd ->
                        { grabbedVine
                            | end = grabbedVine.end |> vineNodeChange
                        }

                    VineBetweenIndex vineBetweenNodeIndex ->
                        { grabbedVine
                            | between =
                                grabbedVine.between
                                    |> listElementAtIndexAlter vineBetweenNodeIndex
                                        vineNodeChange
                        }
            )


grab : State -> State
grab state =
    let
        distanceToPlayer : Point2d Length.Meters Float -> Quantity Float Length.Meters
        distanceToPlayer nodeLocation =
            LineSegment2d.from state.playerLocation nodeLocation
                |> LineSegment2d.length

        closestVineNode : Maybe { distance : Quantity Float Length.Meters, vineIndex : Int, vineNodeIndex : VineNodeIndex }
        closestVineNode =
            state.vines
                |> List.indexedMap
                    (\vineIndex vine ->
                        let
                            betweenNodeInfos =
                                vine.between
                                    |> List.indexedMap
                                        (\betweenIndex betweenNode ->
                                            { distance = distanceToPlayer betweenNode.location
                                            , vineIndex = vineIndex
                                            , vineNodeIndex = VineBetweenIndex betweenIndex
                                            }
                                        )
                        in
                        { distance = distanceToPlayer vine.end.location
                        , vineIndex = vineIndex
                        , vineNodeIndex = VineEnd
                        }
                            :: betweenNodeInfos
                    )
                |> List.concat
                |> listFindBy
                    (\a b ->
                        if a.distance |> Quantity.lessThanOrEqualTo b.distance then
                            a

                        else
                            b
                    )

        maybeClosestGrabbableVineNode =
            closestVineNode
                |> Maybe.andThen
                    (\closest ->
                        if closest.distance |> Quantity.greaterThanOrEqualTo playerReachLength then
                            Nothing

                        else
                            Just
                                { vineIndex = closest.vineIndex
                                , vineNodeIndex = closest.vineNodeIndex
                                }
                    )
    in
    case maybeClosestGrabbableVineNode of
        Nothing ->
            state

        Just closestGrabbableVineNode ->
            { state
                | playerGrabbed = Just closestGrabbableVineNode
                , vines =
                    state.vines
                        |> vineNodeAtIndexAlter closestGrabbableVineNode
                            (\node ->
                                { location = state.playerLocation
                                , velocity =
                                    node.velocity
                                        |> Vector2d.plus
                                            state.playerVelocity
                                , mass = vineNodeMass |> Quantity.plus playerMass
                                }
                            )
                , hasGrabbedInThePast = True
            }


vineNodeMass =
    Mass.kilograms 1


playerMass =
    Mass.kilograms 4.2


playerReachLength : Quantity Float Length.Meters
playerReachLength =
    Length.meters 4.5


listFindBy chooseTheBetter list =
    case list of
        [] ->
            Nothing

        head :: tail ->
            tail
                |> List.foldl
                    (\el soFar ->
                        chooseTheBetter soFar el
                    )
                    head
                |> Just


type VineNodeIndex
    = VineBetweenIndex Int
    | VineEnd


frictionPercentageEachSecond : Float
frictionPercentageEachSecond =
    0.7


vineElasticity : Float
vineElasticity =
    1000


preferredVineSegmentLength : Quantity Float Length.Meters
preferredVineSegmentLength =
    Length.meters 1


accelerationBetweenVinePoints :
    VineNode
    -> VineNode
    -> Vector2d (Quantity.Rate (Quantity.Rate Length.Meters Duration.Seconds) Duration.Seconds) Float
accelerationBetweenVinePoints from to =
    let
        lineSegment : LineSegment2d Length.Meters Float
        lineSegment =
            LineSegment2d.from from.location to.location
    in
    Vector2d.withLength
        (Quantity.multiplyBy vineElasticity
            ((lineSegment |> LineSegment2d.length)
                |> Quantity.minus preferredVineSegmentLength
            )
        )
        (lineSegment |> LineSegment2d.direction |> Maybe.withDefault Direction2d.positiveY)
        |> Vector2d.per Duration.second
        |> Vector2d.per Duration.second
        |> Vector2d.scaleBy (1 / (from.mass |> Mass.inKilograms))


bushTranslateBy offset bush =
    coloredCirclesTranslateBy offset bush


cloudTranslateBy offset cloud =
    coloredCirclesTranslateBy offset cloud


coloredCirclesTranslateBy offset bush =
    bush
        |> List.map
            (\coloredCircle ->
                { coloredCircle
                    | location = coloredCircle.location |> Point2d.translateBy offset
                }
            )


woodImperfectionsRandomGenerator : Random.Pcg.Extended.Generator (List ColoredCircle)
woodImperfectionsRandomGenerator =
    Random.Pcg.Extended.andThen
        (\circleCount ->
            Random.Pcg.Extended.list circleCount woodImperfectionRandomGenerator
        )
        (Random.Pcg.Extended.int 10 15)


woodImperfectionRandomGenerator : Random.Pcg.Extended.Generator ColoredCircle
woodImperfectionRandomGenerator =
    Random.Pcg.Extended.constant
        (\radius color locationRadius locationAngle ->
            { location =
                Point2d.origin
                    |> Point2d.translateBy
                        (Vector2d.withLength locationRadius (Direction2d.fromAngle locationAngle))
            , radius = radius
            , color = color
            }
        )
        |> Random.Pcg.Extended.andMap
            (Random.Pcg.Extended.map Length.meters
                (Random.Pcg.Extended.float 0.3 2)
            )
        |> Random.Pcg.Extended.andMap woodImperfectionColorRandomGenerator
        |> Random.Pcg.Extended.andMap
            (Random.Pcg.Extended.map Length.meters
                (Random.Pcg.Extended.float 16 19)
            )
        |> Random.Pcg.Extended.andMap
            (Random.Pcg.Extended.map Angle.turns
                (Random.Pcg.Extended.float 0 1)
            )


woodImperfectionColorRandomGenerator : Random.Pcg.Extended.Generator Color
woodImperfectionColorRandomGenerator =
    Random.Pcg.Extended.constant (\r g -> Color.rgba r g 0 0.2)
        |> Random.Pcg.Extended.andMap (Random.Pcg.Extended.float 0.3 0.55)
        |> Random.Pcg.Extended.andMap (Random.Pcg.Extended.float 0.2 0.25)


backgroundBushRandomGenerator : Random.Pcg.Extended.Generator (List ColoredCircle)
backgroundBushRandomGenerator =
    Random.Pcg.Extended.andThen
        (\circleCount ->
            Random.Pcg.Extended.list circleCount backgroundBushCircleRandomGenerator
        )
        (Random.Pcg.Extended.int 11 15)


backgroundBushCircleRandomGenerator : Random.Pcg.Extended.Generator ColoredCircle
backgroundBushCircleRandomGenerator =
    Random.Pcg.Extended.constant
        (\radius color locationRadius locationAngle ->
            { location =
                Point2d.origin
                    |> Point2d.translateBy
                        (Vector2d.withLength locationRadius (Direction2d.fromAngle locationAngle))
            , radius = radius
            , color = color
            }
        )
        |> Random.Pcg.Extended.andMap
            (Random.Pcg.Extended.map Length.meters
                (Random.Pcg.Extended.float 0.3 7)
            )
        |> Random.Pcg.Extended.andMap backgroundBushCircleColorRandomGenerator
        |> Random.Pcg.Extended.andMap
            (Random.Pcg.Extended.map Length.meters
                (Random.Pcg.Extended.float 17 26)
            )
        |> Random.Pcg.Extended.andMap
            (Random.Pcg.Extended.map Angle.turns
                (Random.Pcg.Extended.float 0 1)
            )


backgroundBushCircleColorRandomGenerator : Random.Pcg.Extended.Generator Color
backgroundBushCircleColorRandomGenerator =
    Random.Pcg.Extended.constant (\r g b -> Color.rgba r g b 0.27)
        |> Random.Pcg.Extended.andMap (Random.Pcg.Extended.float 0 0.15)
        |> Random.Pcg.Extended.andMap (Random.Pcg.Extended.float 0.4 0.6)
        |> Random.Pcg.Extended.andMap (Random.Pcg.Extended.float 0 0.3)


bushRandomGenerator : Random.Pcg.Extended.Generator (List ColoredCircle)
bushRandomGenerator =
    Random.Pcg.Extended.andThen
        (\circleCount ->
            Random.Pcg.Extended.list circleCount bushCircleRandomGenerator
        )
        (Random.Pcg.Extended.int 18 28)


bushCircleRandomGenerator : Random.Pcg.Extended.Generator ColoredCircle
bushCircleRandomGenerator =
    Random.Pcg.Extended.constant
        (\radius color locationRadius locationAngle ->
            { location =
                Point2d.origin
                    |> Point2d.translateBy
                        (Vector2d.withLength locationRadius (Direction2d.fromAngle locationAngle))
                    |> Point2d.translateBy (Vector2d.fromMeters { x = -(locationRadius |> Length.inMeters), y = -2 * (locationRadius |> Length.inMeters) })
            , radius = radius
            , color = color
            }
        )
        |> Random.Pcg.Extended.andMap
            (Random.Pcg.Extended.map Length.meters
                (Random.Pcg.Extended.float 1 1.9)
            )
        |> Random.Pcg.Extended.andMap bushCircleColorRandomGenerator
        |> Random.Pcg.Extended.andMap
            (Random.Pcg.Extended.map Length.meters
                (Random.Pcg.Extended.float 0 3.9)
            )
        |> Random.Pcg.Extended.andMap
            (Random.Pcg.Extended.map Angle.turns
                (Random.Pcg.Extended.float -0.1 0.6)
            )


bushCircleColorRandomGenerator : Random.Pcg.Extended.Generator Color
bushCircleColorRandomGenerator =
    Random.Pcg.Extended.constant (\r g b -> Color.rgba r g b 0.7)
        |> Random.Pcg.Extended.andMap (Random.Pcg.Extended.float 0 0.15)
        |> Random.Pcg.Extended.andMap (Random.Pcg.Extended.float 0.33 0.4)
        |> Random.Pcg.Extended.andMap (Random.Pcg.Extended.float 0 0.3)


cloudRandomGenerator : Random.Pcg.Extended.Generator (List ColoredCircle)
cloudRandomGenerator =
    Random.Pcg.Extended.andThen
        (\circleCount ->
            Random.Pcg.Extended.list circleCount cloudCircleRandomGenerator
        )
        (Random.Pcg.Extended.int 9 13)


cloudCircleRandomGenerator : Random.Pcg.Extended.Generator ColoredCircle
cloudCircleRandomGenerator =
    Random.Pcg.Extended.constant
        (\radius color locationRadius locationAngle ->
            { location =
                Point2d.origin
                    |> Point2d.translateBy
                        (Vector2d.withLength locationRadius (Direction2d.fromAngle locationAngle))
                    |> Point2d.translateBy (Vector2d.fromMeters { x = -(locationRadius |> Length.inMeters), y = -2 * (locationRadius |> Length.inMeters) })
            , radius = radius
            , color = color
            }
        )
        |> Random.Pcg.Extended.andMap
            (Random.Pcg.Extended.map Length.meters
                (Random.Pcg.Extended.float 2.3 3.5)
            )
        |> Random.Pcg.Extended.andMap cloudCircleColorRandomGenerator
        |> Random.Pcg.Extended.andMap
            (Random.Pcg.Extended.map Length.meters
                (Random.Pcg.Extended.float 0 5.9)
            )
        |> Random.Pcg.Extended.andMap
            (Random.Pcg.Extended.map Angle.turns
                (Random.Pcg.Extended.float -0.14 0.64)
            )


cloudCircleColorRandomGenerator : Random.Pcg.Extended.Generator Color
cloudCircleColorRandomGenerator =
    Random.Pcg.Extended.constant (\r g b -> Color.rgba r g b 0.05)
        |> Random.Pcg.Extended.andMap (Random.Pcg.Extended.float 0.8 0.9)
        |> Random.Pcg.Extended.andMap (Random.Pcg.Extended.float 0.8 0.9)
        |> Random.Pcg.Extended.andMap (Random.Pcg.Extended.float 0.9 1)


vineRandomGenerator : Int -> Random.Pcg.Extended.Generator Vine
vineRandomGenerator nodeCount =
    Random.Pcg.Extended.constant
        (\startOffset between endOffset ->
            { start =
                { velocity = Vector2d.zero
                , mass = vineNodeMass
                , location =
                    Point2d.fromMeters { x = 0, y = 0 }
                        |> Point2d.translateBy endOffset
                }
            , between = between
            , end =
                { velocity = Vector2d.zero
                , mass = vineNodeMass
                , location =
                    Point2d.fromMeters { x = 0, y = -(nodeCount |> Basics.toFloat) * 0.8 }
                        |> Point2d.translateBy endOffset
                }
            }
        )
        |> Random.Pcg.Extended.andMap vineNodeLocationVariation
        |> Random.Pcg.Extended.andMap
            (List.range 1 (nodeCount - 1)
                |> List.map
                    (\i ->
                        Random.Pcg.Extended.constant
                            (\offset ->
                                { velocity = Vector2d.zero
                                , mass = vineNodeMass
                                , location =
                                    Point2d.fromMeters { x = 0, y = -(i |> Basics.toFloat) * 0.8 }
                                        |> Point2d.translateBy offset
                                }
                            )
                            |> Random.Pcg.Extended.andMap vineNodeLocationVariation
                    )
                |> randomPcgExtendedListAll
            )
        |> Random.Pcg.Extended.andMap vineNodeLocationVariation


randomPcgExtendedListAll generators =
    case generators of
        [] ->
            Random.Pcg.Extended.constant []

        headGenerator :: tailGenerators ->
            Random.Pcg.Extended.map2 (\head tail -> head :: tail)
                headGenerator
                (randomPcgExtendedListAll tailGenerators)


vineNodeLocationVariation =
    Random.Pcg.Extended.constant
        (\xOffset yOffset -> Vector2d.fromMeters { x = xOffset, y = yOffset })
        |> Random.Pcg.Extended.andMap (Random.Pcg.Extended.float -0.08 0.08)
        |> Random.Pcg.Extended.andMap (Random.Pcg.Extended.float -0.05 0.05)


vineTranslateBy : Vector2d Length.Meters Float -> Vine -> Vine
vineTranslateBy offset vine =
    { start = vine.start |> vineNodeTranslateBy offset
    , between =
        vine.between
            |> List.map
                (\node ->
                    node |> vineNodeTranslateBy offset
                )
    , end = vine.end |> vineNodeTranslateBy offset
    }


vineNodeTranslateBy : Vector2d Length.Meters Float -> VineNode -> VineNode
vineNodeTranslateBy offset vineNode =
    { location = vineNode.location |> Point2d.translateBy offset
    , velocity = vineNode.velocity
    , mass = vineNode.mass
    }


gravity : Vector2d (Quantity.Rate (Quantity.Rate Length.Meters Duration.Seconds) Duration.Seconds) Float
gravity =
    -- would be cool to make dynamic based on nearby rocks!
    Vector2d.fromMeters { x = 0, y = -26 }
        |> Vector2d.per Duration.second
        |> Vector2d.per Duration.second


grabActionKeyboardKey : String
grabActionKeyboardKey =
    " "


worldSizeCells : { x : Float, y : Float }
worldSizeCells =
    { x = 80, y = 45 }


listMapWithNeighbors : (Maybe a -> a -> Maybe a -> b) -> List a -> List b
listMapWithNeighbors elementWithNeighborsToNewElement list =
    let
        justList : List (Maybe a)
        justList =
            list |> List.map Just
    in
    List.map3 elementWithNeighborsToNewElement
        (Nothing :: justList)
        list
        (List.drop 1 justList ++ [ Nothing ])


listElementAtIndex : Int -> List a -> Maybe a
listElementAtIndex index list =
    case list of
        [] ->
            Nothing

        head :: tail ->
            case compare index 0 of
                EQ ->
                    Just head

                LT ->
                    Nothing

                GT ->
                    listElementAtIndex (index - 1) tail


listElementAtIndexAlter : Int -> (a -> a) -> List a -> List a
listElementAtIndexAlter index elementChange list =
    case list of
        [] ->
            []

        head :: tail ->
            case compare index 0 of
                EQ ->
                    (head |> elementChange) :: tail

                LT ->
                    head :: tail

                GT ->
                    head :: listElementAtIndexAlter (index - 1) elementChange tail
