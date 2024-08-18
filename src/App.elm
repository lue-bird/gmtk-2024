module App exposing (app)

{-| paste in example code and fiddle around
-}

import Color
import Direction2d
import Duration exposing (Duration)
import Json.Decode
import Length
import LineSegment2d exposing (LineSegment2d)
import Point2d exposing (Point2d)
import Quantity exposing (Quantity)
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
    , ropes : List Rope
    }


type alias Rope =
    { startLocation : Point2d Length.Meters Float
    , startVelocity : Vector2d (Quantity.Rate Length.Meters Duration.Seconds) Float
    , between :
        List
            { location : Point2d Length.Meters Float
            , velocity : Vector2d (Quantity.Rate Length.Meters Duration.Seconds) Float
            }
    , endLocation : Point2d Length.Meters Float
    , endVelocity : Vector2d (Quantity.Rate Length.Meters Duration.Seconds) Float
    }


ropeElasticity : Float
ropeElasticity =
    840


preferredRopeSegmentLength : Quantity Float Length.Meters
preferredRopeSegmentLength =
    Length.meters 1


accelerationBetweenRopePoints :
    Point2d Length.Meters Float
    -> Point2d Length.Meters Float
    -> Vector2d (Quantity.Rate (Quantity.Rate Length.Meters Duration.Seconds) Duration.Seconds) Float
accelerationBetweenRopePoints start end =
    let
        lineSegment : LineSegment2d Length.Meters Float
        lineSegment =
            LineSegment2d.from start end
    in
    Vector2d.withLength
        (Quantity.multiplyBy ropeElasticity
            ((lineSegment |> LineSegment2d.length)
                |> Quantity.minus preferredRopeSegmentLength
            )
        )
        (lineSegment |> LineSegment2d.direction |> Maybe.withDefault Direction2d.positiveY)
        |> Vector2d.per Duration.second
        |> Vector2d.per Duration.second


app =
    { initialState = initialState
    , interface = interface
    }


initialState : State
initialState =
    { windowSize = { width = 1920, height = 1080 }
    , randomness = Nothing
    , lastSimulationTime = Nothing
    , playerLocation = Point2d.fromMeters { x = 0, y = 0 }
    , playerVelocity =
        Vector2d.fromMeters { x = 2, y = 20 }
            |> Vector2d.per Duration.second
    , ropes = []
    }


ropeRandomGenerator : Random.Pcg.Extended.Generator Rope
ropeRandomGenerator =
    let
        nodeCount : Int
        nodeCount =
            9
    in
    Random.Pcg.Extended.constant
        (\startOffset between endOffset ->
            { startVelocity = Vector2d.zero
            , startLocation =
                Point2d.fromMeters { x = 0, y = 0 }
                    |> Point2d.translateBy endOffset
            , between = between
            , endVelocity = Vector2d.zero
            , endLocation =
                Point2d.fromMeters { x = 0, y = -(nodeCount |> Basics.toFloat) * 0.8 }
                    |> Point2d.translateBy endOffset
            }
        )
        |> Random.Pcg.Extended.andMap ropeNodeLocationVariation
        |> Random.Pcg.Extended.andMap
            (List.range 1 (nodeCount - 1)
                |> List.map
                    (\i ->
                        Random.Pcg.Extended.constant
                            (\offset ->
                                { velocity = Vector2d.zero
                                , location =
                                    Point2d.fromMeters { x = 0, y = -(i |> Basics.toFloat) * 0.8 }
                                        |> Point2d.translateBy offset
                                }
                            )
                            |> Random.Pcg.Extended.andMap ropeNodeLocationVariation
                    )
                |> randomPcgExtendedListAll
            )
        |> Random.Pcg.Extended.andMap ropeNodeLocationVariation


randomPcgExtendedListAll generators =
    case generators of
        [] ->
            Random.Pcg.Extended.constant []

        headGenerator :: tailGenerators ->
            Random.Pcg.Extended.map2 (\head tail -> head :: tail)
                headGenerator
                (randomPcgExtendedListAll tailGenerators)


ropeNodeLocationVariation =
    Random.Pcg.Extended.constant
        (\xOffset yOffset -> Vector2d.fromMeters { x = xOffset, y = yOffset })
        |> Random.Pcg.Extended.andMap (Random.Pcg.Extended.float -0.08 0.08)
        |> Random.Pcg.Extended.andMap (Random.Pcg.Extended.float -0.05 0.05)


ropeTranslateBy : Vector2d Length.Meters Float -> Rope -> Rope
ropeTranslateBy offset rope =
    { startLocation = rope.startLocation |> Point2d.translateBy offset
    , startVelocity = rope.startVelocity
    , between =
        rope.between
            |> List.map
                (\node ->
                    { location = node.location |> Point2d.translateBy offset
                    , velocity = node.velocity
                    }
                )
    , endLocation = rope.endLocation |> Point2d.translateBy offset
    , endVelocity = rope.endVelocity
    }


interface : State -> Web.Interface State
interface =
    \state ->
        [ [ Web.Window.sizeRequest, Web.Window.resizeListen ]
            |> Web.interfaceBatch
            |> Web.interfaceFutureMap (\size -> { state | windowSize = size })
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

                                        ( generatedRope, newSeed ) =
                                            Random.Pcg.Extended.step
                                                ropeRandomGenerator
                                                initialSeed
                                    in
                                    { state
                                        | randomness =
                                            { initial = ( initialRandomnessInt0, initialRandomnessInt1Up )
                                            , seed = newSeed
                                            }
                                                |> Just
                                        , ropes =
                                            [ generatedRope |> ropeTranslateBy (Vector2d.fromMeters { x = 4, y = 5 })
                                            ]
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

            ropeUi : Rope -> Web.Dom.Node state_
            ropeUi rope =
                Svg.LocalExtra.polyline
                    ((rope.startLocation
                        :: (rope.between |> List.map .location)
                        ++ [ rope.endLocation ]
                     )
                        |> List.map (\point -> point |> Point2d.toRecord Length.inMeters)
                    )
                    [ Svg.LocalExtra.strokeWidth 1
                    , Svg.LocalExtra.strokeUniform (Color.rgb 0.36 0.28 0.01)
                    , Web.Dom.style "stroke-linejoin" "round"
                    , Web.Dom.style "stroke-linecap" "round"
                    ]

            ropesUi : Web.Dom.Node state_
            ropesUi =
                Web.Svg.element "g"
                    []
                    (state.ropes
                        |> List.map ropeUi
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
                        [ ropesUi
                        , playerUi
                        ]
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
                                    (newPlayerVelocity |> Vector2d.for durationToSimulate)

                        newPlayerVelocity : Vector2d (Quantity.Rate Length.Meters Duration.Seconds) Float
                        newPlayerVelocity =
                            state.playerVelocity
                                |> Vector2d.plus
                                    (gravity |> Vector2d.for durationToSimulate)

                        newRopes : List Rope
                        newRopes =
                            state.ropes |> List.map ropeUpdate

                        ropeUpdate : Rope -> Rope
                        ropeUpdate rope =
                            let
                                newEndVelocity =
                                    let
                                        connectedLocation : Point2d Length.Meters Float
                                        connectedLocation =
                                            case List.reverse rope.between of
                                                [] ->
                                                    rope.startLocation

                                                nextNode :: _ ->
                                                    nextNode.location
                                    in
                                    rope.endVelocity
                                        |> Vector2d.plus
                                            (accelerationBetweenRopePoints rope.endLocation connectedLocation
                                                |> Vector2d.for durationToSimulate
                                            )
                                        |> Vector2d.plus
                                            (gravity |> Vector2d.for durationToSimulate)
                                        |> Vector2d.scaleBy
                                            (1 - (frictionPercentageEachSecond * (durationToSimulate |> Duration.inSeconds)))
                            in
                            { startLocation =
                                rope.startLocation
                            , startVelocity =
                                rope.startVelocity
                            , between =
                                rope.between
                                    |> listMapWithNeighbors
                                        (\maybePreviousNodeInBetween node maybeNextNodeInBetween ->
                                            let
                                                previousConnectedLocation : Point2d Length.Meters Float
                                                previousConnectedLocation =
                                                    case maybePreviousNodeInBetween of
                                                        Nothing ->
                                                            rope.startLocation

                                                        Just previousNode ->
                                                            previousNode.location

                                                nextConnectedLocation : Point2d Length.Meters Float
                                                nextConnectedLocation =
                                                    case maybeNextNodeInBetween of
                                                        Nothing ->
                                                            rope.endLocation

                                                        Just nextNode ->
                                                            nextNode.location

                                                newVelocity =
                                                    node.velocity
                                                        |> Vector2d.plus
                                                            (accelerationBetweenRopePoints node.location previousConnectedLocation
                                                                |> Vector2d.for durationToSimulate
                                                            )
                                                        |> Vector2d.plus
                                                            (accelerationBetweenRopePoints node.location nextConnectedLocation
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
                                            }
                                        )
                            , endLocation =
                                rope.endLocation
                                    |> Point2d.translateBy
                                        (newEndVelocity |> Vector2d.for durationToSimulate)
                            , endVelocity = newEndVelocity
                            }
                    in
                    if newPlayerLocation |> Point2d.yCoordinate |> Quantity.lessThan (Length.meters -25) then
                        initialState

                    else
                        { state
                            | lastSimulationTime = Just newTime
                            , playerLocation = newPlayerLocation
                            , playerVelocity = newPlayerVelocity
                            , ropes = newRopes
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


frictionPercentageEachSecond : Float
frictionPercentageEachSecond =
    0.7


gravity : Vector2d (Quantity.Rate (Quantity.Rate Length.Meters Duration.Seconds) Duration.Seconds) Float
gravity =
    -- would be cool to make dynamic based on nearby rocks!
    Vector2d.fromMeters { x = 0, y = -26 }
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
