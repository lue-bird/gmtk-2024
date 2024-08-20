module App exposing (app)

import Color exposing (Color)
import Direction2d
import Duration exposing (Duration)
import Json.Decode
import Length
import LineSegment2d exposing (LineSegment2d)
import Mass
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
    , playerGrabbed : Maybe { ropeIndex : Int, ropeNodeIndex : RopeNodeIndex }
    , ropes : List Rope
    , bushes : List BushCircle
    }


type alias BushCircle =
    { color : Color
    , location : Point2d Length.Meters Float
    }


type alias Rope =
    { start : RopeNode
    , between : List RopeNode
    , end : RopeNode
    }


type alias RopeNode =
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
    { windowSize = { width = 1920, height = 1080 }
    , randomness = Nothing
    , lastSimulationTime = Nothing
    , playerLocation = Point2d.fromMeters { x = -17, y = 0 }
    , playerVelocity =
        Vector2d.fromMeters { x = 19, y = 10 }
            |> Vector2d.per Duration.second
    , playerGrabbed = Nothing
    , ropes = []
    , bushes = []
    }


interface : State -> Web.Interface State
interface state =
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
                , Svg.LocalExtra.strokeUniform (Color.rgb 0.5 0.2 0)
                ]

        playerUi : Web.Dom.Node state_
        playerUi =
            Web.Svg.element "g"
                []
                [ playerArmUi
                    (state.playerLocation
                        |> Point2d.translateBy
                            (Vector2d.fromMeters { x = -1 + playerArmWidth / 2, y = 0.2 })
                    )
                , playerArmUi
                    (state.playerLocation
                        |> Point2d.translateBy
                            (Vector2d.fromMeters { x = 1 - playerArmWidth / 2, y = 0.2 })
                    )
                , playerLegUi
                    (state.playerLocation
                        |> Point2d.translateBy
                            (Vector2d.fromMeters { x = -0.8 + playerLegWidth / 2, y = -0.5 })
                    )
                , playerLegUi
                    (state.playerLocation
                        |> Point2d.translateBy
                            (Vector2d.fromMeters { x = 0.8 - playerLegWidth / 2, y = -0.5 })
                    )
                , Svg.LocalExtra.circle
                    { position =
                        state.playerLocation |> Point2d.toRecord Length.inMeters
                    , radius = 1
                    }
                    [ Svg.LocalExtra.fillUniform (Color.rgb 1 0.5 0)
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

        ropeUi : Rope -> Web.Dom.Node state_
        ropeUi rope =
            Svg.LocalExtra.polyline
                ((rope.start
                    :: rope.between
                    ++ [ rope.end ]
                 )
                    |> List.map (\node -> node.location |> Point2d.toRecord Length.inMeters)
                )
                [ Svg.LocalExtra.strokeWidth 1
                , Svg.LocalExtra.strokeUniform (Color.rgb 0.41 0.28 0.01)
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
        [ Web.Svg.element "svg"
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
                        [ ropesUi
                        , playerUi
                        ]
                    ]
                ]
            ]
        ]
        |> Web.Dom.render
    , Web.Time.periodicallyListen (Duration.milliseconds 16)
        |> Web.interfaceFutureMap
            (\newTime ->
                if state.playerLocation |> Point2d.yCoordinate |> Quantity.lessThan (Length.meters -25) then
                    initialState

                else
                    let
                        durationToSimulate : Duration
                        durationToSimulate =
                            case state.lastSimulationTime of
                                Nothing ->
                                    Duration.seconds 0

                                Just lastSimulationTime ->
                                    Duration.from lastSimulationTime newTime

                        newRopes : List Rope
                        newRopes =
                            state.ropes |> List.map ropeUpdate

                        ropeUpdate : Rope -> Rope
                        ropeUpdate rope =
                            let
                                newEndVelocity =
                                    let
                                        connected : RopeNode
                                        connected =
                                            case List.reverse rope.between of
                                                [] ->
                                                    rope.start

                                                nextNode :: _ ->
                                                    nextNode
                                    in
                                    rope.end.velocity
                                        |> Vector2d.plus
                                            (accelerationBetweenRopePoints rope.end connected
                                                |> Vector2d.for durationToSimulate
                                            )
                                        |> Vector2d.plus
                                            (gravity |> Vector2d.for durationToSimulate)
                                        |> Vector2d.scaleBy
                                            (1 - (frictionPercentageEachSecond * (durationToSimulate |> Duration.inSeconds)))
                            in
                            { start = rope.start
                            , between =
                                rope.between
                                    |> listMapWithNeighbors
                                        (\maybePreviousNodeInBetween node maybeNextNodeInBetween ->
                                            let
                                                previousConnected : RopeNode
                                                previousConnected =
                                                    case maybePreviousNodeInBetween of
                                                        Nothing ->
                                                            rope.start

                                                        Just previousNode ->
                                                            previousNode

                                                nextConnected : RopeNode
                                                nextConnected =
                                                    case maybeNextNodeInBetween of
                                                        Nothing ->
                                                            rope.end

                                                        Just nextNode ->
                                                            nextNode

                                                newVelocity =
                                                    node.velocity
                                                        |> Vector2d.plus
                                                            (accelerationBetweenRopePoints node previousConnected
                                                                |> Vector2d.for durationToSimulate
                                                            )
                                                        |> Vector2d.plus
                                                            (accelerationBetweenRopePoints node nextConnected
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
                                    rope.end.location
                                        |> Point2d.translateBy
                                            (newEndVelocity |> Vector2d.for durationToSimulate)
                                , velocity = newEndVelocity
                                , mass = rope.end.mass
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
                                , ropes = newRopes
                            }

                        Just playerGrabNodeIndex ->
                            { state
                                | lastSimulationTime = Just newTime
                                , playerLocation =
                                    case newRopes |> ropeNodeAtIndex playerGrabNodeIndex of
                                        Just grabbedNode ->
                                            grabbedNode.location

                                        -- should not happen
                                        Nothing ->
                                            state.playerLocation
                                , ropes = newRopes
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
                                if grabActionKeyboardKeys |> Set.member key then
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
                                        state.ropes
                                            |> ropeNodeAtIndex playerGrab
                                in
                                if grabActionKeyboardKeys |> Set.member key then
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
                                        , ropes =
                                            state.ropes
                                                |> ropeNodeAtIndexAlter playerGrab
                                                    (\releasedNode ->
                                                        { releasedNode
                                                            | mass = ropeNodeMass
                                                        }
                                                    )
                                    }

                                else
                                    state
                    )
    ]
        |> Web.interfaceBatch


releaseBoost : Vector2d (Quantity.Rate Length.Meters Duration.Seconds) Float
releaseBoost =
    Vector2d.fromMeters { x = 0, y = 25 }
        |> Vector2d.per Duration.second


ropeNodeAtIndex :
    { ropeIndex : Int, ropeNodeIndex : RopeNodeIndex }
    -> List Rope
    -> Maybe RopeNode
ropeNodeAtIndex ropeNodeIndex ropes =
    ropes
        |> listElementAtIndex ropeNodeIndex.ropeIndex
        |> Maybe.andThen
            (\grabbedRope ->
                case ropeNodeIndex.ropeNodeIndex of
                    RopeEnd ->
                        Just grabbedRope.end

                    RopeBetweenIndex ropeBetweenNodeIndex ->
                        grabbedRope.between
                            |> listElementAtIndex ropeBetweenNodeIndex
            )


ropeNodeAtIndexAlter :
    { ropeIndex : Int, ropeNodeIndex : RopeNodeIndex }
    -> (RopeNode -> RopeNode)
    -> List Rope
    -> List Rope
ropeNodeAtIndexAlter ropeNodeIndex ropeNodeChange ropes =
    ropes
        |> listElementAtIndexAlter ropeNodeIndex.ropeIndex
            (\grabbedRope ->
                case ropeNodeIndex.ropeNodeIndex of
                    RopeEnd ->
                        { grabbedRope
                            | end = grabbedRope.end |> ropeNodeChange
                        }

                    RopeBetweenIndex ropeBetweenNodeIndex ->
                        { grabbedRope
                            | between =
                                grabbedRope.between
                                    |> listElementAtIndexAlter ropeBetweenNodeIndex
                                        ropeNodeChange
                        }
            )


grab : State -> State
grab state =
    let
        distanceToPlayer : Point2d Length.Meters Float -> Quantity Float Length.Meters
        distanceToPlayer nodeLocation =
            LineSegment2d.from state.playerLocation nodeLocation
                |> LineSegment2d.length

        closestRopeNode : Maybe { distance : Quantity Float Length.Meters, ropeIndex : Int, ropeNodeIndex : RopeNodeIndex }
        closestRopeNode =
            state.ropes
                |> List.indexedMap
                    (\ropeIndex rope ->
                        let
                            betweenNodeInfos =
                                rope.between
                                    |> List.indexedMap
                                        (\betweenIndex betweenNode ->
                                            { distance = distanceToPlayer betweenNode.location
                                            , ropeIndex = ropeIndex
                                            , ropeNodeIndex = RopeBetweenIndex betweenIndex
                                            }
                                        )
                        in
                        { distance = distanceToPlayer rope.end.location
                        , ropeIndex = ropeIndex
                        , ropeNodeIndex = RopeEnd
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

        maybeClosestGrabbableRopeNode =
            closestRopeNode
                |> Maybe.andThen
                    (\closest ->
                        if closest.distance |> Quantity.greaterThanOrEqualTo playerReachLength then
                            Nothing

                        else
                            Just
                                { ropeIndex = closest.ropeIndex
                                , ropeNodeIndex = closest.ropeNodeIndex
                                }
                    )
    in
    case maybeClosestGrabbableRopeNode of
        Nothing ->
            state

        Just closestGrabbableRopeNode ->
            { state
                | playerGrabbed = Just closestGrabbableRopeNode
                , ropes =
                    state.ropes
                        |> ropeNodeAtIndexAlter closestGrabbableRopeNode
                            (\node ->
                                { location = state.playerLocation
                                , velocity =
                                    node.velocity
                                        |> Vector2d.plus
                                            state.playerVelocity
                                , mass = ropeNodeMass |> Quantity.plus playerMass
                                }
                            )
            }


ropeNodeMass =
    Mass.kilograms 1


playerMass =
    Mass.kilograms 4.2


playerReachLength : Quantity Float Length.Meters
playerReachLength =
    Length.meters 4.2


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


type RopeNodeIndex
    = RopeBetweenIndex Int
    | RopeEnd


frictionPercentageEachSecond : Float
frictionPercentageEachSecond =
    0.7


ropeElasticity : Float
ropeElasticity =
    1000


preferredRopeSegmentLength : Quantity Float Length.Meters
preferredRopeSegmentLength =
    Length.meters 1


accelerationBetweenRopePoints :
    RopeNode
    -> RopeNode
    -> Vector2d (Quantity.Rate (Quantity.Rate Length.Meters Duration.Seconds) Duration.Seconds) Float
accelerationBetweenRopePoints from to =
    -- TODO take masses into account
    let
        lineSegment : LineSegment2d Length.Meters Float
        lineSegment =
            LineSegment2d.from from.location to.location
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
        |> Vector2d.scaleBy (1 / (from.mass |> Mass.inKilograms))


ropeRandomGenerator : Random.Pcg.Extended.Generator Rope
ropeRandomGenerator =
    let
        nodeCount : Int
        nodeCount =
            9
    in
    Random.Pcg.Extended.constant
        (\startOffset between endOffset ->
            { start =
                { velocity = Vector2d.zero
                , mass = ropeNodeMass
                , location =
                    Point2d.fromMeters { x = 0, y = 0 }
                        |> Point2d.translateBy endOffset
                }
            , between = between
            , end =
                { velocity = Vector2d.zero
                , mass = ropeNodeMass
                , location =
                    Point2d.fromMeters { x = 0, y = -(nodeCount |> Basics.toFloat) * 0.8 }
                        |> Point2d.translateBy endOffset
                }
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
                                , mass = ropeNodeMass
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
    { start = rope.start |> ropeNodeTranslateBy offset
    , between =
        rope.between
            |> List.map
                (\node ->
                    node |> ropeNodeTranslateBy offset
                )
    , end = rope.end |> ropeNodeTranslateBy offset
    }


ropeNodeTranslateBy : Vector2d Length.Meters Float -> RopeNode -> RopeNode
ropeNodeTranslateBy offset ropeNode =
    { location = ropeNode.location |> Point2d.translateBy offset
    , velocity = ropeNode.velocity
    , mass = ropeNode.mass
    }


gravity : Vector2d (Quantity.Rate (Quantity.Rate Length.Meters Duration.Seconds) Duration.Seconds) Float
gravity =
    -- would be cool to make dynamic based on nearby rocks!
    Vector2d.fromMeters { x = 0, y = -26 }
        |> Vector2d.per Duration.second
        |> Vector2d.per Duration.second


grabActionKeyboardKeys : Set String
grabActionKeyboardKeys =
    Set.fromList
        [ "w"
        , "ArrowUp"
        , " "
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
