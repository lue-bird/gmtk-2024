module App exposing (app)

{-| paste in example code and fiddle around
-}

import Color
import Random.Pcg.Extended
import Svg.LocalExtra
import Web
import Web.Dom
import Web.Random
import Web.Svg
import Web.Window


type alias State =
    { randomness :
        Maybe
            { initial : ( Int, List Int )
            , seed : Random.Pcg.Extended.Seed
            }
    , windowSize : { height : Int, width : Int }
    }


app =
    { initialState =
        { windowSize = { width = 1920, height = 1080 }
        , randomness = Nothing
        }
    , interface = interface
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
                [ worldUi ]
            ]
            |> Web.Dom.render
        ]
            |> Web.interfaceBatch


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
