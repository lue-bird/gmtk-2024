port module Main exposing (main)

{-| You do not need to touch this file. Go to `src/App.elm` to paste in examples and fiddle around
-}

import App
import Json.Encode
import Web


main =
    Web.program
        { initialState = App.app.initialState
        , interface = App.app.interface
        , ports = { fromJs = fromJs, toJs = toJs }
        }


port toJs : Json.Encode.Value -> Cmd event_


port fromJs : (Json.Encode.Value -> event) -> Sub event
