module Main exposing (main)

import Config
import Platform
import Server.Http as Http exposing (Request, Response)


config : List Config.Configuration
config =
    Config.configuration


type Msg
    = NoOp
    | NewRequest Http.Id


init =
    ( (), Cmd.none )


update msg _ =
    case msg of
        NewRequest id ->
            ( (), Http.send (Http.textResponse Http.okStatus "Hello World" id) )

        NoOp ->
            ( (), Cmd.none )


routeRequest incoming =
    case incoming of
        Ok request ->
            NewRequest request.id

        _ ->
            NoOp


subscriptions _ =
    Sub.batch
        [ Http.listen routeRequest
        ]


main =
    Platform.program
        { init = init
        , update = update
        , subscriptions = subscriptions
        }
