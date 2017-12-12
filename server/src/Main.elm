module Main exposing (main)

import Config
import Dict exposing (Dict)
import Erl
import OAuthMiddleware
import Platform
import Server.Http as Http exposing (Request, Response)


config : List Config.Configuration
config =
    Config.configuration


type alias ClientInfo =
    { clientId : String
    , clientSecret : String
    }


type alias RedirectDict =
    Dict String ClientInfo


type alias TokenDict =
    Dict String String


addConfigToDict : Config.Configuration -> RedirectDict -> RedirectDict
addConfigToDict config dict =
    let
        info =
            { clientId = config.clientId
            , clientSecret = config.clientSecret
            }
    in
    List.foldl
        (\host dict ->
            Dict.insert host info dict
        )
        dict
        config.redirectBackHosts


buildRedirectDict : List Config.Configuration -> RedirectDict
buildRedirectDict configs =
    List.foldl addConfigToDict Dict.empty configs


type alias Model =
    { redirectDict : RedirectDict
    , tokenDict : TokenDict
    }


type Msg
    = NoOp
    | NewRequest Http.Request


init : ( Model, Cmd Msg )
init =
    ( { redirectDict = buildRedirectDict config
      , tokenDict = Dict.empty
      }
    , Cmd.none
    )


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        NewRequest request ->
            newRequest request model

        NoOp ->
            ( model, Cmd.none )


{-| We expect requests of two forms:

1.  From the authorization server:
    ...?code=<long code>&state=<from OAuthMiddleware.encodeRedirectState>

2.  From OAuthMiddleWare.getToken
    ...?getToken=tokenKey

-}
newRequest : Http.Request -> Model -> ( Model, Cmd Msg )
newRequest request model =
    ( model
    , Http.send
        (Http.textResponse Http.okStatus "Hello World" request.id)
    )


routeRequest : Result String Http.Request -> Msg
routeRequest incoming =
    case incoming of
        Ok request ->
            NewRequest request

        _ ->
            NoOp


subscriptions : Model -> Sub Msg
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
