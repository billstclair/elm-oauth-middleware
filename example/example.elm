----------------------------------------------------------------------
--
-- GitHubExample.elm
-- Example of using OAuthMiddleware for GitHub.
-- Copyright (c) 2017 Bill St. Clair <billstclair@gmail.com>
-- Some rights reserved.
-- Distributed under the MIT License
-- See LICENSE.txt
--
----------------------------------------------------------------------


module Main exposing (..)

import Dict exposing (Dict)
import Html
    exposing
        ( Attribute
        , Html
        , button
        , div
        , h2
        , option
        , p
        , pre
        , select
        , text
        )
import Html.Attributes
    exposing
        ( selected
        , style
        , value
        )
import Html.Events exposing (onClick, onInput)
import Http
import Json.Decode as JD
import Json.Encode as JE exposing (Value)
import Navigation exposing (Location)
import OAuthMiddleware
    exposing
        ( Authorization
        , ResponseToken
        , TokenAuthorization
        , TokenState(..)
        , authorize
        , getAuthorizations
        , locationToRedirectBackUri
        , receiveTokenAndState
        , use
        )
import OAuthMiddleware.EncodeDecode
    exposing
        ( authorizationsEncoder
        , responseTokenEncoder
        )


type alias Model =
    { authorization : Maybe Authorization
    , token : Maybe ResponseToken
    , state : Maybe String
    , msg : Maybe String
    , replyType : String
    , reply : Maybe Value
    , redirectBackUri : String
    , provider : String
    , authorizations : Dict String Authorization
    , tokenAuthorization : Maybe TokenAuthorization
    , api : Maybe Api
    }


type Msg
    = ReceiveLocation Location
    | ReceiveAuthorizations (Result Http.Error (List Authorization))
    | ChangeProvider String
    | Login
    | GetUser
    | ReceiveUser (Result Http.Error Value)


userAgentHeader : Http.Header
userAgentHeader =
    Http.header "User-Agent" "Xossbow"


type alias Api =
    { getUser : String
    }


apis : Dict String Api
apis =
    Dict.fromList
        [ ( "GitHub"
          , { getUser = "user"
            }
          )
        , ( "Gmail"
          , { getUser = "me/profile"
            }
          )
        , ( "Facebook"
          , { getUser = "me"
            }
          )
        , ( "Gab"
          , { getUser = "me" -- "users/{username}"
            }
          )
        ]


main =
    Navigation.program
        ReceiveLocation
        { init = init
        , update = update
        , view = view
        , subscriptions = \_ -> Sub.none
        }


init : Location -> ( Model, Cmd Msg )
init location =
    let
        ( token, state, msg ) =
            case receiveTokenAndState location of
                TokenAndState tok stat ->
                    ( Just tok, stat, Nothing )

                TokenErrorAndState m stat ->
                    ( Nothing, stat, Just m )

                TokenDecodeError m ->
                    ( Nothing, Nothing, Just m )

                NoToken ->
                    ( Nothing, Nothing, Nothing )
    in
    { authorization = Nothing
    , token = token
    , state = state
    , msg = msg
    , replyType = "Token"
    , reply =
        case token of
            Nothing ->
                Nothing

            Just tok ->
                Just <| responseTokenEncoder tok
    , redirectBackUri = locationToRedirectBackUri location
    , authorizations = Dict.empty
    , provider =
        case state of
            Just p ->
                p

            Nothing ->
                "Gmail"
    , tokenAuthorization = Nothing
    , api = Nothing
    }
        ! [ Http.send ReceiveAuthorizations <|
                getAuthorizations False "authorizations.json"
          , Navigation.modifyUrl "#"
          ]


getUser : Model -> ( Model, Cmd Msg )
getUser model =
    case model.token of
        Nothing ->
            { model
                | msg = Just "You must login before getting user information."
            }
                ! []

        Just token ->
            case ( model.api, model.authorization ) of
                ( Just api, Just auth ) ->
                    let
                        url =
                            auth.apiUri ++ api.getUser

                        req =
                            Http.request
                                { method = "GET"
                                , headers = use token [ userAgentHeader ]
                                , url = url
                                , body = Http.emptyBody
                                , expect = Http.expectJson JD.value
                                , timeout = Nothing
                                , withCredentials = False
                                }
                    in
                    model ! [ Http.send ReceiveUser req ]

                _ ->
                    { model | msg = Just "No known API." }
                        ! []


lookupProvider : Model -> Model
lookupProvider model =
    let
        authorization =
            case Dict.get model.provider model.authorizations of
                Nothing ->
                    case List.head <| Dict.toList model.authorizations of
                        Nothing ->
                            Nothing

                        Just ( _, auth ) ->
                            Just auth

                Just auth ->
                    Just auth
    in
    case authorization of
        Nothing ->
            model

        Just auth ->
            case List.head <| Dict.toList auth.scopes of
                Nothing ->
                    model

                Just ( _, scope ) ->
                    let
                        provider =
                            auth.name

                        api =
                            Dict.get provider apis
                    in
                    { model
                        | provider = provider
                        , tokenAuthorization =
                            Just
                                { authorization = auth
                                , scope = [ scope ]
                                , state = Just model.provider
                                , redirectBackUri = model.redirectBackUri
                                }
                        , api = api
                        , authorization = authorization
                    }


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        ReceiveLocation _ ->
            model ! []

        ReceiveAuthorizations result ->
            case result of
                Err err ->
                    { model | msg = Just <| toString err }
                        ! []

                Ok authorizations ->
                    let
                        ( replyType, reply ) =
                            case ( model.token, model.msg ) of
                                ( Nothing, Nothing ) ->
                                    ( "Authorizations"
                                    , Just <|
                                        authorizationsEncoder
                                            authorizations
                                    )

                                _ ->
                                    ( model.replyType, model.reply )
                    in
                    lookupProvider
                        { model
                            | authorizations =
                                Dict.fromList <|
                                    List.map (\a -> ( a.name, a )) authorizations
                            , replyType = replyType
                            , reply = reply
                        }
                        ! []

        ChangeProvider provider ->
            lookupProvider
                { model | provider = provider }
                ! []

        Login ->
            case model.tokenAuthorization of
                Nothing ->
                    { model | msg = Just "No provider selected." }
                        ! []

                Just authorization ->
                    model
                        ! [ authorize authorization ]

        GetUser ->
            getUser model

        ReceiveUser result ->
            case result of
                Err err ->
                    { model
                        | reply = Nothing
                        , msg = Just <| toString err
                    }
                        ! []

                Ok reply ->
                    { model
                        | replyType = "API Response"
                        , reply = Just reply
                        , msg = Nothing
                    }
                        ! []


providerOption : String -> String -> Html Msg
providerOption currentProvider provider =
    option
        [ value provider
        , selected <| provider == currentProvider
        ]
        [ text provider ]


providerSelect : Model -> Html Msg
providerSelect model =
    select [ onInput ChangeProvider ]
        (Dict.toList model.authorizations
            |> List.map Tuple.second
            |> List.map .name
            |> List.map (providerOption model.provider)
        )


view : Model -> Html Msg
view model =
    div
        [ style [ ( "margin-left", "3em" ) ]
        ]
        [ h2 [] [ text "OAuthMiddleware Example" ]
        , p []
            [ text "Provider: "
            , providerSelect model
            ]
        , p []
            [ button [ onClick Login ]
                [ text "Login" ]
            , text " "
            , button [ onClick GetUser ]
                [ text "Get User" ]
            ]
        , pre []
            [ case ( model.msg, model.reply ) of
                ( Just msg, _ ) ->
                    text <| toString msg

                ( _, Just reply ) ->
                    text <| model.replyType ++ ":\n" ++ JE.encode 2 reply

                _ ->
                    text "Nothing to report"
            ]
        ]
