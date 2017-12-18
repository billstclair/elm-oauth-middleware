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

import Html
    exposing
        ( Attribute
        , Html
        , a
        , br
        , button
        , div
        , h2
        , input
        , option
        , p
        , pre
        , select
        , span
        , table
        , td
        , text
        , textarea
        , th
        , tr
        )
import Html.Attributes
    exposing
        ( checked
        , cols
        , disabled
        , href
        , name
        , rows
        , selected
        , size
        , style
        , target
        , type_
        , value
        )
import Html.Events exposing (on, onClick, onInput, targetValue)
import Http
import Json.Decode as JD
import Json.Encode as JE exposing (Value)
import Navigation exposing (Location)
import OAuthMiddleware
    exposing
        ( Authorization
        , ResponseToken
        , TokenState(..)
        , authorize
        , locationToRedirectBackUri
        , receiveTokenAndState
        , use
        )
import OAuthMiddleware.EncodeDecode exposing (responseTokenEncoder)


type alias Model =
    { token : Maybe ResponseToken
    , state : Maybe String
    , msg : Maybe String
    , reply : Maybe Value
    , redirectBackUri : String
    , provider : Provider
    }


type Msg
    = ReceiveLocation Location
    | Login
    | GetUser
    | ReceiveUser (Result Http.Error Value)


userAgentHeader : Http.Header
userAgentHeader =
    Http.header "User-Agent" "Xossbow"


gitHubAuthorization : Authorization
gitHubAuthorization =
    { authorizationUri = "https://github.com/login/oauth/authorize"
    , tokenUri = "https://github.com/login/oauth/access_token"
    , clientId = "ab3259e23daac66c0952"
    , redirectUri = "https://xossbow.com/oath/github/"
    , scope = [ "user" ]
    , state = Just "Vermont"
    , redirectBackUri = "" --filled in by init
    }


gmailAuthorization : Authorization
gmailAuthorization =
    { authorizationUri = "https://accounts.google.com/o/oauth2/auth"
    , tokenUri = "https://accounts.google.com/o/oauth2/token"
    , clientId = "488367092930-4jafco3fjmf4voiv6n3nu4v9s4uv3n5u.apps.googleusercontent.com"
    , redirectUri = "https://xossbow.com/oath/"
    , scope = [ "https://www.googleapis.com/auth/gmail.readonly" ]
    , state = Just "Vermont"
    , redirectBackUri = "" --filled in by init
    }


gabAuthorization : Authorization
gabAuthorization =
    { authorizationUri = "https://api.gab.ai/oauth/authorize"
    , tokenUri = "https://api.gab.ai/oauth/token"
    , clientId = "4"
    , redirectUri = "https://xossbow.com/oath/gab/"
    , scope = [ "read" ]
    , state = Just "Vermont"
    , redirectBackUri = "" --filled in by init
    }


type alias Provider =
    { authorization : Authorization
    , apiUrl : String
    , profilePath : String
    }


gitHubProvider : Provider
gitHubProvider =
    { authorization = gitHubAuthorization
    , apiUrl = "https://api.github.com/"
    , profilePath = "user"
    }


gmailProvider : Provider
gmailProvider =
    { authorization = gmailAuthorization
    , apiUrl = "https://www.googleapis.com/gmail/v1/users/"
    , profilePath = "me/profile"
    }


gabProvider : Provider
gabProvider =
    { authorization = gabAuthorization
    , apiUrl = "https://api.gab.ai/v1.0/"
    , profilePath = "users/Xossbow"
    }


defaultProvider =
    gmailProvider


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
    { token = token
    , state = state
    , msg = msg
    , reply =
        case token of
            Nothing ->
                Nothing

            Just tok ->
                Just <| responseTokenEncoder tok
    , redirectBackUri = locationToRedirectBackUri location
    , provider = defaultProvider
    }
        ! []


getUser : Model -> ( Model, Cmd Msg )
getUser model =
    case model.token of
        Nothing ->
            { model | msg = Just "You must login before getting user information." }
                ! []

        Just token ->
            let
                provider =
                    model.provider

                url =
                    provider.apiUrl ++ provider.profilePath

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


{-| Getting from GitHub for login:

body = "access_token=1450d906051a45db3b2a645287f6ed379af4dc52&scope=user&token_type=bearer"

Two things, from <https://developer.github.com/apps/building-oauth-apps/authorization-options-for-oauth-apps/>

1.  Need to give it an "Accept: application/json" header.
2.  It returns "s1,s2,s3" intead of ["s1","s2","s3"] for scope

-}
update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        ReceiveLocation _ ->
            model ! []

        Login ->
            let
                provider =
                    model.provider

                auth =
                    provider.authorization

                authorization =
                    { auth | redirectBackUri = model.redirectBackUri }
            in
            model ! [ authorize authorization ]

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
                        | reply = Just reply
                        , msg = Nothing
                    }
                        ! []


view : Model -> Html Msg
view model =
    div
        [ style [ ( "margin-left", "3em" ) ]
        ]
        [ h2 [] [ text "OAuthMiddleware Example" ]
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
                    text <| JE.encode 2 reply

                _ ->
                    text "Nothing to report"
            ]
        ]
