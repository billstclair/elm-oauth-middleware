----------------------------------------------------------------------
--
-- Authenticate.elm
-- Code to request a token from the OAuth token server.
-- Copied from elm-oauth2's Internal.elm, so I can accomodate
-- GitHub's non-conformance with the OAuth spec.
-- Changes marked with "GitHub" in comments.
-- Copyright (c) 2017 TruQu, copied and modified under The MIT License.
-- Some rights reserved.
-- Distributed under the MIT License
-- See LICENSE.txt
--
----------------------------------------------------------------------


module OAuthTokenServer.Authenticate exposing (authenticate)

import Base64
import Http as Http
import Json.Decode as Json
import OAuth exposing (Credentials, ResponseToken, Token(..))
import OAuthMiddleware.EncodeDecode exposing (responseTokenDecoder)
import QueryString as QS


type alias Authentication =
    { credentials : Credentials
    , code : String
    , redirectUri : String
    , scope : List String
    , state : Maybe String
    , url : String
    }


authenticate : Authentication -> Http.Request ResponseToken
authenticate authentication =
    let
        { credentials, code, redirectUri, scope, state, url } =
            authentication

        body =
            QS.empty
                |> QS.add "grant_type" "authorization_code"
                |> QS.add "client_id" credentials.clientId
                |> QS.add "redirect_uri" redirectUri
                |> QS.add "code" code
                |> qsAddList "scope" scope
                |> qsAddMaybe "state" state
                |> QS.render
                |> String.dropLeft 1

        headers =
            authHeader <|
                if String.isEmpty credentials.secret then
                    Nothing
                else
                    Just credentials
    in
    makeRequest url headers body


authHeader : Maybe Credentials -> List Http.Header
authHeader credentials =
    credentials
        |> Maybe.map (\{ clientId, secret } -> Base64.encode (clientId ++ ":" ++ secret))
        |> Maybe.andThen Result.toMaybe
        |> Maybe.map (\s -> [ Http.header "Authorization" ("Basic " ++ s) ])
        |> Maybe.withDefault []
        -- GitHub requires this header, or it URL-encodes the result
        |> (::) (Http.header "Accept" "application/json")


makeRequest : String -> List Http.Header -> String -> Http.Request ResponseToken
makeRequest url headers body =
    Http.request
        { method = "POST"
        , headers = headers
        , url = url
        , body = Http.stringBody "application/x-www-form-urlencoded" body
        , expect = Http.expectJson responseTokenDecoder
        , timeout = Nothing
        , withCredentials = False
        }


qsAddList : String -> List String -> QS.QueryString -> QS.QueryString
qsAddList param xs qs =
    case xs of
        [] ->
            qs

        _ ->
            QS.add param (String.join " " xs) qs


qsAddMaybe : String -> Maybe String -> QS.QueryString -> QS.QueryString
qsAddMaybe param ms qs =
    case ms of
        Nothing ->
            qs

        Just s ->
            QS.add param s qs
