module Tests exposing (all)

import Dict
import Expect exposing (Expectation)
import Json.Decode as JD exposing (Decoder, Value)
import List
import Maybe exposing (withDefault)
import OAuth exposing (ResponseToken, Token(..))
import OAuthMiddleware exposing (Authorization)
import OAuthMiddleware.EncodeDecode as ED exposing (RedirectState, ResponseTokenError)
import Test exposing (..)


log =
    Debug.log


{-| change to True to log JSON input & output results
-}
enableLogging : Bool
enableLogging =
    False


maybeLog : String -> a -> a
maybeLog label value =
    if enableLogging then
        log label value
    else
        value


all : Test
all =
    Test.concat <|
        List.concat
            [ List.map doEncodeTest redirectStateTestData
            , List.map doEncodeTest responseTokenTestData
            , List.map doEncodeTest responseTokenErrorTestData
            , List.map doEncodeTest authorizationTestData
            ]


expectResult : Result err a -> Result err a -> Expectation
expectResult sb was =
    case maybeLog "  result" was of
        Err err ->
            case sb of
                Err _ ->
                    Expect.true "You shouldn't ever see this." True

                Ok _ ->
                    Expect.false (toString err) True

        Ok wasv ->
            case sb of
                Err _ ->
                    Expect.false "Expected an error but didn't get one." True

                Ok sbv ->
                    Expect.equal sbv wasv


doEncodeTest : ( String, a -> Result String a, a ) -> Test
doEncodeTest ( name, encodeDecode, a ) =
    test name
        (\_ ->
            expectResult (Ok a) (encodeDecode a)
        )


encodeDecode : (a -> Value) -> JD.Decoder a -> a -> Result String a
encodeDecode encoder decoder a =
    encoder a
        -- |> Debug.log "value"
        |> JD.decodeValue decoder


encodeDecodeRedirectState : RedirectState -> Result String RedirectState
encodeDecodeRedirectState =
    encodeDecode ED.redirectStateEncoder ED.redirectStateDecoder


encodeDecodeResponseToken : ResponseToken -> Result String ResponseToken
encodeDecodeResponseToken =
    encodeDecode ED.responseTokenEncoder ED.responseTokenDecoder


encodeDecodeResponseTokenError : ResponseTokenError -> Result String ResponseTokenError
encodeDecodeResponseTokenError =
    encodeDecode ED.responseTokenErrorEncoder ED.responseTokenErrorDecoder


encodeDecodeAuthorization : Authorization -> Result String Authorization
encodeDecodeAuthorization =
    encodeDecode ED.authorizationEncoder ED.authorizationDecoder


insertEncodeDecode : (a -> Result String a) -> ( String, a ) -> ( String, a -> Result String a, a )
insertEncodeDecode encodeDecode ( name, data ) =
    ( name, encodeDecode, data )


redirectStateTestData : List ( String, RedirectState -> Result String RedirectState, RedirectState )
redirectStateTestData =
    [ ( "RedirectState"
      , { clientId = "foo"
        , tokenUri = "https://api.gab.ai/oauth/token"
        , redirectUri = "https://xossbow.com/oath/xossbow/"
        , scope = [ "read", "write" ]
        , redirectBackUri = "http://xossbow-devtest.com"
        , state = Just "foo"
        }
      )
    ]
        |> List.map (insertEncodeDecode encodeDecodeRedirectState)


responseTokenTestData : List ( String, ResponseToken -> Result String ResponseToken, ResponseToken )
responseTokenTestData =
    [ ( "ResponseToken 1"
      , { expiresIn = Nothing
        , refreshToken = Nothing
        , scope = []
        , state = Nothing
        , token = Bearer "foo"
        }
      )
    , ( "ResponseToken 2"
      , { expiresIn = Just 1000
        , refreshToken = Just <| Bearer "bar"
        , scope = [ "read", "write" ]
        , state = Just "What would yomama say?"
        , token = Bearer "bletch"
        }
      )
    ]
        |> List.map (insertEncodeDecode encodeDecodeResponseToken)


responseTokenErrorTestData : List ( String, ResponseTokenError -> Result String ResponseTokenError, ResponseTokenError )
responseTokenErrorTestData =
    [ ( "ResponseTokenError 1"
      , { err = "foo"
        , state = Just "Vermont"
        }
      )
    , ( "ResponseTokenError 2"
      , { err = "bar"
        , state = Nothing
        }
      )
    ]
        |> List.map (insertEncodeDecode encodeDecodeResponseTokenError)


authorizationTestData : List ( String, Authorization -> Result String Authorization, Authorization )
authorizationTestData =
    [ ( "Authorization"
      , { name = "Gmail"
        , authorizationUri = "authorizationuri"
        , tokenUri = "tokenuri"
        , clientId = "clientid"
        , redirectUri = "redirecturi"
        , scopes =
            Dict.fromList
                [ ( "name1", "scope1" )
                , ( "name2", "scope2" )
                ]
        }
      )
    ]
        |> List.map (insertEncodeDecode encodeDecodeAuthorization)
