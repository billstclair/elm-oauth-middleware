module Tests exposing (all)

import Dict
import Expect exposing (Expectation)
import Json.Decode as JD exposing (Decoder, Value)
import List
import Maybe exposing (withDefault)
import OAuth exposing (Token)
import OAuthMiddleware exposing (Authorization)
import OAuthMiddleware.EncodeDecode as ED
    exposing
        ( RedirectState
        , ResponseTokenError
        )
import OAuthMiddleware.ResponseToken exposing (ResponseToken)
import OAuthMiddleware.ServerConfiguration as SC
    exposing
        ( Configurations
        , RedirectBackHost
        )
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
            , List.map doEncodeTest configurationTestData
            , List.map doSbWasTest redirectBackHostTestData
            , List.map doSbWasTest decodeConfigurationsTestData
            , List.map doSbWasTest decodeAuthorizationsTestData
            ]


expectResult : Result err a -> Result err a -> Expectation
expectResult sb was =
    case maybeLog "  result" was of
        Err err ->
            case sb of
                Err _ ->
                    Expect.true "You shouldn't ever see this." True

                Ok _ ->
                    Expect.false (Debug.toString err) True

        Ok wasv ->
            case sb of
                Err _ ->
                    Expect.false "Expected an error but didn't get one." True

                Ok sbv ->
                    Expect.equal sbv wasv


doSbWasTest : ( String, Result err a, Result err a ) -> Test
doSbWasTest ( name, was, sb ) =
    test name
        (\_ ->
            expectResult sb was
        )


doEncodeTest : ( String, a -> Result String a, a ) -> Test
doEncodeTest ( name, ed, a ) =
    test name
        (\_ ->
            expectResult (Ok a) (ed a)
        )


decodeValue : Decoder a -> Value -> Result String a
decodeValue decoder value =
    case JD.decodeValue decoder value of
        Ok a ->
            Ok a

        Err error ->
            Err <| JD.errorToString error


decodeString : Decoder a -> String -> Result String a
decodeString decoder string =
    case JD.decodeString decoder string of
        Ok a ->
            Ok a

        Err error ->
            Err <| JD.errorToString error


encodeDecode : (a -> Value) -> JD.Decoder a -> a -> Result String a
encodeDecode encoder decoder a =
    encoder a
        -- |> Debug.log "value"
        |> decodeValue decoder


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


encodeDecodeConfigurations : Configurations -> Result String Configurations
encodeDecodeConfigurations =
    encodeDecode SC.configurationsEncoder SC.configurationsDecoder


insertEncodeDecode : (a -> Result String a) -> ( String, a ) -> ( String, a -> Result String a, a )
insertEncodeDecode ed ( name, data ) =
    ( name, ed, data )


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


bearer : String -> Maybe Token
bearer str =
    OAuth.tokenFromString <| "Bearer " ++ str


responseTokenTestData : List ( String, ResponseToken -> Result String ResponseToken, ResponseToken )
responseTokenTestData =
    case ( bearer "foo", bearer "bar", bearer "bletch" ) of
        ( Just foo, Just bar, Just bletch ) ->
            [ ( "ResponseToken 1"
              , { expiresIn = Nothing
                , refreshToken = Nothing
                , scope = []
                , state = Nothing
                , token = foo
                }
              )
            , ( "ResponseToken 2"
              , { expiresIn = Just 1000
                , refreshToken = Just bar
                , scope = [ "read", "write" ]
                , state = Just "What would yomama say?"
                , token = bletch
                }
              )
            ]
                |> List.map (insertEncodeDecode encodeDecodeResponseToken)

        _ ->
            Debug.todo "Can't make tokens"


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
        , apiUri = "apiuri"
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


configurationTestData : List ( String, Configurations -> Result String Configurations, Configurations )
configurationTestData =
    [ ( "Configurations"
      , { local =
            { httpPort = 3001
            , configSamplePeriod = 3
            }
        , remote =
            [ { tokenUri = "https://example.com/oath/token"
              , clientId = "clientid"
              , clientSecret = "secret"
              , redirectBackHosts =
                    [ { host = "myexample.com", ssl = True }
                    , { host = "oauth-client-dev.com", ssl = False }
                    ]
              }
            , { tokenUri = "https://example2.com/oath/token"
              , clientId = "clientid2"
              , clientSecret = "secret2"
              , redirectBackHosts =
                    [ { host = "myexample2.com", ssl = True }
                    , { host = "oauth-client-dev.com", ssl = False }
                    ]
              }
            ]
        }
      )
    ]
        |> List.map (insertEncodeDecode encodeDecodeConfigurations)


decodeRedirectBackHost : String -> Result String RedirectBackHost
decodeRedirectBackHost json =
    decodeString SC.redirectBackHostDecoder json


redirectBackHostTestData : List ( String, Result String RedirectBackHost, Result String RedirectBackHost )
redirectBackHostTestData =
    [ ( "redirectBackHost 1"
      , decodeRedirectBackHost "\"https://example.com\""
      , Ok { host = "example.com", ssl = True }
      )
    , ( "redirectBackHost 2"
      , decodeRedirectBackHost "\"example.com\""
      , Ok { host = "example.com", ssl = False }
      )
    , ( "redirectBackHost 3"
      , decodeRedirectBackHost "\"http://example.com\""
      , Ok { host = "example.com", ssl = False }
      )
    ]


decodeConfigurations : String -> Result String Configurations
decodeConfigurations json =
    decodeString SC.configurationsDecoder json


decodeConfigurationsTestData : List ( String, Result String Configurations, Result String Configurations )
decodeConfigurationsTestData =
    [ ( "decodeConfigurations 1"
      , decodeConfigurations
            """
           [ {"comment": 1,
              "port": 3000
             },
             {"port": 3001}
           ]
          """
      , Ok
            { local =
                { httpPort = 3001
                , configSamplePeriod = 2
                }
            , remote = []
            }
      )
    , ( "decodeConfigurations 2"
      , decodeConfigurations
            """
           [ {"configSamplePeriod": 0}
           ]
          """
      , Ok
            { local =
                { httpPort = 3000
                , configSamplePeriod = 0
                }
            , remote = []
            }
      )
    , ( "decodeConfigurations 3"
        -- Multiple local configurations
      , decodeConfigurations
            """
           [ {"port": 3000 }
           , {"configSamplePeriod": 3}
           ]
           """
      , Err "error messages are not compared"
      )
    , ( "decodeConfigurations 4"
        -- misspelled "clientId"
      , decodeConfigurations
            """
           [ {"tokenUri": "https://example.com/oath/token",
             "clientID": "clientid",
             "clientSecret": "secret",
             "redirectBackHosts": ["https://example.com", "oauth-client-dev.com"]
           ]
           """
      , Err "error messages are ignored."
      )
    ]


decodeAuthorizations : String -> Result String (List Authorization)
decodeAuthorizations json =
    decodeString ED.authorizationsDecoder json


decodeAuthorizationsTestData : List ( String, Result String (List Authorization), Result String (List Authorization) )
decodeAuthorizationsTestData =
    [ ( "decodeAuthorizations"
      , decodeAuthorizations
            """
           [ { "name": "GitHub",
               "authorizationUri": "https://github.com/login/oauth/authorize",
               "tokenUri": "https://github.com/login/oauth/access_token",
               "apiUri": "https://api.github.com/",
               "clientId": "xxx",
               "redirectUri": "https://example.com/oath/",
               "scopes": {"user": "user"}
             },
             { "comment": "Not using this yet",
               "name": "Gmail",
               "authorizationUri": "https://accounts.google.com/o/oauth2/auth",
               "apiUri": "https://www.googleapis.com/gmail/v1/users/",
               "tokenUri": "https://accounts.google.com/o/oauth2/token",
               "clientId": "yyy.apps.googleusercontent.com",
               "redirectUri": "https://example.com/oath/",
               "scopes": {"readonly": "https://www.googleapis.com/auth/gmail.readonly"}
             }
           ]
           """
      , Ok
            [ { name = "GitHub"
              , authorizationUri = "https://github.com/login/oauth/authorize"
              , tokenUri = "https://github.com/login/oauth/access_token"
              , apiUri = "https://api.github.com/"
              , clientId = "xxx"
              , redirectUri = "https://example.com/oath/"
              , scopes = Dict.fromList [ ( "user", "user" ) ]
              }
            ]
      )
    ]
