module Generated.UserAPI exposing (..)

-- The following module comes from bartavelle/json-helpers

import Http
import Json.Decode
import Json.Encode exposing (Value)
import Json.Helpers exposing (..)
import Url.Builder


type alias User =
    { name : String
    , age : Int
    , email : String
    }


jsonDecUser : Json.Decode.Decoder User
jsonDecUser =
    Json.Decode.succeed (\pname page pemail -> { name = pname, age = page, email = pemail })
        |> required "name" Json.Decode.string
        |> required "age" Json.Decode.int
        |> required "email" Json.Decode.string


jsonEncUser : User -> Value
jsonEncUser val =
    Json.Encode.object
        [ ( "name", Json.Encode.string val.name )
        , ( "age", Json.Encode.int val.age )
        , ( "email", Json.Encode.string val.email )
        ]


getUsers : (Result Http.Error (List User) -> msg) -> Cmd msg
getUsers toMsg =
    let
        params =
            List.filterMap identity
                (List.concat
                    []
                )
    in
    Http.request
        { method =
            "GET"
        , headers =
            []
        , url =
            Url.Builder.crossOrigin "http://localhost:8081"
                [ "users"
                ]
                params
        , body =
            Http.emptyBody
        , expect =
            Http.expectJson toMsg (Json.Decode.list jsonDecUser)
        , timeout =
            Nothing
        , tracker =
            Nothing
        }


postUsers : User -> (Result Http.Error () -> msg) -> Cmd msg
postUsers body toMsg =
    let
        params =
            List.filterMap identity
                (List.concat
                    []
                )
    in
    Http.request
        { method =
            "POST"
        , headers =
            []
        , url =
            Url.Builder.crossOrigin "http://localhost:8081"
                [ "users"
                ]
                params
        , body =
            Http.jsonBody (jsonEncUser body)
        , expect =
            Http.expectString
                (\x ->
                    case x of
                        Err e ->
                            toMsg (Err e)

                        Ok _ ->
                            toMsg (Ok ())
                )
        , timeout =
            Nothing
        , tracker =
            Nothing
        }
