module Main exposing (..)

import Browser
import Generated.UserAPI exposing (User, getUsers)
import Html exposing (Html, div, h1, li, text, ul)
import Http



---- MAIN ----


main : Program () Model Msg
main =
    Browser.element
        { view = view
        , init = \_ -> init
        , update = update
        , subscriptions = always Sub.none
        }



---- MODEL ----
-- The users returned from our API


type alias Model =
    { users : List User }



-- initially call the backend to fetch users, using the generated function


init : ( Model, Cmd Msg )
init =
    ( { users = [] }, getUsers GotUsers )



---- UPDATE ----
-- Our result message for using the genrated function


type Msg
    = GotUsers (Result Http.Error (List User))


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        GotUsers (Ok users) ->
            ( { users = users }, Cmd.none )

        _ ->
            ( model, Cmd.none )



---- VIEW ----


view : Model -> Html Msg
view model =
    div []
        [ h1 [] [ text "Your Elm App is working!" ]
        , ul [] (List.map viewUsersList model.users)
        ]


viewUsersList : User -> Html msg
viewUsersList user =
    li []
        [ text " Name: "
        , text user.name
        , text ", Age: "
        , text (String.fromInt user.age)
        , text ", Email: "
        , text user.email
        ]
