module Main exposing (..)

import Browser
import Generated.UserAPI exposing (User, getUsers, postUsers)
import Html exposing (Html, button, div, h1, input, li, text, ul)
import Html.Attributes exposing (autocomplete, placeholder, type_, value)
import Html.Events exposing (onClick, onInput)
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
    { users : List User, form : Form }


type alias Form =
    { name : String
    , age : String
    , email : String
    }



-- initially call the backend to fetch users, using the generated function


init : ( Model, Cmd Msg )
init =
    ( { users = []
      , form = { name = "", age = "", email = "" }
      }
    , getUsers GotUsers
    )



---- UPDATE ----
-- Our result message for using the genrated function


type Msg
    = GotUsers (Result Http.Error (List User))
    | SavedUser (Result Http.Error ())
    | SaveUser
    | Name String
    | Age String
    | Email String


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        Name name ->
            ( { model | form = (\form -> { form | name = name }) model.form }, Cmd.none )

        Age age ->
            ( { model | form = (\form -> { form | age = age }) model.form }, Cmd.none )

        Email email ->
            ( { model | form = (\form -> { form | email = email }) model.form }, Cmd.none )

        SaveUser ->
            let
                inputData =
                    { name = model.form.name
                    , age = Maybe.withDefault 0 (String.toInt model.form.age)
                    , email = model.form.email
                    }
            in
            ( model, postUsers inputData SavedUser )

        SavedUser result ->
            case result of
                Ok _ ->
                    ( model, getUsers GotUsers )

                Err _ ->
                    ( model, getUsers GotUsers )

        GotUsers result ->
            case result of
                Ok users ->
                    ( { model | users = users }, Cmd.none )

                Err _ ->
                    ( model, Cmd.none )



---- VIEW ----


view : Model -> Html Msg
view model =
    div []
        [ h1 [] [ text "Your Elm App is working!" ]
        , ul [] (List.map viewUsersList model.users)
        , viewInput "text" "Name" model.form.name Name
        , viewInput "text" "Age" model.form.age Age
        , viewInput "text" "Email" model.form.email Email
        , button [ onClick SaveUser ] [ text "Save" ]
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


viewInput : String -> String -> String -> (String -> msg) -> Html msg
viewInput t p v toMsg =
    input [ type_ t, placeholder p, value v, onInput toMsg, autocomplete False ] []
