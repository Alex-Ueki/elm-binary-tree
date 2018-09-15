module Login exposing (Model, Msg(..), init, jsonUsername, login, main, makeUsernameRequest, sendUsername, subscriptions, update, view)

import Html exposing (Attribute, Html, button, div, input, text)
import Html.Attributes exposing (..)
import Html.Events exposing (onClick, onInput)
import Http exposing (..)
import Json.Decode exposing (..)
import Json.Encode exposing (..)
import Navigation exposing (..)


main : Program Never Model Msg
main =
    Html.program
        { init = ( init, Cmd.none )
        , view = view
        , update = update
        , subscriptions = subscriptions
        }


type alias Model =
    { username : String
    , response : Maybe String
    }


init : Model
init =
    { username = "Alex", response = Nothing }


type Msg
    = NoOp
    | SubmitForm
    | SetUsername String
    | Response (Result Http.Error String)


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        NoOp ->
            ( model, Cmd.none )

        SubmitForm ->
            ( { model | response = Nothing }, login model )

        SetUsername username ->
            ( { model | username = username }, Cmd.none )

        Response (Ok response) ->
            ( { model | response = Just response }, Navigation.load response )

        Response (Err error) ->
            ( { model | response = Just (toString error) }, Cmd.none )



-- Http.send: (Result Error a -> msg) -> Request a -> Cmd msg
-- Response: (Result Http.Error String) -> Msg
-- Http.send Response: Request String -> Cmd Msg


sendUsername : Request String -> Cmd Msg
sendUsername =
    Http.send Response



-- Http.post: String (url) -> Body -> Decoder a -> Request a
-- Json.Encode.string: String -> Value
-- Http.jsonBody: Value -> Body
-- Json.Encode.string Http.jsonBody: String -> Body
-- (Json.Encode.string model.username) Http.jsonBody: Body


jsonUsername : Model -> Body
jsonUsername model =
    Json.Encode.string model.username |> Http.jsonBody



-- Json.Decode.string: Decoder String


makeUsernameRequest : Model -> Request String
makeUsernameRequest model =
    Http.post "login" (jsonUsername model) Json.Decode.string


login : Model -> Cmd Msg
login model =
    makeUsernameRequest model |> sendUsername



-- SUBSCRIPTIONS


subscriptions : Model -> Sub Msg
subscriptions model =
    Sub.none



-- VIEWS


view : Model -> Html Msg
view model =
    div []
        [ div []
            [ input [ placeholder "username", onInput SetUsername ] []
            , button [ onClick SubmitForm ] [ text "Login" ]
            ]
        , div [] [ text (toString model) ]
        ]
