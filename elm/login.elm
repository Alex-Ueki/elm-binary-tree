module Login exposing (main)

import Browser
import Browser.Navigation
import Html
import Html.Attributes as Attributes
import Html.Events as Events
import Http
import Json.Decode
import Json.Encode


main : Program () Model Msg
main =
    Browser.element
        { init = init
        , view = view
        , update = update
        , subscriptions = subscriptions
        }


type alias Model =
    { username : String
    , response : Maybe String
    }


init : () -> ( Model, Cmd Msg )
init _ =
    ( { username = "Alex", response = Nothing }, Cmd.none )


type Msg
    = SubmitForm
    | SetUsername String
    | Response (Result Http.Error String)


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        SubmitForm ->
            ( { model | response = Nothing }, login model )

        SetUsername username ->
            ( { model | username = username }, Cmd.none )

        Response (Ok response) ->
            ( { model | response = Just response }, Browser.Navigation.load response )

        Response (Err error) ->
            case error of
                Http.BadUrl httpMsg ->
                    ( { model | response = Just httpMsg }, Cmd.none )

                Http.Timeout ->
                    ( { model | response = Just "It took too long to get a response." }
                    , Cmd.none
                    )

                Http.NetworkError ->
                    ( { model | response = Just "Encountered a network error." }
                    , Cmd.none
                    )

                Http.BadStatus responseRecord ->
                    ( { model | response = String.fromInt responseRecord.status.code |> Just }
                    , Cmd.none
                    )

                Http.BadPayload debug responseRecord ->
                    ( { model | response = Just debug }
                    , Cmd.none
                    )



-- Http.send: (Result Error a -> msg) -> Request a -> Cmd msg
-- Response: (Result Http.Error String) -> Msg
-- Http.send Response: Request String -> Cmd Msg


sendUsername : Http.Request String -> Cmd Msg
sendUsername =
    Http.send Response



-- Http.post: String (url) -> Body -> Decoder a -> Request a
-- Json.Encode.string: String -> Value
-- Http.jsonBody: Value -> Body
-- Json.Encode.string Http.jsonBody: String -> Body
-- (Json.Encode.string model.username) Http.jsonBody: Body


jsonUsername : Model -> Http.Body
jsonUsername model =
    Json.Encode.string model.username |> Http.jsonBody



-- Json.Decode.string: Decoder String


makeUsernameRequest : Model -> Http.Request String
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


view : Model -> Html.Html Msg
view model =
    Html.div []
        [ Html.input [ Attributes.placeholder "username", Events.onInput SetUsername ] []
        , Html.button [ Events.onClick SubmitForm ] [ Html.text "Login" ]
        ]
