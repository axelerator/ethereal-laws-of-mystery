module Home exposing (Model, Msg, fromBackend, init, update, view)

import Hades exposing (ToBackend(..), ToBackendEnvelope(..), ToFrontend(..), toBackendEnvelopeEncoder)
import Html exposing (button, div, text)
import Html.Events exposing (onClick)
import Http exposing (jsonBody)
import Json.Encode as Encode
import WebAuthn exposing (Msg)
import Html exposing (br)
import Hades exposing (RealmId(..))


type alias Model =
    { counter : Int }


send : ToBackend -> Cmd Msg
send msg =
    Http.post
        { url = "/send"
        , body = jsonBody <| toBackendEnvelopeEncoder <| ForRealm Lobby msg
        , expect = Http.expectWhatever GotSendResponse
        }


fromBackend : ToFrontend -> Msg
fromBackend toFrontend =
    FromBackend toFrontend


type Msg
    = Moar
    | Less
    | GotSendResponse (Result Http.Error ())
    | FromBackend ToFrontend


init =
    { counter = 0 }


updateFromRealm toFrontend model =
    model


update { webauthn } msg model =
    case msg of
        FromBackend toFrontend ->
            case toFrontend of
                UpdateCounter i ->
                    ( { model | counter = i }
                    , Cmd.none
                    )

        GotSendResponse result ->
            ( model, Cmd.none )

        Moar ->
            ( model
            , send Increment
            )

        Less ->
            ( model
            , send Decrement
            )


view model =
    div []
        [ text <| String.fromInt model.counter
        , br [] []
        , button [ onClick Less ] [ text "less" ]
        , button [ onClick Moar ] [ text "moar" ]
        ]
