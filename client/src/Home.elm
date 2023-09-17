module Home exposing (Model, Msg, fromBackend, init, update, view)

import Hades exposing (RealmId(..), ToBackend(..), ToBackendEnvelope(..), ToFrontend(..), ToFrontendLobby(..), ToLobby(..), Transition(..), toBackendEnvelopeEncoder)
import Html exposing (br, button, div, text)
import Html.Attributes exposing (class)
import Html.Events exposing (onClick)
import Http exposing (jsonBody)
import Json.Encode as Encode
import WebAuthn exposing (Msg)
import Animation

type alias Model =
    { counter : Int }


send : ToLobby -> Cmd Msg
send msg =
    sendToBackend <| ForRealm Lobby <| ForLobby msg


sendToBackend : ToBackendEnvelope -> Cmd Msg
sendToBackend msg =
    Http.post
        { url = "/send"
        , body = jsonBody <| toBackendEnvelopeEncoder <| msg
        , expect = Http.expectWhatever GotSendResponse
        }


fromBackend : ToFrontendLobby -> Msg
fromBackend toFrontend =
    FromBackend toFrontend


type Msg
    = Moar
    | Less
    | GotSendResponse (Result Http.Error ())
    | FromBackend ToFrontendLobby
    | Go


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

                GameStart realmId ->
                    ( model
                    , sendToBackend <| EnterRealm realmId
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

        Go ->
            ( model
            , send StartGame
            )

view model =
    div []
        [ text <| String.fromInt model.counter
        , br [] []
        , button [ onClick Less ] [ text "less" ]
        , button [ onClick Moar ] [ text "moar" ]
        , button [ onClick Go ] [ text "start" ]
        ]
