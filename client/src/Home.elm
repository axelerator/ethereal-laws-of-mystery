module Home exposing (Model, Msg, fromBackend, init, update, view)

import Hades
    exposing
        ( RealmId(..)
        , ToBackend(..)
        , ToBackendEnvelope(..)
        , ToFrontend(..)
        , ToFrontendLobby(..)
        , ToLobby(..)
        , Transition(..)
        , toBackendEnvelopeEncoder
        )
import Html exposing (button, div, text)
import Html.Events exposing (onClick)
import Http exposing (jsonBody)
import WebAuthn exposing (Msg)


type alias Model =
    { waitingForMorePlayers : Bool }


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
    = GotSendResponse (Result Http.Error ())
    | FromBackend ToFrontendLobby
    | Go
    | MultiGo Int


init =
    { waitingForMorePlayers = False }


updateFromRealm toFrontend model =
    model


update { webauthn } msg model =
    case msg of
        FromBackend toFrontend ->
            case toFrontend of
                WaitingForMorePlayers ->
                    ( { model | waitingForMorePlayers = True }
                    , Cmd.none
                    )

                GameStart realmId ->
                    ( model
                    , sendToBackend <| EnterRealm realmId
                    )

        GotSendResponse result ->
            ( model, Cmd.none )

        Go ->
            ( model
            , send StartGame
            )

        MultiGo numberOfPlayers  ->
            ( model
            , send <| WaitForGame numberOfPlayers
            )


view model =
  if model.waitingForMorePlayers then
    div [] [text "Waiting for more players"]
  else
    div []
        [ button [ onClick Go ] [ text "Single Player" ]
        , button [ onClick (MultiGo 2) ] [ text "2 Player game" ]
        ]
