module Home exposing (Model, Msg, init, update, view, fromBackend)

import Hades exposing (RealmMsg(..), ToBackendEnvelope(..), toBackendEnvelopeEncoder)
import Html exposing (button, div, text)
import Html.Events exposing (onClick)
import Http exposing (jsonBody)
import Json.Encode as Encode
import WebAuthn exposing (Msg)
import Hades exposing (ToFrontend(..))


type alias Model =
  { counter: Int }


send : RealmMsg -> Cmd Msg
send msg =
    Http.post
        { url = "/send"
        , body = jsonBody <| toBackendEnvelopeEncoder <| ForRealm 0 msg
        , expect = Http.expectWhatever GotSendResponse
        }

fromBackend : ToFrontend -> Msg
fromBackend toFrontend = FromBackend toFrontend

type Msg
    = Click
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

        Click ->
            let
                data =
                    Encode.encode 0 <| toBackendEnvelopeEncoder <| ForRealm 0 Ping
            in
            ( model
            , send Ping
            )


view model =
    div [] 
      [ text <| String.fromInt model.counter
      , button [ onClick Click ] [ text "click" ] 
      ]
