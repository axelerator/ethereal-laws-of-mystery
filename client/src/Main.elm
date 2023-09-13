port module Main exposing (main)

import Browser
import Hades exposing (ToFrontendEnvelope(..), toFrontendEnvelopeDecoder)
import Home
import Html exposing (Html, button, div, h1, input, text)
import Html.Attributes exposing (value)
import Html.Events exposing (onClick, onInput)
import Json.Decode as Decode
import WebAuthn


port portOut : ( String, String ) -> Cmd msg


port portIn : (( String, String ) -> msg) -> Sub msg


main : Program () Model Msg
main =
    Browser.element
        { init = init
        , update = update
        , subscriptions = subscriptions
        , view = view
        }


type Model
    = OnLogin WebAuthn.Model
    | OnHome Home.Model


init : () -> ( Model, Cmd Msg )
init _ =
    let
        ( model, cmd ) =
            WebAuthn.initOnLogin { webauthn = portOut } "at"
    in
    ( OnLogin <| model
    , Cmd.map ForWebauthn cmd
    )


type Msg
    = ForWebauthn WebAuthn.Msg
    | ForHome Home.Msg
    | GotLoginResponse (String, String)


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case ( msg, model ) of
        ( GotLoginResponse _, OnLogin model_ ) ->
            ( OnHome Home.init
            , Cmd.none
            )

        ( GotLoginResponse ( "event", eventData ), OnHome model_ ) ->
            let
                noop =
                    ( model, Cmd.none )
            in
            case Decode.decodeString toFrontendEnvelopeDecoder eventData of
                Ok envelope ->
                    case envelope of
                          Noop -> noop
                          Unauthorized -> noop
                          FromRealm (toFrontend) ->
                            let
                                (model__, cmd_) =
                                  Home.update {webauthn = portOut} (Home.fromBackend toFrontend) model_
                                cmd = Cmd.map ForHome cmd_
                            in
                              ( OnHome model__
                              , cmd
                              )

                Err _ ->
                    noop

        ( ForHome msg_, OnHome model_ ) ->
            let
                ( model__, cmd_ ) =
                    Home.update { webauthn = portOut } msg_ model_

                cmd =
                    Cmd.map ForHome cmd_
            in
            ( OnHome model__
            , cmd
            )

        ( ForWebauthn msg_, OnLogin model_ ) ->
            let
                ( model__, cmd_ ) =
                    WebAuthn.update { webauthn = portOut } msg_ model_

                cmd =
                    Cmd.map ForWebauthn cmd_
            in
            ( OnLogin model__
            , cmd
            )

        _ ->
            ( model, Cmd.none )


subscriptions : Model -> Sub Msg
subscriptions _ =
    portIn GotLoginResponse


view : Model -> Html Msg
view model =
    case model of
        OnLogin model_ ->
            Html.map ForWebauthn <| WebAuthn.view model_

        OnHome model_ ->
            Html.map ForHome <| Home.view model_
