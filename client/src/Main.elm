port module Main exposing (main)
import Hades exposing (toFrontendDecoder)

import Browser
import Hades exposing (ToFrontendEnvelope(..), toFrontendEnvelopeDecoder)
import Time
import Home
import Game
import Html exposing (Html, button, div, h1, input, text)
import Html.Attributes exposing (value)
import Html.Events exposing (onClick, onInput)
import Json.Decode as Decode
import WebAuthn
import Task
import Hades exposing (ToFrontend(..))
import Hades exposing (ToBackend(..))


port portOut : ( String, String ) -> Cmd msg


port portIn : (( String, String ) -> msg) -> Sub msg


logout : Cmd msg
logout =
    portOut ( "logout", "" )


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
    | OnGame Game.Model


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
    | ForGame Game.Msg
    | GotLoginResponse ( String, String )

globalActions =
  { webauthn = portOut
  }



update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case ( msg, model ) of
        ( GotLoginResponse _, OnLogin model_ ) ->
            ( OnHome Home.init
            , Cmd.none
            )

        ( GotLoginResponse ( "event", eventData ), OnGame model_ ) ->
            let
                noop =
                    ( model, Cmd.none )
            in
            case Decode.decodeString toFrontendEnvelopeDecoder eventData of
                Ok envelope ->
                  case Debug.log "Event:" envelope of
                        Noop ->
                            noop

                        Unauthorized ->
                            ( Tuple.first <| init ()
                            , logout
                            )

                        FromRealm toFrontend ->
                            case toFrontend of
                                EnteredGame realmId ->
                                  noop
                                ToGameFrontend forGame ->
                                  let
                                    ( model__, cmd_ ) =
                                        Game.update globalActions (Game.fromBackend forGame) model_

                                    cmd =
                                        Cmd.map ForGame cmd_
                                  in
                                    ( OnGame model__
                                    , cmd
                                    )

                                ToLobbyFrontend forLobby ->
                                  noop

                Err _ ->
                    noop
        ( GotLoginResponse ( "event", eventData ), OnHome model_ ) ->
            let
                noop =
                    ( model, Cmd.none )
            in
            case Decode.decodeString toFrontendEnvelopeDecoder eventData of
                Ok envelope ->
                  case Debug.log "Event:" envelope of
                        Noop ->
                            noop

                        Unauthorized ->
                            ( Tuple.first <| init ()
                            , logout
                            )

                        FromRealm toFrontend ->
                            case toFrontend of
                                EnteredGame realmId ->
                                  (OnGame <| Game.init realmId
                                  , Cmd.none
                                  )
                                ToGameFrontend forGame ->
                                  noop

                                ToLobbyFrontend forLobby ->
                                  let
                                    ( model__, cmd_ ) =
                                        Home.update globalActions (Home.fromBackend forLobby) model_

                                    cmd =
                                        Cmd.map ForHome cmd_
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

        ( ForGame msg_, OnGame model_ ) ->
            let
                ( model__, cmd_ ) =
                    Game.update { webauthn = portOut } msg_ model_

                cmd =
                    Cmd.map ForGame cmd_
            in
            ( OnGame model__
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
subscriptions model =
    Sub.batch 
      [ portIn GotLoginResponse
      , case model of
          OnGame model_ -> Sub.map ForGame <| Game.subscriptions model_
          _ -> Sub.none
      ]


view : Model -> Html Msg
view model =
    case model of
        OnLogin model_ ->
            Html.map ForWebauthn <| WebAuthn.view model_

        OnHome model_ ->
            Html.map ForHome <| Home.view model_

        OnGame model_ ->
            Html.map ForGame <| Game.view model_

