port module Main exposing (main)

import Browser
import Game
import Hades exposing (ToBackend(..), ToFrontend(..), ToFrontendEnvelope(..), toFrontendEnvelopeDecoder)
import Home
import Html exposing (Html)
import Json.Decode as Decode
import WebAuthn


port portOut : ( String, String ) -> Cmd msg


port portIn : (( String, String ) -> msg) -> Sub msg


logout : Cmd msg
logout =
    portOut ( "logout", "" )


type alias Flags =
    String


main : Program Flags Model Msg
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


init : Flags -> ( Model, Cmd Msg )
init lastLoginInfo =
    let
        ( model, cmd, cmd_ ) =
            WebAuthn.initOnLogin { webauthn = portOut } lastLoginInfo
    in
    ( OnLogin <| model
    , Cmd.batch [ Cmd.map ForWebauthn cmd, Cmd.map ForWebauthn cmd_ ]
    )


type Msg
    = ForWebauthn WebAuthn.Msg
    | ForHome Home.Msg
    | ForGame Game.Msg
    | FromPort ( String, String )


globalActions =
    { webauthn = portOut
    }


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case ( msg, model ) of
        ( FromPort _, OnLogin _ ) ->
            let
                ( model__, cmd ) =
                    Home.init
            in
            ( OnHome model__
            , Cmd.map ForHome cmd
            )

        ( FromPort ( "event", eventData ), OnGame model_ ) ->
            let
                noop =
                    ( model, Cmd.none )
            in
            case Decode.decodeString toFrontendEnvelopeDecoder eventData of
                Ok envelope ->
                    case envelope of
                        Noop ->
                            noop

                        Unauthorized ->
                            ( Tuple.first <| init ""
                            , logout
                            )

                        FromRealm toFrontend ->
                            case toFrontend of
                                EnteredGame _ _ ->
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

        ( FromPort ( "event", eventData ), OnHome model_ ) ->
            let
                noop =
                    ( model, Cmd.none )
            in
            case Decode.decodeString toFrontendEnvelopeDecoder eventData of
                Ok envelope ->
                    case envelope of
                        Noop ->
                            noop

                        Unauthorized ->
                            ( Tuple.first <| init ""
                            , logout
                            )

                        FromRealm toFrontend ->
                            case toFrontend of
                                EnteredGame realmId gameInfo ->
                                    let
                                        ( model__, cmd_ ) =
                                            Game.init realmId gameInfo
                                    in
                                    ( OnGame model__
                                    , Cmd.map ForGame cmd_
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
            case Game.returnToLobby model_ msg_ of
                Just ( width, height ) ->
                    let
                        ( m, cmd__ ) =
                            Home.withOpenMenu width height
                    in
                    ( OnHome m
                    , Cmd.map ForHome cmd__
                    )

                _ ->
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
        [ portIn FromPort
        , case model of
            OnGame model_ ->
                Sub.map ForGame <| Game.subscriptions model_

            OnHome model_ ->
                Sub.map ForHome <| Home.subscriptions model_

            _ ->
                Sub.none
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
