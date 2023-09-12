port module Main exposing (main)

import Browser
import Html exposing (Html, button, div, h1, text)
import Html.Events exposing (onClick)
import Html exposing (input)
import Html.Attributes exposing (value)
import Html.Events exposing (onInput)
import WebAuthn

port webauthn : (String, String) -> Cmd msg


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

init : () -> ( Model, Cmd Msg )
init _ =
  let
      (model, cmd) = WebAuthn.initOnLogin { webauthn = webauthn } "at1"
  in
  
    ( OnLogin <| model
    , Cmd.map ForWebauthn cmd
    )


type Msg
    = ForWebauthn WebAuthn.Msg

update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case (msg, model) of
        (ForWebauthn msg_, OnLogin model_) ->
          let
              (model__, cmd_) =
                WebAuthn.update {webauthn = webauthn} msg_ model_
              cmd = Cmd.map ForWebauthn cmd_
          in
            ( OnLogin model__
            , cmd
            )


subscriptions : Model -> Sub Msg
subscriptions _ =
    Sub.none

view : Model -> Html Msg
view model =
  case model of
      OnLogin model_ ->
        Html.map ForWebauthn <| WebAuthn.view model_

