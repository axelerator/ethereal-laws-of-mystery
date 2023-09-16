module Game exposing (Model, Msg, fromBackend, init, update, view, subscriptions)

import Hades
    exposing
        ( Card
        , RealmId(..)
        , ToBackend(..)
        , ToBackendEnvelope(..)
        , ToFrontend(..)
        , ToGame(..)
        , Transition(..)
        , toBackendEnvelopeEncoder
        )
import Html exposing (br, button, div, text)
import Html.Events exposing (onClick)
import Html.Attributes exposing (class)
import Http exposing (jsonBody)
import WebAuthn exposing (Msg)
import Cards exposing (CardsModel)
import Cards 
import Cards exposing (CardId, Location(..), addCard, lastHandId, fold, moveCardTo)



type alias Model =
    { counter : Int
    , realmId : RealmId
    , cardIdGen : Int
    , cards : CardsModel
    }

init : RealmId -> Model
init realmId =
    { counter = 0
    , realmId = realmId
    , cardIdGen = 0
    , cards = Cards.init_
    }

subscriptions : Model -> Sub Msg
subscriptions model =
    Cards.subscriptions_ model.cards GotFrame


send : RealmId -> ToGame -> Cmd Msg
send realmId msg =
    sendToBackend <| ForRealm realmId <| ForGame msg


sendToBackend : ToBackendEnvelope -> Cmd Msg
sendToBackend msg =
    Http.post
        { url = "/send"
        , body = jsonBody <| toBackendEnvelopeEncoder <| msg
        , expect = Http.expectWhatever GotSendResponse
        }


fromBackend : Transition -> Msg
fromBackend transition =
    FromBackend transition


type Msg
    = GotSendResponse (Result Http.Error ())
    | FromBackend Transition
    | Draw
    | Noop CardId
    | GotFrame Float




updateFromRealm toFrontend model =
    model

drawFromDeck : Model -> String -> Model
drawFromDeck model content =
    let
        newId =
            model.cardIdGen

        newCard =
            { id = newId
            , location = Deck
            , content = content
            }

        withNewCard =
            addCard model.cards newId newCard content

        nextHandPos =
            1 + fold lastHandId -1 withNewCard
    in
    { model
        | cardIdGen = model.cardIdGen + 1
        , cards = moveCardTo withNewCard newId (MyHand nextHandPos)
    }


update : { a | webauthn : b } -> Msg -> Model -> (Model, Cmd Msg)
update { webauthn } msg model =
    case msg of
        Noop _ ->
          (model, Cmd.none)
        FromBackend transition ->
            case transition of
                IDraw card ->
                    ( drawFromDeck model (String.fromInt card.number)
                    , Cmd.none 
                    )

                TheyDraw ->
                    ( model, Cmd.none )

        GotSendResponse result ->
            ( model, Cmd.none )

        Draw ->
            ( model, send model.realmId DrawFromPile )
        GotFrame delta ->
          ( { model | cards = Cards.gotFrame delta model.cards }
          , Cmd.none
          )


view model =
    div []
        [ cardsView model
        , button [ onClick Draw ] [ text "draw" ]
        ]


cardsView {cards} =
   Cards.viewAnis cards Noop 
