module Game exposing (Model, Msg, fromBackend, init, subscriptions, update, view)

import Cards exposing (Card, CardId, CardsModel, Location(..), Point, addCard, fold, lastHandId, move, moveCardTo, point, vec)
import Draggable
import Draggable.Events
import Hades
    exposing
        ( CardContent(..)
        , Operator(..)
        , RealmId(..)
        , ToBackend(..)
        , ToBackendEnvelope(..)
        , ToFrontend(..)
        , ToGame(..)
        , Transition(..)
        , toBackendEnvelopeEncoder
        )
import Html exposing (Html, button, div, p, text)
import Html.Attributes exposing (class, style)
import Html.Events exposing (onClick)
import Http exposing (jsonBody)
import Point2d exposing (toPixels)
import String exposing (fromFloat, fromInt)
import WebAuthn exposing (Msg)


type alias Model =
    { counter : Int
    , realmId : RealmId
    , cardIdGen : Int
    , cards : CardsModel
    , draggedCard : Maybe ( Cards.Card, Point, Int )
    , drag : Draggable.State DragId
    }


init : RealmId -> Model
init realmId =
    { counter = 0
    , realmId = realmId
    , cardIdGen = 0
    , cards = Cards.init_
    , draggedCard = Nothing
    , drag = Draggable.init
    }


subscriptions : Model -> Sub Msg
subscriptions model =
    Sub.batch
        [ Cards.subscriptions_ model.cards GotFrame
        , Draggable.subscriptions DragMsg model.drag
        ]


dragConfig : Draggable.Config DragId Msg
dragConfig =
    Draggable.customConfig
        [ Draggable.Events.onDragStart OnDragStart
        , Draggable.Events.onDragEnd OnDragEnd
        , Draggable.Events.onDragBy OnDragBy
        ]


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


type alias DragId =
    ( CardId, Int )


type Msg
    = GotSendResponse (Result Http.Error ())
    | FromBackend Transition
    | Draw
    | Noop CardId
    | GotFrame Float
    | OnDragStart DragId
    | OnDragEnd
    | OnDragBy Draggable.Delta
    | DragMsg (Draggable.Msg DragId)


updateFromRealm toFrontend model =
    model


drawFromDeck : Model -> CardContent -> Model
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
            addCard model.cards newId newCard Deck content

        nextHandPos =
            1 + fold lastHandId -1 withNewCard
    in
    { model
        | cardIdGen = model.cardIdGen + 1
        , cards = moveCardTo withNewCard newId (MyHand nextHandPos)
    }


update : { a | webauthn : b } -> Msg -> Model -> ( Model, Cmd Msg )
update { webauthn } msg model =
    case msg of
        Noop _ ->
            ( model, Cmd.none )

        FromBackend transition ->
            case transition of
                IDraw content ->
                    ( drawFromDeck model content
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

        OnDragStart ( cardId, originalHandPos ) ->
            let
                ( card, cards_ ) =
                    Cards.removeCard cardId model.cards
            in
            case card of
                Just ( c, p ) ->
                    ( { model
                        | draggedCard = Just ( c, p, originalHandPos )
                        , cards = cards_
                      }
                    , Cmd.none
                    )

                Nothing ->
                    ( model, Cmd.none )

        OnDragEnd ->
            case model.draggedCard of
                Just ( card, pos, originalHandPos ) ->
                    let
                        fromDrag =
                            addCard model.cards card.id card (InFlight pos) card.content

                        toHand =
                            moveCardTo fromDrag card.id (MyHand originalHandPos)
                    in
                    ( { model
                        | cards = toHand
                        , draggedCard = Nothing
                      }
                    , Cmd.none
                    )

                Nothing ->
                    ( model, Cmd.none )

        OnDragBy ( dx, dy ) ->
            case model.draggedCard of
                Just ( card, pos, originalHandPos ) ->
                    ( { model | draggedCard = Just ( card, move (vec dx dy) pos, originalHandPos ) }
                    , Cmd.none
                    )

                Nothing ->
                    ( model, Cmd.none )

        DragMsg dragMsg ->
            Draggable.update dragConfig dragMsg model


view model =
    div []
        [ div [ class "cards" ] (cardsView model ++ draggedCardView model.draggedCard)
        , button [ onClick Draw ] [ text "draw" ]
        ]


draggedCardView : Maybe ( Cards.Card, Point, Int ) -> List (Html Msg)
draggedCardView tpl =
    case tpl of
        Just ( card, pos, _ ) ->
            let
                { x, y } =
                    toPixels pos

                aniAttrs =
                    style "transform" <| "translate(" ++ fromFloat x ++ "px," ++ fromFloat y ++ "px) rotate(10deg)"
            in
            [ div [ class "card", aniAttrs ] <| viewCardContent card.content ]

        Nothing ->
            []


viewCardContent :  CardContent -> List (Html Msg)
viewCardContent content =
    let
        txt =
            case content of
                NumberCard n ->
                    text <| fromInt n

                OperatorCard op ->
                    case op of
                        Plus ->
                            text "+"

                        Minus ->
                            text "-"

                        Times ->
                            text "*"

                SwapOperators ->
                    text "="
    in
        [ div [class "mini"] [txt]
        , div [class "big"] [txt]
        ]


cardView : Card -> List (Html.Attribute Msg) -> Html Msg
cardView card aniAttrs =
    let
        originalHandPos =
            case card.location of
                MyHand p ->
                    p

                _ ->
                    0

        z =
            case card.location of
                Deck ->
                    0

                MyHand i ->
                    100 - i

                DiscardPile ->
                    1

                InFlight _ ->
                    100

                CenterRow i ->
                    0

        class_ =
            case card.location of
                Deck ->
                    "deck"

                MyHand i ->
                    "hand"

                DiscardPile ->
                    "discardPile"

                InFlight _ ->
                    "inFlight"

                CenterRow i ->
                    "centerRow"
    in
    div
        (Draggable.mouseTrigger ( card.id, originalHandPos ) DragMsg
            :: (style "zIndex" <| fromInt z)
            :: class ("card " ++ class_)
            :: aniAttrs
        )
        <| viewCardContent card.content


cardsView : Model -> List (Html Msg)
cardsView { cards } =
    Cards.viewAnis cards cardView
