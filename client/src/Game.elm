module Game exposing (Model, Msg, fromBackend, init, subscriptions, update, view)

import Cards exposing (Card, CardId, CardsModel,  Point, addCard, fold, idsOf, isNumberCard, lastHandId, move, moveCardTo, numberCenterCards, operatorCenterCards, point, swapCenterCards, vec)
import Draggable
import Draggable.Events
import Hades
    exposing
        ( CardContent(..)
        , GameInfo
        , Location(..)
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
import Html.Attributes exposing (class, dropzone, style)
import Html.Events exposing (onClick)
import Http exposing (jsonBody)
import List exposing (isEmpty)
import Maybe.Extra
import Point2d exposing (toPixels)
import String exposing (fromFloat, fromInt)
import WebAuthn exposing (Msg)
import Html.Attributes exposing (id)
import String exposing (dropLeft)
import Maybe.Extra exposing (values)
import Point2d exposing (fromPixels)
import Cards exposing (locationOf)
import Cards exposing (isInInflight)
import Cards exposing (removeCard)
import Cards exposing (consolidateHandCards)
import Cards exposing (isInCenterRow)


type alias Model =
    { counter : Int
    , realmId : RealmId
    , cardIdGen : Int
    , cards : CardsModel
    , draggedCard : Maybe ( Cards.Card, Point, Int )
    , drag : Draggable.State DragId
    , highlightedCards : List ( CardId, Highlight )
    }


type Highlight
    = DropZone Highlight
    | PotentialDrop


init : RealmId -> GameInfo -> Model
init realmId gameInfo =
    { counter = 0
    , realmId = realmId
    , cardIdGen = 10
    , cards = Cards.init_ gameInfo
    , highlightedCards = []
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
    | MouseEnter CardId
    | MouseLeave CardId


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

placeInFlightCard : Model -> Location -> Model
placeInFlightCard model targetLocation =
    let
        inFlightCard = List.head <| idsOf isInInflight model.cards
        cardOnTarget = List.head <| idsOf (\c -> c.location == targetLocation) model.cards

        cards =
          case (inFlightCard, cardOnTarget) of
              (Just fromId, Just toId) ->
                let
                    (_, withoutPrevCenterCard) = removeCard toId model.cards
                    toCenter = moveCardTo withoutPrevCenterCard fromId targetLocation 
                in
                toCenter
              _ ->
                model.cards
    in
    { model | cards = consolidateHandCards cards }




removeHighlightFrom : Model -> Model
removeHighlightFrom model =
    let
        replaceHighlight (( c, hl ) as chl) =
            case hl of
                DropZone before ->
                    ( c, before )

                _ ->
                    chl
    in
    { model
        | highlightedCards = List.map replaceHighlight model.highlightedCards
    }


highlightAsDropZone highlightedCards cardId =
    let
        replaceHighlight (( c, hl ) as chl) =
            if c == cardId then
                ( c, DropZone hl )

            else
                chl
    in
    List.map replaceHighlight highlightedCards


update : { a | webauthn : b } -> Msg -> Model -> ( Model, Cmd Msg )
update { webauthn } msg model =
    case msg of
        MouseLeave cardId ->
            ( removeHighlightFrom model
            , Cmd.none
            )

        MouseEnter cardId ->
            ( { model
                | highlightedCards = highlightAsDropZone model.highlightedCards cardId
              }
            , Cmd.none
            )

        Noop _ ->
            ( model, Cmd.none )

        FromBackend transition ->
            case transition of
                IDraw content ->
                    ( drawFromDeck model content
                    , Cmd.none
                    )
                IPlayed targetLocation ->
                    ( placeInFlightCard model targetLocation
                    , Cmd.none
                    )

                TheyDraw ->
                    ( model, Cmd.none )

        GotSendResponse _ ->
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

                highlightedCards =
                    case card of
                        Just ( c, _ ) ->
                            case c.content of
                                NumberCard _ ->
                                    idsOf numberCenterCards cards_

                                OperatorCard _ ->
                                    idsOf operatorCenterCards cards_

                                SwapOperators ->
                                    idsOf operatorCenterCards cards_

                        Nothing ->
                            []
            in
            case card of
                Just ( c, p ) ->
                    ( { model
                        | draggedCard = Just ( c, p, originalHandPos )
                        , cards = cards_
                        , highlightedCards = List.map (\id -> ( id, PotentialDrop )) highlightedCards
                      }
                    , Cmd.none
                    )

                Nothing ->
                    ( model, Cmd.none )

        OnDragEnd ->
            case model.draggedCard of
                Just ( card, pos, originalHandPos ) ->
                    let
                        {x,y} = toPixels pos
                        fromDrag =
                            addCard model.cards card.id card (InFlight x y) card.content

                        toHand =
                            moveCardTo fromDrag card.id (MyHand originalHandPos)
                        dropZoneCard (id, hl) =
                          case hl of
                              DropZone _ -> Just id
                              _ -> Nothing
                        dropLocation =
                            List.head <| values <| List.map dropZoneCard model.highlightedCards
                        (cards, cmd) =
                          case dropLocation of
                              Just droppedOnId ->
                                case locationOf model.cards droppedOnId of
                                    Just (CenterRow centerPos) -> 
                                      (fromDrag, send model.realmId <| Play originalHandPos centerPos )
                                    _ ->
                                      (fromDrag, Cmd.none)
                                      
                              Nothing ->
                                (toHand, Cmd.none)
                          
                    in
                    ( { model
                        | cards = cards
                        , draggedCard = Nothing
                        , highlightedCards = []
                      }
                    , cmd
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
            [ div [ class "dragged card", aniAttrs ] <| viewCardContent card.content ]

        Nothing ->
            []


viewCardContent : CardContent -> List (Html Msg)
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
    [ div [ class "mini" ] [ txt ]
    , div [ class "big" ] [ txt ]
    ]


highlightClassName : ( CardId, Highlight ) -> String
highlightClassName ( _, h ) =
    case h of
        DropZone _ ->
            "dropzone"

        PotentialDrop ->
            "potentialdrop"


highlightClass : Maybe ( CardId, Highlight ) -> String
highlightClass hl =
    Maybe.map highlightClassName hl
        |> Maybe.withDefault ""


cardView : Bool -> List ( CardId, Highlight ) -> Card -> List (Html.Attribute Msg) -> Html Msg
cardView isDragging highlightedCards card aniAttrs =
    let
        dragTrigger =
            case card.location of
                MyHand p ->
                  [Draggable.mouseTrigger ( card.id, p ) DragMsg]

                _ ->
                    []

        highLight =
            highlightClass <| List.head <| List.filter (\( id, _ ) -> id == card.id) highlightedCards

        z =
            case card.location of
                Deck ->
                    0

                MyHand i ->
                    100 - i

                DiscardPile ->
                    1

                InFlight _ _ ->
                    300 -- not used from here but set globally in styles.css

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

                InFlight _ _ ->
                    "inFlight"

                CenterRow i ->
                    "centerRow"

        mouseEvents =
            if isDragging && isInCenterRow card then
                [ Html.Events.onMouseEnter <| MouseEnter card.id
                , Html.Events.onMouseLeave <| MouseLeave card.id
                ]

            else
                []
    in
    div
            
            ( (style "zIndex" <| fromInt z)
            :: class (highLight ++ " card " ++ class_)
            :: mouseEvents
            ++ dragTrigger
            ++ aniAttrs
        )
    <|
        viewCardContent card.content


cardsView : Model -> List (Html Msg)
cardsView { cards, highlightedCards, draggedCard } =
    Cards.viewAnis cards (cardView (Maybe.Extra.isJust draggedCard) highlightedCards)
