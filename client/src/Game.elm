module Game exposing (Model, Msg, fromBackend, init, returnToLobby, subscriptions, update, view)

import Angle exposing (turn)
import Axis3d exposing (z)
import Browser.Dom exposing (getViewport)
import Browser.Events exposing (onResize)
import Cards
    exposing
        ( Animating(..)
        , Card
        , CardAniAttrs
        , CardId
        , CardsModel
        , OpponentId
        , Point
        , Vec
        , addCard
        , consolidateHandCards
        , deckAttrs
        , draggedOver
        , fold
        , idOf
        , idsOf
        , inlineCSS
        , isInInflight
        , isOnDiscardPile
        , lastHandId
        , locationOf
        , move
        , moveCardTo
        , numberCenterCards
        , operatorCenterCards
        , revealContent
        , updateViewport
        , vec
        )
import Draggable
import Draggable.Events
import Hades
    exposing
        ( CardContent(..)
        , GameInfo
        , GameState(..)
        , Location(..)
        , Operator(..)
        , Opponent
        , RealmId(..)
        , ToBackend(..)
        , ToBackendEnvelope(..)
        , ToFrontend(..)
        , ToGame(..)
        , Transition(..)
        , toBackendEnvelopeEncoder
        )
import Html exposing (Attribute, Html, a, br, button, div, p, span, text)
import Html.Attributes exposing (attribute, class, id, style)
import Html.Events exposing (onClick)
import Http exposing (jsonBody)
import Maybe.Extra exposing (values)
import Point2d exposing (toPixels)
import PseudoRandom
import String exposing (fromFloat, fromInt)
import Task
import Time
import WebAuthn exposing (Msg)


type alias Model =
    { counter : Int
    , realmId : RealmId
    , cardIdGen : Int
    , cards : CardsModel
    , draggedCard : Maybe ( Cards.Card, Point, Int )
    , drag : Draggable.State DragId
    , highlightedCards : List ( CardId, Highlight )
    , viewportSize : Vec
    , gameState : GameState
    , opponents : List Opponent
    , turn : RelativeOpponent
    , pileSize : Int
    , fadingMsg : Maybe ( String, Float )
    }



{-
   0 = My Turn
-}


type alias RelativeOpponent =
    Int


type Highlight
    = DropZone Highlight
    | PotentialDrop


init : RealmId -> GameInfo -> ( Model, Cmd Msg )
init realmId gameInfo =
    let
        ( cards, cardsIdGen ) =
            Cards.init_ (vec 500 500) gameInfo
    in
    ( { counter = 0
      , realmId = realmId
      , cardIdGen = cardsIdGen
      , cards = cards
      , pileSize = gameInfo.pileSize
      , highlightedCards = []
      , draggedCard = Nothing
      , drag = Draggable.init
      , viewportSize = vec 500 500
      , gameState = gameInfo.gameState
      , opponents = gameInfo.opponents
      , turn = gameInfo.turn
      , fadingMsg = Nothing
      }
    , Task.perform GotViewPort getViewport
    )


subscriptions : Model -> Sub Msg
subscriptions model =
    let
        msgSub =
            case model.fadingMsg of
                Just ( _, timeLeft ) ->
                    Time.every timeLeft FadeMsg

                Nothing ->
                    Sub.none
    in
    Sub.batch <|
        [ Cards.subscriptions_ model.cards GotFrame
        , Draggable.subscriptions DragMsg model.drag
        , onResize Resized
        , msgSub
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
    | GotViewPort Browser.Dom.Viewport
    | Resized Int Int
    | ReturnToLobby
    | FadeMsg Time.Posix


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
        , pileSize = model.pileSize - 1
        , turn = 1
    }


placeInFlightCard : Model -> Location -> Model
placeInFlightCard model targetLocation =
    let
        inFlightCard =
            List.head <| idsOf isInInflight model.cards

        cardOnTarget =
            List.head <| idsOf (\c -> c.location == targetLocation) model.cards

        cards =
            case ( inFlightCard, cardOnTarget ) of
                ( Just fromId, Just toId ) ->
                    let
                        lastDiscardedPos =
                            List.length <| idsOf isOnDiscardPile model.cards

                        withoutDiscardedCenterCard =
                            moveCardTo model.cards toId (DiscardPile (lastDiscardedPos + 1))
                    in
                    moveCardTo withoutDiscardedCenterCard fromId targetLocation

                _ ->
                    model.cards
    in
    { model
        | cards = consolidateHandCards cards
        , turn = 1
    }


removeHighlightFrom : List ( CardId, Highlight ) -> List ( CardId, Highlight )
removeHighlightFrom highlightedCards =
    let
        replaceHighlight (( c, hl ) as chl) =
            case hl of
                DropZone before ->
                    ( c, before )

                _ ->
                    chl
    in
    List.map replaceHighlight highlightedCards


highlightAsDropZone : List ( CardId, Highlight ) -> CardId -> List ( CardId, Highlight )
highlightAsDropZone highlightedCards cardId =
    let
        replaceHighlight (( c, hl ) as chl) =
            if c == cardId then
                ( c, DropZone hl )

            else
                chl
    in
    List.map replaceHighlight <| removeHighlightFrom highlightedCards


returnToLobby : Msg -> Bool
returnToLobby msg =
    msg == ReturnToLobby


update : { a | webauthn : b } -> Msg -> Model -> ( Model, Cmd Msg )
update {} msg model =
    case msg of
        ReturnToLobby ->
            ( model
            , Cmd.none
            )

        GotViewPort { scene } ->
            ( { model | cards = updateViewport (vec scene.width scene.height) model.cards }
            , Cmd.none
            )

        Resized _ _ ->
            ( model
            , Task.perform GotViewPort getViewport
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

                TheyDraw opponentId nextPlayer ->
                    ( theyDraw model opponentId nextPlayer
                    , Cmd.none
                    )

                IWon ->
                    ( { model | gameState = GameOver True }
                    , Cmd.none
                    )

                ILost ->
                    ( { model | gameState = GameOver False }
                    , Cmd.none
                    )

                TheyPlayed from to content nextPlayer ->
                    ( theyPlayed model from to content nextPlayer
                    , Cmd.none
                    )

                TurnChanged turn ->
                    ( { model
                        | turn = turn
                      }
                    , Cmd.none
                    )

                GameEnded _ ->
                    ( model
                    , Cmd.none
                    )

        GotSendResponse _ ->
            ( model, Cmd.none )

        Draw ->
            case ( model.gameState, model.turn ) of
                ( Running, 0 ) ->
                    ( model, send model.realmId DrawFromPile )

                ( Running, _ ) ->
                    ( { model | fadingMsg = Just ( "It's not your turn yet!", 128000 ) }
                    , Cmd.none
                    )

                _ ->
                    ( model, Cmd.none )

        GotFrame delta ->
            ( { model | cards = Cards.gotFrame delta model.cards }
            , Cmd.none
            )

        OnDragStart ( cardId, originalHandPos ) ->
            let
                ( card, cards_ ) =
                    Cards.removeCard cardId model.cards

                highlightedCards =
                    case ( card, model.turn ) of
                        ( Just ( c, _ ), 0 ) ->
                            case c.content of
                                NumberCard _ ->
                                    idsOf numberCenterCards cards_

                                OperatorCard _ ->
                                    idsOf operatorCenterCards cards_

                                SwapOperators ->
                                    idsOf operatorCenterCards cards_

                        _ ->
                            []
            in
            case ( card, model.gameState ) of
                ( Just ( c, p ), Running ) ->
                    ( { model
                        | draggedCard = Just ( c, p, originalHandPos )
                        , cards = cards_
                        , highlightedCards = List.map (\id -> ( id, PotentialDrop )) highlightedCards
                      }
                    , Cmd.none
                    )

                _ ->
                    ( model, Cmd.none )

        OnDragEnd ->
            case model.draggedCard of
                Just ( card, pos, originalHandPos ) ->
                    let
                        { x, y } =
                            toPixels pos

                        fromDrag =
                            addCard model.cards card.id card (InFlightOpen x y) card.content

                        toHand =
                            moveCardTo fromDrag card.id (MyHand originalHandPos)

                        dropZoneCard ( id, hl ) =
                            case hl of
                                DropZone _ ->
                                    Just id

                                _ ->
                                    Nothing

                        dropLocation =
                            List.head <| values <| List.map dropZoneCard model.highlightedCards

                        ( cards, cmd ) =
                            case dropLocation of
                                Just droppedOnId ->
                                    case locationOf model.cards droppedOnId of
                                        Just (CenterRow centerPos) ->
                                            ( fromDrag, send model.realmId <| Play originalHandPos centerPos )

                                        _ ->
                                            ( fromDrag, Cmd.none )

                                Nothing ->
                                    ( toHand, Cmd.none )
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
                    let
                        newPos =
                            move (vec dx dy) pos

                        highlightedCards =
                            case draggedOver newPos model.cards of
                                Just cardId ->
                                    highlightAsDropZone model.highlightedCards cardId

                                Nothing ->
                                    removeHighlightFrom model.highlightedCards
                    in
                    ( { model
                        | draggedCard = Just ( card, newPos, originalHandPos )
                        , highlightedCards = highlightedCards
                      }
                    , Cmd.none
                    )

                Nothing ->
                    ( model, Cmd.none )

        DragMsg dragMsg ->
            Draggable.update dragConfig dragMsg model

        FadeMsg _ ->
            ( { model | fadingMsg = Nothing }
            , Cmd.none
            )


theyPlayed : Model -> Location -> Location -> CardContent -> RelativeOpponent -> Model
theyPlayed model from to content nextPlayer =
    case ( idOf from model.cards, idOf to model.cards ) of
        ( Just fromId, Just toId ) ->
            let
                lastDiscardedPos =
                    List.length <| idsOf isOnDiscardPile model.cards

                withoutDiscardedCenterCard =
                    moveCardTo model.cards toId (DiscardPile (lastDiscardedPos + 1))

                withRevealedCard =
                    revealContent withoutDiscardedCenterCard fromId content

                cards =
                    moveCardTo withRevealedCard fromId to
            in
            { model
                | cards = cards
                , turn = nextPlayer
            }

        _ ->
            model


theyDraw : Model -> OpponentId -> RelativeOpponent -> Model
theyDraw model opponentId nextPlayer =
    let
        newId =
            model.cardIdGen

        content =
            NumberCard 1

        newCard =
            { id = newId
            , location = Deck
            , content = content
            }

        withNewCard =
            addCard model.cards newId newCard Deck content

        nextHandPos =
            case List.head <| List.drop opponentId model.opponents of
                Just opponent ->
                    opponent.handSize + 1

                Nothing ->
                    0
    in
    { model
        | cardIdGen = model.cardIdGen + 1
        , cards = moveCardTo withNewCard newId (TheirHand opponentId nextHandPos)
        , turn = nextPlayer
    }


view model =
    let
        wonCSS =
            case model.gameState of
                GameOver _ ->
                    "game-over"

                _ ->
                    ""

        wonDiv =
            case model.gameState of
                GameOver True ->
                    div [ class "wonLabel" ]
                        [ div [ class "pyro" ]
                            [ div [ class "before" ] []
                            , div [ class "after" ] []
                            ]
                        , div []
                            [ text "You're the bomb!"
                            , br [] []
                            , button [ onClick ReturnToLobby, class "in-game-button" ] [ text "return to lobby" ]
                            ]
                        ]

                GameOver False ->
                    div [ class "lostLabel" ] [ div [] [ text "Better luck next time" ] ]

                _ ->
                    text ""

        turnCSS =
            "turn-" ++ fromInt model.turn
    in
    div []
        [ inlineCSS model.cards
        , wonDiv
        , fadeMsgView model.fadingMsg
        , div [ class <| turnCSS ++ " cards " ++ wonCSS ]
            (cardsView model ++ draggedCardView model.draggedCard)
        ]


fadeMsgView : Maybe ( String, Float ) -> Html Msg
fadeMsgView mb =
    case mb of
        Just ( msg, _ ) ->
            let
                chars =
                    String.toList msg

                rnd =
                    PseudoRandom.floatSequence (List.length chars) 24 ( 2, 5 )
            in
            div [ class "flash" ]
                [ div [] <|
                    List.indexedMap
                        (\i c ->
                            span
                                [ attribute "style" <|
                                    "--delay:"
                                        ++ (fromFloat <| Maybe.withDefault 0 <| List.head <| List.drop i rnd)
                                        ++ "s"
                                ]
                                [ text <| String.fromList [ c ] ]
                        )
                    <|
                        String.toList msg
                ]

        Nothing ->
            text ""


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
            [ div [ class "dragged card", aniAttrs ] [ viewCardContent [] card.content ] ]

        Nothing ->
            []


viewCardContent : List (Attribute Msg) -> CardContent -> Html Msg
viewCardContent aniAttrs content =
    let
        ( txt, imageName ) =
            case content of
                NumberCard n ->
                    ( text <| fromInt n, fromInt n )

                OperatorCard op ->
                    case op of
                        Plus ->
                            ( text "+", "plus" )

                        Minus ->
                            ( text "-", "minus" )

                        Times ->
                            ( text "*", "times" )

                SwapOperators ->
                    ( text "=", "swap" )
    in
    div (class "inner" :: aniAttrs)
        [ div [ class "front", style "background-image" <| "url(\"/assets/images/" ++ imageName ++ ".jpg\")" ]
            [ div [ class "mini" ] [ txt ]
            , div [ class "big" ] [ txt ]
            ]
        , div [ class "back" ] []
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


cardView : Bool -> List ( CardId, Highlight ) -> Card -> CardAniAttrs Msg -> Html Msg
cardView _ highlightedCards card ( aniAttrs, innerAttrs ) =
    let
        dragTrigger =
            case card.location of
                MyHand p ->
                    Draggable.mouseTrigger ( card.id, p ) DragMsg :: Draggable.touchTriggers ( card.id, p ) DragMsg

                _ ->
                    []

        highLight =
            highlightClass <| List.head <| List.filter (\( id, _ ) -> id == card.id) highlightedCards

        z =
            case card.location of
                Deck ->
                    10

                MyHand i ->
                    200 - i

                TheirHand _ i ->
                    200 - i

                DiscardPile i ->
                    100 + i

                InFlight _ _ ->
                    300

                InFlightOpen _ _ ->
                    300

                CenterRow _ ->
                    0

        class_ =
            case card.location of
                Deck ->
                    "deck"

                MyHand _ ->
                    "hand player-0"

                TheirHand op _ ->
                    "opponentHand player-" ++ fromInt op

                DiscardPile _ ->
                    "discardPile"

                InFlight _ _ ->
                    "inFlight"

                InFlightOpen _ _ ->
                    "inFlightOpen"

                CenterRow _ ->
                    "centerRow"
    in
    div
        ((style "zIndex" <| fromInt z)
            :: class (highLight ++ " card " ++ class_)
            :: dragTrigger
            ++ aniAttrs
        )
    <|
        [ viewCardContent innerAttrs card.content ]


deckCard : Card
deckCard =
    { id = -1, location = Deck, content = NumberCard 1 }


deckCardAni : CardsModel -> CardAniAttrs Msg
deckCardAni model =
    let
        ( attrs, iattrs ) =
            deckAttrs model
    in
    ( onClick Draw :: attrs, iattrs )


stack : Int -> List (Attribute Msg) -> Html Msg
stack size attrs =
    let
        wiggles =
            PseudoRandom.floatSequence (size // 5) 234 ( 0, 10 )
    in
    div attrs <|
        List.map2
            (\i w ->
                div
                    [ class "stack", attribute "style" <| "--offset: " ++ fromInt (i * 3) ++ "px; --wiggle: " ++ fromFloat w ++ "deg" ]
                    []
            )
            (List.reverse <| List.range 0 (size // 10))
            wiggles


deckView : Int -> CardsModel -> Html Msg
deckView deckSize cards =
    let
        ( attrs, _ ) =
            deckAttrs cards
    in
    div [ class "deck-stack" ]
        [ cardView False [] deckCard (deckCardAni cards)
        , stack deckSize attrs
        ]


cardsView : Model -> List (Html Msg)
cardsView { cards, highlightedCards, draggedCard, pileSize } =
    deckView pileSize cards
        :: Cards.viewAnis cards (cardView (Maybe.Extra.isJust draggedCard) highlightedCards)
