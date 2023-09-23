module Game exposing (Model, Msg, fromBackend, init, returnToLobby, subscriptions, update, view)

import Angle exposing (turn)
import Browser.Dom exposing (getViewport)
import Browser.Events exposing (onResize)
import Cards
    exposing
        ( Animating(..)
        , Card
        , CardAniAttrs
        , CardId
        , CardPositions
        , CardsModel
        , OpponentId
        , Point
        , Props
        , Vec
        , ViewportInfo
        , addCard_
        , addCards
        , discardPileWiggle
        , draggedOver
        , empty
        , idOf
        , idsOf
        , interpolate
        , locationOf
        , maxSpread
        , move
        , moveCardTo
        , offsetPerCard
        , offsetPerCardV
        , point
        , px
        , revealContent
        , startSpread
        , times
        , updateAni
        , updateCards
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
import Html exposing (Attribute, Html, a, br, button, div, node, p, span, text)
import Html.Attributes exposing (attribute, class, id, style)
import Html.Events exposing (onClick)
import Http exposing (jsonBody)
import Maybe.Extra exposing (values)
import Pixels
import Point2d exposing (toPixels)
import PseudoRandom
import String exposing (fromFloat, fromInt)
import Task
import Time
import Vector2d
import WebAuthn exposing (Msg)


type alias Model =
    { counter : Int
    , realmId : RealmId
    , cards : CardsModel
    , draggedCard : Maybe ( Cards.Card, Point, Int )
    , drag : Draggable.State DragId
    , highlightedCards : List ( CardId, Highlight )
    , viewportSize : Vec
    , viewportInfo : ViewportInfo
    , gameState : GameState
    , opponents : List Opponent
    , turn : RelativeOpponent
    , pileSize : Int
    , fadingMsg : Maybe ( String, Float )
    }


type alias RelativeOpponent =
    Int


type Highlight
    = DropZone Highlight
    | PotentialDrop


init__ : Vec -> GameInfo -> CardsModel
init__ viewportSize { center, hand, opponents, discardPile } =
    let
        centerCards =
            List.indexedMap (\i c -> ( CenterRow i, c )) center

        handCards =
            List.indexedMap (\i c -> ( MyHand i, c )) hand

        opponentCards =
            List.indexedMap mkOpponentCards opponents |> List.concat

        hiddenContent =
            NumberCard 1

        mkOpponentCards : Int -> Opponent -> List ( Location, CardContent )
        mkOpponentCards opId { handSize } =
            List.map (mkOpponentCard opId) <| List.range 0 handSize

        mkOpponentCard opId handPos =
            ( TheirHand opId handPos
            , hiddenContent
            )

        discardedCards =
            List.indexedMap mkDiscarded discardPile

        mkDiscarded i content =
            ( DiscardPile i
            , content
            )

        cards =
            List.concat [ centerCards, handCards, opponentCards, discardedCards ]
    in
    addCards cards (empty viewportSize)


init : RealmId -> GameInfo -> ( Model, Cmd Msg )
init realmId gameInfo =
    ( { counter = 0
      , realmId = realmId
      , cards = init__ (vec 500 500) gameInfo
      , pileSize = gameInfo.pileSize
      , highlightedCards = []
      , draggedCard = Nothing
      , drag = Draggable.init
      , viewportSize = vec 500 500
      , viewportInfo = viewportInfoFor <| vec 500 500
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
        cardPos =
            cardPositions model

        ( withNewCard, newId ) =
            addCard_ cardPos Deck content model.cards

        nextHandPos =
            List.length <| Cards.filter isInMyHand model.cards
    in
    { model
        | cards = moveCardTo cardPos withNewCard newId (MyHand nextHandPos)
        , pileSize = model.pileSize - 1
        , turn =
            if List.isEmpty model.opponents then
                0

            else
                1
    }


isInInflight : Card -> Bool
isInInflight { location } =
    case location of
        InFlight _ _ ->
            True

        InFlightOpen _ _ ->
            True

        _ ->
            False


placeInFlightCard : Model -> Location -> Model
placeInFlightCard model targetLocation =
    let
        inFlightCard =
            List.head <| Cards.filter isInInflight model.cards

        cardOnTarget =
            List.head <| idsOf (\c -> c.location == targetLocation) model.cards

        cards =
            case ( inFlightCard, cardOnTarget ) of
                ( Just inFlightCard_, Just toId ) ->
                    let
                        fromId =
                            inFlightCard_.id

                        lastDiscardedPos =
                            List.length <| idsOf isOnDiscardPile model.cards

                        withoutDiscardedCenterCard =
                            moveCardTo (cardPositions model) model.cards toId (DiscardPile (lastDiscardedPos + 1))
                    in
                    moveCardTo (cardPositions model) withoutDiscardedCenterCard fromId targetLocation

                _ ->
                    model.cards
    in
    { model
        | cards = consolidateHandCards (cardPositions model) cards
        , turn =
            if List.isEmpty model.opponents then
                0

            else
                1
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
            let
                viewportInfo =
                    viewportInfoFor <| vec scene.width scene.height
            in
            ( { model
                | cards = updateAni (screenPos viewportInfo) model.cards
                , viewportInfo = viewportInfo
              }
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
                    Cards.removeCard (cardPositions model) cardId model.cards

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

                        ( fromDrag, newId ) =
                            addCard_ (cardPositions model) (InFlightOpen x y) card.content model.cards

                        toHand =
                            moveCardTo (cardPositions model) fromDrag newId (MyHand originalHandPos)

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
                            case draggedOver newPos model.viewportInfo model.cards of
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
                    moveCardTo (cardPositions model) model.cards toId (DiscardPile (lastDiscardedPos + 1))

                withRevealedCard =
                    revealContent withoutDiscardedCenterCard fromId content

                cards =
                    moveCardTo (cardPositions model) withRevealedCard fromId to
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
        content =
            NumberCard 1

        ( withNewCard, newId ) =
            addCard_ (cardPositions model) Deck content model.cards

        nextHandPos =
            case List.head <| List.drop opponentId model.opponents of
                Just opponent ->
                    opponent.handSize + 1

                Nothing ->
                    0
    in
    { model
        | cards = moveCardTo (cardPositions model) withNewCard newId (TheirHand opponentId nextHandPos)
        , turn = nextPlayer
    }


view : Model -> Html Msg
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
        [ inlineCSS model.viewportInfo
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


deckCardAni : ViewportInfo -> CardAniAttrs Msg
deckCardAni viewportInfo =
    let
        ( attrs, iattrs ) =
            deckAttrs viewportInfo
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


deckView : ViewportInfo -> Int -> Html Msg
deckView viewportInfo deckSize =
    let
        ( attrs, _ ) =
            deckAttrs viewportInfo
    in
    div [ class "deck-stack" ]
        [ cardView False [] deckCard (deckCardAni viewportInfo)
        , stack deckSize attrs
        ]


cardsView : Model -> List (Html Msg)
cardsView { cards, highlightedCards, draggedCard, pileSize, viewportInfo } =
    deckView viewportInfo pileSize
        :: Cards.viewAnis cards (cardView (Maybe.Extra.isJust draggedCard) highlightedCards)


cardPositions : Model -> CardsModel -> Location -> Props
cardPositions { viewportInfo } =
    screenPos viewportInfo


centerRowCardGutterFactor =
    1.15


screenPos : ViewportInfo -> CardsModel -> Location -> Props
screenPos viewportInfo cardsModel loc =
    case loc of
        Deck ->
            viewportInfo.deckPos

        DiscardPile pos ->
            let
                default =
                    viewportInfo.discardPilePos
            in
            { default
                | degrees = Maybe.withDefault 0 <| List.head <| List.drop pos discardPileWiggle
            }

        MyHand p ->
            let
                handCardCount =
                    List.length <| Cards.filter isInMyHand cardsModel

                degreePerCard =
                    maxSpread / toFloat handCardCount

                totalWidth =
                    times (toFloat handCardCount) offsetPerCard

                left =
                    move (times -0.5 totalWidth) viewportInfo.handOrigin
            in
            { pos = move (times (toFloat p) offsetPerCard) left
            , opacity = 1.0
            , degrees = startSpread + degreePerCard * toFloat p
            , flip = 0
            }

        TheirHand opponentId p ->
            case opponentId of
                1 ->
                    let
                        handCardCount =
                            List.length <| Cards.filter (isInOpponentsHand opponentId) cardsModel

                        degreePerCard =
                            maxSpread / toFloat handCardCount

                        totalHeight =
                            times (toFloat handCardCount) offsetPerCardV

                        ( _, viewportHeight ) =
                            Vector2d.toTuple Pixels.inPixels viewportInfo.size

                        top =
                            move (times -0.5 totalHeight) (point (viewportInfo.cardSize.width * -0.5) (viewportHeight * 0.5))
                    in
                    { pos = move (times (toFloat p) offsetPerCardV) top
                    , opacity = 1.0
                    , degrees = -90 + (startSpread + degreePerCard * toFloat p)
                    , flip = 180
                    }

                2 ->
                    let
                        handCardCount =
                            List.length <| Cards.filter (isInOpponentsHand opponentId) cardsModel

                        degreePerCard =
                            maxSpread / toFloat handCardCount

                        totalWidth =
                            times (toFloat handCardCount) offsetPerCard

                        { x } =
                            toPixels viewportInfo.handOrigin

                        left =
                            move (times -0.5 totalWidth) (point x (viewportInfo.cardSize.height * -0.5))
                    in
                    { pos = move (times (toFloat p) offsetPerCard) left
                    , opacity = 1.0
                    , degrees = -startSpread - degreePerCard * toFloat p
                    , flip = 180
                    }

                _ ->
                    Debug.todo "positions for third opp "

        InFlight x y ->
            { pos = point x y
            , opacity = 1.0
            , degrees = 10
            , flip = 180
            }

        InFlightOpen x y ->
            { pos = point x y
            , opacity = 1.0
            , degrees = 10
            , flip = 0
            }

        CenterRow i ->
            { pos = centerRowSpot viewportInfo i
            , opacity = 1.0
            , degrees = 0
            , flip = 0
            }


viewportInfoFor : Vec -> ViewportInfo
viewportInfoFor size =
    let
        ( width, height ) =
            Vector2d.toTuple Pixels.inPixels size

        cardHeight =
            height / 4

        cardWidth =
            cardHeight * 0.7

        centerRowWidth =
            5 * centerRowCardGutterFactor * cardWidth

        centerRowX =
            (width * 0.5) - (centerRowWidth * 0.5)

        centerRowY =
            (height * 0.5) - (cardHeight * (centerRowCardGutterFactor + 0.25))

        deckPosX =
            (width * 0.5) - (centerRowCardGutterFactor * cardWidth)

        deckPosY =
            centerRowY + (centerRowCardGutterFactor * cardHeight)

        deckPos : Props
        deckPos =
            { pos = point deckPosX deckPosY
            , degrees = 0
            , opacity = 1.0
            , flip = 180
            }

        discardPilePos =
            { pos = point (deckPosX + (centerRowCardGutterFactor * cardWidth)) deckPosY
            , degrees = 0
            , opacity = 1.0
            , flip = 0
            }

        handOriginX =
            width * 0.5

        handOriginY =
            height - cardHeight
    in
    { size = size
    , handOrigin = point handOriginX handOriginY
    , cardSize =
        { vec = vec cardWidth cardHeight
        , width = cardWidth
        , height = cardHeight
        , font = cardHeight * 0.1
        }
    , centerRowOrigin = point centerRowX centerRowY
    , deckPos = deckPos
    , discardPilePos = discardPilePos
    }


inlineCSS : ViewportInfo -> Html msg
inlineCSS viewportInfo =
    let
        inlineRawCss =
            String.join "\n" <|
                [ ".card, .stack, .stack::after {"
                , " width: " ++ px viewportInfo.cardSize.width ++ ";"
                , " height: " ++ px viewportInfo.cardSize.height ++ ";"
                , " font-size: " ++ px viewportInfo.cardSize.font ++ ""
                , "}"
                ]
    in
    node "style" [] [ text <| inlineRawCss ]


deckAttrs : ViewportInfo -> CardAniAttrs msg
deckAttrs viewportInfo =
    interpolate (Settled viewportInfo.deckPos)


centerRowSpot : ViewportInfo -> Int -> Point
centerRowSpot { centerRowOrigin, cardSize } i =
    move (vec (toFloat i * centerRowCardGutterFactor * cardSize.width) 0) centerRowOrigin


isInMyHand c =
    case c.location of
        MyHand _ ->
            True

        _ ->
            False


consolidateHandCards : CardPositions -> CardsModel -> CardsModel
consolidateHandCards cpos cards =
    let
        handPos c =
            case c.location of
                MyHand i ->
                    i

                _ ->
                    -1

        withPosition i card =
            { card | location = MyHand i }

        handCards =
            Cards.filter isInMyHand cards
                |> List.sortBy handPos
                |> List.indexedMap withPosition
    in
    updateCards cpos handCards cards


isOnDiscardPile : Card -> Bool
isOnDiscardPile { location } =
    case location of
        DiscardPile _ ->
            True

        _ ->
            False


isInCenterRow : Card -> Bool
isInCenterRow { location } =
    case location of
        CenterRow _ ->
            True

        _ ->
            False


isOperandCard : Card -> Bool
isOperandCard { content } =
    case content of
        OperatorCard _ ->
            True

        _ ->
            False


isSwapCard : Card -> Bool
isSwapCard { content } =
    case content of
        SwapOperators ->
            True

        _ ->
            False


isNumberCard : Card -> Bool
isNumberCard { content } =
    case content of
        NumberCard _ ->
            True

        _ ->
            False


isInOpponentsHand : OpponentId -> Card -> Bool
isInOpponentsHand opId { location } =
    case location of
        TheirHand id _ ->
            id == opId

        _ ->
            False


inOpponentsHandCards : OpponentId -> List Card -> List Card
inOpponentsHandCards opponentId cards =
    List.filter (isInOpponentsHand opponentId) cards


numberCenterCards : Card -> Bool
numberCenterCards c =
    isInCenterRow c && isNumberCard c


operatorCenterCards : Card -> Bool
operatorCenterCards c =
    isInCenterRow c && isOperandCard c


swapCenterCards : Card -> Bool
swapCenterCards c =
    isInCenterRow c && isSwapCard c
