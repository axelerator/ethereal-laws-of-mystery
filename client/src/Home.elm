module Home exposing (Model, Msg, fromBackend, init, subscriptions, update, view, withOpenMenu)

import Browser.Dom exposing (getViewport)
import Cards
    exposing
        ( Animating(..)
        , CardAniAttrs
        , CardsModel
        , Props
        , addCards
        , interpolate
        , moveCardTo
        , point
        , px
        , updateAni
        )
import Hades
    exposing
        ( CardContent(..)
        , RealmId(..)
        , ToBackend(..)
        , ToBackendEnvelope(..)
        , ToFrontend(..)
        , ToFrontendLobby(..)
        , ToLobby(..)
        , Transition(..)
        , toBackendEnvelopeEncoder
        )
import Html exposing (Attribute, Html, div, node, span, text)
import Html.Attributes exposing (attribute, class, style, width)
import Html.Events exposing (onClick)
import Http exposing (jsonBody)
import PseudoRandom
import String exposing (fromFloat, fromInt)
import Task
import WebAuthn exposing (Msg)


type Model
    = Closed ViewportInfo
    | Open
        { cards : CardsModel
        , viewportInfo : ViewportInfo
        }
    | PengingGame


type Location
    = SinglePlayerPile
    | MultiPile Int
    | Deck


type CardContent
    = SinglePlayerCard
    | DuelCard
    | TriadCard
    | QuadCard


type alias Card =
    Cards.Card Location CardContent


type alias CardsModel =
    Cards.CardsModel Location CardContent


init : ( Model, Cmd Msg )
init =
    ( Closed <| viewportInfoFor 500 500
    , Task.perform GotViewPort getViewport
    )


withOpenMenu : Float -> Float -> ( Model, Cmd Msg )
withOpenMenu =
    openMenu


subscriptions : Model -> Sub Msg
subscriptions model =
    case model of
        Open model_ ->
            Cards.subscriptions_ model_.cards GotFrame

        _ ->
            Sub.none


send : ToLobby -> Cmd Msg
send msg =
    sendToBackend <| ForRealm Lobby <| ForLobby msg


sendToBackend : ToBackendEnvelope -> Cmd Msg
sendToBackend msg =
    Http.post
        { url = "/send"
        , body = jsonBody <| toBackendEnvelopeEncoder <| msg
        , expect = Http.expectWhatever GotSendResponse
        }


fromBackend : ToFrontendLobby -> Msg
fromBackend toFrontend =
    FromBackend toFrontend


type Msg
    = GotSendResponse (Result Http.Error ())
    | FromBackend ToFrontendLobby
    | Go
    | MultiGo Int
    | GotViewPort Browser.Dom.Viewport
    | GotFrame Float
    | OpenMenu


updateFromRealm toFrontend model =
    model


update { webauthn } msg model =
    case ( msg, model ) of
        ( FromBackend toFrontend, _ ) ->
            case toFrontend of
                WaitingForMorePlayers ->
                    ( PengingGame
                    , Cmd.none
                    )

                GameStart realmId ->
                    ( model
                    , sendToBackend <| EnterRealm realmId
                    )

        ( GotFrame delta, Open model_ ) ->
            ( Open { model_ | cards = Cards.gotFrame delta model_.cards }
            , Cmd.none
            )

        ( GotSendResponse result, Open _ ) ->
            ( model, Cmd.none )

        ( Go, Open _ ) ->
            ( model
            , send StartGame
            )

        ( MultiGo numberOfPlayers, Open _ ) ->
            ( model
            , send <| WaitForGame numberOfPlayers
            )

        ( GotViewPort { scene }, Closed _ ) ->
            ( Closed <| viewportInfoFor scene.width scene.height
            , Cmd.none
            )

        ( GotViewPort { scene }, Open model_ ) ->
            let
                viewportInfo =
                    viewportInfoFor scene.width scene.height
            in
            ( Open
                { model_
                    | cards = updateAni (screenPos viewportInfo) model_.cards
                    , viewportInfo = viewportInfo
                }
            , Cmd.none
            )

        ( OpenMenu, Closed viewportInfo ) ->
            let
                cardPoss =
                    screenPos viewportInfo

                cardsOnDeck =
                    [ ( Deck, SinglePlayerCard )
                    , ( Deck, QuadCard )
                    , ( Deck, TriadCard )
                    , ( Deck, DuelCard )
                    ]

                ( withNewCards, cardsOnDeck_ ) =
                    addCards cardPoss cardsOnDeck Cards.empty

                newLocations =
                    [ SinglePlayerPile
                    , MultiPile 0
                    , MultiPile 1
                    , MultiPile 2
                    ]

                zip =
                    List.map2 Tuple.pair

                cardsWithFinalDestinations =
                    zip cardsOnDeck_ newLocations

                f ( crd, dst ) crds =
                    moveCardTo cardPoss crds crd.id dst

                withNewCardsInFinalDestinations =
                    List.foldr f withNewCards cardsWithFinalDestinations
            in
            ( Open { cards = withNewCardsInFinalDestinations, viewportInfo = viewportInfo }
            , Cmd.none
            )

        _ ->
            ( model, Cmd.none )


type alias ViewportInfo =
    { width : Float
    , height : Float
    , cardSize : { width : Float, height : Float, fontSize : Float }
    , deckPos : Props
    }


openMenu width height =
    let
        viewportInfo =
            viewportInfoFor width height

        cardPoss =
            screenPos viewportInfo

        cardsOnDeck =
            [ ( Deck, SinglePlayerCard )
            , ( Deck, QuadCard )
            , ( Deck, TriadCard )
            , ( Deck, DuelCard )
            ]

        ( withNewCards, cardsOnDeck_ ) =
            addCards cardPoss cardsOnDeck Cards.empty

        newLocations =
            [ SinglePlayerPile
            , MultiPile 0
            , MultiPile 1
            , MultiPile 2
            ]

        zip =
            List.map2 Tuple.pair

        cardsWithFinalDestinations =
            zip cardsOnDeck_ newLocations

        f ( crd, dst ) crds =
            moveCardTo cardPoss crds crd.id dst

        withNewCardsInFinalDestinations =
            List.foldr f withNewCards cardsWithFinalDestinations
    in
    ( Open { cards = withNewCardsInFinalDestinations, viewportInfo = viewportInfo }
    , Cmd.none
    )


viewportInfoFor : Float -> Float -> ViewportInfo
viewportInfoFor width height =
    let
        cardHeight =
            height / 4

        cardWidth =
            cardHeight * 0.7

        centerX =
            width * 0.5

        centerY =
            height * 0.5
    in
    { width = width
    , height = height
    , cardSize =
        { height = cardHeight
        , width = cardWidth
        , fontSize = cardHeight * 0.25
        }
    , deckPos =
        { pos = point (centerX - 0.5 * cardWidth) (centerY - cardHeight * 0.5)
        , opacity = 1.0
        , degrees = 10
        , flip = 180
        }
    }


screenPos : ViewportInfo -> CardsModel -> Location -> Props
screenPos { width, height, cardSize } _ location =
    let
        centerX =
            width * 0.5

        centerY =
            height * 0.5
    in
    case location of
        SinglePlayerPile ->
            { pos = point (centerX - 2 * cardSize.width) 100
            , opacity = 1.0
            , degrees = -10
            , flip = 0
            }

        MultiPile i ->
            { pos = point (centerX + cardSize.width + (toFloat i * cardSize.width * -0.3)) 100
            , opacity = 1.0
            , degrees = 20 + (toFloat i * -10)
            , flip = 0
            }

        Deck ->
            { pos = point (centerX - 0.5 * cardSize.width) (centerY - cardSize.height * 0.5)
            , opacity = 1.0
            , degrees = 10
            , flip = 180
            }


view : Model -> Html Msg
view model =
    div [ class "cards home" ] <|
        title
            :: cardsView model


title =
    div [ class "gameTitle" ] <|
        List.map
            (\s ->
                span
                    (if s /= "" then
                        [ class s ]

                     else
                        []
                    )
                    [ text s ]
            )
            [ "E", "thereal", " ", "L", "aws", " ", "of", " ", "M", "ystery" ]


inlineCSS : ViewportInfo -> Html msg
inlineCSS viewportInfo =
    let
        inlineRawCss =
            String.join "\n" <|
                [ ".card, .stack, .stack::after {"
                , " width: " ++ px viewportInfo.cardSize.width ++ ";"
                , " height: " ++ px viewportInfo.cardSize.height ++ ";"
                , " font-size: " ++ px viewportInfo.cardSize.fontSize ++ ";"
                , "}"
                ]
    in
    node "style" [] [ text <| inlineRawCss ]


cardsView : Model -> List (Html Msg)
cardsView model =
    case model of
        Closed viewportInfo ->
            [ inlineCSS viewportInfo, deckView viewportInfo 32 ]

        Open { cards, viewportInfo } ->
            inlineCSS viewportInfo :: Cards.viewAnis cards cardView

        _ ->
            [ text "waiting" ]


cardView : Card -> CardAniAttrs Msg -> Html Msg
cardView card ( aniAttrs, innerAttrs ) =
    div (class "card menu" :: aniAttrs)
        [ viewCardContent innerAttrs card.content ]


viewCardContent : List (Attribute Msg) -> CardContent -> Html Msg
viewCardContent aniAttrs content =
    let
        ( txt, smallTxt, imageName ) =
            case content of
                SinglePlayerCard ->
                    ( text "Solo", text "1 player", "solo" )

                DuelCard ->
                    ( text "Duel", text "2 players", "duel" )

                TriadCard ->
                    ( text "Triad", text "3 players", "duel" )

                QuadCard ->
                    ( text "Quad", text "4 players", "duel" )

        action =
            case content of
                SinglePlayerCard ->
                    onClick Go

                DuelCard ->
                    onClick <| MultiGo 2

                TriadCard ->
                    onClick <| MultiGo 3

                QuadCard ->
                    onClick <| MultiGo 4
    in
    div (action :: class "inner" :: aniAttrs)
        [ div [ class "front" ] [ div [ class "big" ] [ txt ], div [ class "mini" ] [ smallTxt ] ]
        , div [ class "back" ] []
        ]


deckView : ViewportInfo -> Int -> Html Msg
deckView viewportInfo deckSize =
    let
        ( attrs, attrsInner ) =
            interpolate (Settled viewportInfo.deckPos)

        attributes =
            [ style "zIndex" "1"
            , class <| "card menu"
            ]
                ++ attrs

        deckCard =
            div attributes [ viewCardContent attrsInner DuelCard ]
    in
    div [ class "deck-stack", onClick OpenMenu ]
        [ deckCard, stack deckSize attrs ]


stack : Int -> List (Attribute Msg) -> Html Msg
stack size attrs =
    let
        wiggles =
            PseudoRandom.floatSequence (size // 5) 234 ( 0, 20 )
    in
    div attrs <|
        List.map2
            (\i w ->
                div
                    [ class "stack"
                    , attribute "style" <| "--offset: " ++ fromInt (i * 3) ++ "px; --wiggle: " ++ fromFloat w ++ "deg"
                    ]
                    []
            )
            (List.reverse <| List.range 0 (size // 10))
            wiggles
