module Cards exposing (..)

import Animation exposing (Animation)
import Browser.Events exposing (onAnimationFrameDelta)
import Ease exposing (outCubic)
import Hades exposing (CardContent(..), GameInfo, Location(..), Opponent)
import Html exposing (Attribute, Html, div, node, p, text)
import Html.Attributes exposing (class, id, style)
import Html.Events exposing (onClick)
import Json.Decode exposing (bool)
import List exposing (length, range)
import Maybe.Extra exposing (values)
import Pixels
import Point2d exposing (toPixels)
import Rectangle2d exposing (Rectangle2d)
import String exposing (fromFloat)
import Vector2d


type Msg
    = Draw
    | Discard CardId
    | GotFrame Float


type alias CardsModelDetails =
    { visuals : Visuals
    , cards : List Card
    , viewportInfo : ViewportInfo
    }


type alias ViewportInfo =
    { size : Vec
    , handOrigin : Point
    , cardSize : { vec : Vec, width : Float, height : Float, font : Float }
    , centerRowOrigin : Point
    , deckPos : Props
    , discardPilePos : Props
    }


type CardsModel
    = CardsModel CardsModelDetails


type alias OpponentId =
    Int


type alias Model =
    { count : Int
    , idGen : Int
    , animatedCards : CardsModel
    }


init_ : Vec -> GameInfo -> (CardsModel, CardId)
init_ viewportSize { center, hand, opponents } =
    let
        centerCount =
            List.length center

        mkCard : Int -> CardContent -> Card
        mkCard i content =
            { id = i
            , location = CenterRow i
            , content = content
            }

        centerCards =
            List.indexedMap mkCard center

        mkHandCard i content =
            { id = i + centerCount
            , location = MyHand i
            , content = content
            }

        handCards =
            List.indexedMap mkHandCard hand

        handCount =
            List.length hand

        viewportInfo =
            viewportInfoFor viewportSize

        mkOpponentCard opId handPos cardId =
            { id = cardId
            , location = TheirHand opId handPos
            , content = NumberCard 1
            }

        mkOpponentCards : OpponentId -> Int -> Int -> ( List Card, CardId )
        mkOpponentCards opId lastId numberOfCards =
            ( List.indexedMap (mkOpponentCard opId) <| List.range lastId (lastId + numberOfCards - 1)
            , lastId + numberOfCards
            )

        forOp : ( OpponentId, Opponent ) -> ( List Card, CardId ) -> ( List Card, CardId )
        forOp ( opId, opponent ) ( crds, lastId ) =
            let
                ( crds_, lastId_ ) =
                    mkOpponentCards opId lastId opponent.handSize
            in
            ( crds ++ crds_, lastId_ )

        (opponentCards, finalId) =
            List.foldr forOp ( [], centerCount + handCount ) <|
                List.map2 (\i o -> ( i, o ))
                    (List.range 1 <| List.length opponents)
                    opponents

        cards =
            centerCards ++ handCards ++ opponentCards

        visuals =
            updateAni viewportInfo cards []
    in
    (CardsModel
        { cards = cards
        , visuals = visuals
        , viewportInfo = viewportInfo
        }
    , finalId + 1
    )


subscriptions_ : CardsModel -> (Float -> msg) -> Sub msg
subscriptions_ (CardsModel { visuals }) gotFrame_ =
    let
        isAnimating ( _, a ) =
            case a of
                Animation _ _ _ _ ->
                    True

                _ ->
                    False

        hasAnimating =
            List.any isAnimating visuals
    in
    if hasAnimating then
        onAnimationFrameDelta gotFrame_

    else
        Sub.none


updateViewport : Vec -> CardsModel -> CardsModel
updateViewport viewportSize (CardsModel details) =
    let
        viewportInfo =
            viewportInfoFor viewportSize

        withUpdatedViewportInfo =
            updateAni viewportInfo details.cards details.visuals
    in
    CardsModel
        { details
            | viewportInfo = viewportInfo
            , visuals = withUpdatedViewportInfo
        }


inlineCSS : CardsModel -> Html msg
inlineCSS (CardsModel { viewportInfo }) =
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


centerRowCardGutterFactor =
    1.15


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


centerRowSpot : ViewportInfo -> Int -> Point
centerRowSpot { centerRowOrigin, cardSize } i =
    move (vec (toFloat i * centerRowCardGutterFactor * cardSize.width) 0) centerRowOrigin


draggedOver : Point -> CardsModel -> Maybe CardId
draggedOver pointerPos (CardsModel { viewportInfo, visuals }) =
    let
        isInside ( _, a ) =
            case a of
                Settled { pos } ->
                    let
                        cardRect =
                            Rectangle2d.from pos (move viewportInfo.cardSize.vec pos)
                    in
                    Rectangle2d.contains pointerPos cardRect

                Animation _ from to t ->
                    False

                Vanished ->
                    False
    in
    Maybe.map (\( c, _ ) -> c.id) <| List.head <| List.filter isInside <| visuals


deckAttrs : CardsModel -> CardAniAttrs msg
deckAttrs (CardsModel { viewportInfo }) =
    interpolate (Settled viewportInfo.deckPos)


addCard : CardsModel -> CardId -> Card -> Location -> CardContent -> CardsModel
addCard (CardsModel ({ cards, visuals, viewportInfo } as rest)) newId _ location content =
    let
        newCard =
            { id = newId
            , location = location
            , content = content
            }

        withNewCard =
            newCard :: cards

        visualsWithoutPriorAnis =
            List.filter (\( c, _ ) -> c.id /= newId) visuals

        withNewCardOnDeck =
            updateAni viewportInfo withNewCard visualsWithoutPriorAnis
    in
    CardsModel
        { rest
            | cards = withNewCard
            , visuals = withNewCardOnDeck
        }


removeCard : CardId -> CardsModel -> ( Maybe ( Card, Point ), CardsModel )
removeCard cardId ((CardsModel ({ cards, visuals, viewportInfo } as rest)) as model) =
    let
        ( extracted_cards, cards_ ) =
            List.partition ((==) cardId << .id) cards

        visual =
            List.head <| List.filter ((==) cardId << .id << Tuple.first) visuals

        pos a =
            case a of
                Just ( _, Animation _ from to t ) ->
                    Just <| .pos <| interpolate_ from to t

                Just ( _, Settled p ) ->
                    Just p.pos

                _ ->
                    Nothing
    in
    case ( List.head extracted_cards, pos visual ) of
        ( Just card, Just p ) ->
            ( Just ( card, p )
            , CardsModel
                { rest | cards = cards_, visuals = updateAni viewportInfo cards_ visuals }
            )

        _ ->
            ( Nothing, model )


moveCardTo : CardsModel -> CardId -> Location -> CardsModel
moveCardTo (CardsModel ({ cards, visuals, viewportInfo } as rest)) cardId location =
    let
        updateLocation card =
            if card.id == cardId then
                { card | location = location }

            else
                card

        cards_ =
            List.map updateLocation cards
    in
    CardsModel
        { rest
            | cards = cards_
            , visuals = updateAni viewportInfo cards_ visuals
        }


fold : (Card -> b -> b) -> b -> CardsModel -> b
fold f b (CardsModel { cards }) =
    List.foldr f b cards


type ScreenSpace
    = ScreenSpace


type alias Point =
    Point2d.Point2d Pixels.Pixels ScreenSpace


type alias Vec =
    Vector2d.Vector2d Pixels.Pixels ScreenSpace


move : Vec -> Point -> Point
move =
    Point2d.translateBy


times : Float -> Vec -> Vec
times =
    Vector2d.scaleBy


point : Float -> Float -> Point
point =
    Point2d.pixels


vec : Float -> Float -> Vec
vec =
    Vector2d.pixels


type alias Props =
    { pos : Point
    , degrees : Float
    , opacity : Float
    , flip : Float
    }


type alias Visuals =
    List ( Card, Animating )


updateAni : ViewportInfo -> List Card -> Visuals -> Visuals
updateAni viewportInfo cards visuals =
    let
        updatedExisting =
            List.map (updateAniFor viewportInfo cards) visuals

        newCards =
            findNew cards visuals

        newPos card =
            ( card, Settled <| screenPos viewportInfo cards card.location )

        newAnis =
            List.map newPos newCards
    in
    List.filter (not << isVanished) <| updatedExisting ++ newAnis


isVanished : ( Card, Animating ) -> Bool
isVanished ( _, a ) =
    case a of
        Vanished ->
            True

        _ ->
            False


findNew : List Card -> Visuals -> List Card
findNew cards visuals =
    let
        cardsInVisuals =
            List.map (\( c, _ ) -> c) visuals

        isNew card =
            List.isEmpty <| List.filter (hasSameIdAs card) cardsInVisuals
    in
    List.filter isNew cards


hasSameIdAs : Card -> Card -> Bool
hasSameIdAs { id } card =
    id == card.id


updateAniFor : ViewportInfo -> List Card -> ( Card, Animating ) -> ( Card, Animating )
updateAniFor viewportInfo cards ( card, ani ) =
    let
        target =
            List.head <| List.filter (hasSameIdAs card) cards
    in
    case target of
        Just targetCard ->
            if targetCard.location == card.location then
                let
                    expectedPos =
                        screenPos viewportInfo cards card.location
                in
                case ani of
                    Vanished ->
                        ( targetCard
                        , Settled expectedPos
                        )

                    Settled current ->
                        if current == expectedPos then
                            ( card
                            , ani
                            )

                        else
                            ( targetCard
                            , Animation cardMoveSpeed current expectedPos 0
                            )

                    Animation speed from to t ->
                        if to == expectedPos then
                            ( targetCard
                            , ani
                            )

                        else
                            let
                                newCurrent =
                                    interpolate_ from to t
                            in
                            ( targetCard
                            , Animation speed newCurrent expectedPos 0
                            )

            else
                ( targetCard
                , moveTo viewportInfo cards ani targetCard.location
                )

        Nothing ->
            ( card
            , vanish ani
            )


vanish ani =
    case ani of
        Vanished ->
            Vanished

        Settled from ->
            Animation cardMoveSpeed from { from | opacity = 0 } 0

        Animation _ from to t ->
            let
                from_ =
                    interpolate_ from to t
            in
            Animation cardMoveSpeed from_ { from_ | opacity = 0 } 0


cardMoveSpeed =
    2.0


revealContent : CardsModel -> CardId -> CardContent -> CardsModel
revealContent (CardsModel ({ cards, visuals } as details)) cardId content =
    let
        updateCard c =
            if c.id == cardId then
                { c | content = content }

            else
                c

        cards_ =
            List.map updateCard cards

        updateVisual (( c, a ) as tpl) =
            if c.id == cardId then
                ( { c | content = content }, a )

            else
                tpl

        visuals_ =
            List.map updateVisual visuals
    in
    CardsModel
        { details
            | cards = cards_
            , visuals = visuals_
        }


moveTo : ViewportInfo -> List Card -> Animating -> Location -> Animating
moveTo viewportInfo cards ani location =
    case ani of
        Animation speed from to t ->
            let
                from_ =
                    interpolate_ from to t
            in
            Animation speed from_ (screenPos viewportInfo cards location) 0

        Settled from ->
            Animation cardMoveSpeed from (screenPos viewportInfo cards location) 0

        Vanished ->
            Animation cardMoveSpeed viewportInfo.deckPos (screenPos viewportInfo cards location) 0


isInMyHand : Card -> Bool
isInMyHand { location } =
    case location of
        MyHand _ ->
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


isInInflight : Card -> Bool
isInInflight { location } =
    case location of
        InFlightOpen _ _ ->
            True

        InFlight _ _ ->
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


myHandCards : List Card -> List Card
myHandCards cards =
    List.filter isInMyHand cards

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


idsOf : (Card -> Bool) -> CardsModel -> List CardId
idsOf predicate (CardsModel { cards }) =
    List.map .id <| List.filter predicate cards


idOf : Location -> CardsModel -> Maybe CardId
idOf location cards =
    List.head <| idsOf (\c -> c.location == location) cards


locationOf : CardsModel -> CardId -> Maybe Location
locationOf (CardsModel { cards }) cardId =
    Maybe.map .location <| List.head <| List.filter (\c -> c.id == cardId) cards


screenPos : ViewportInfo -> List Card -> Location -> Props
screenPos viewportInfo cards loc =
    case loc of
        Deck ->
            viewportInfo.deckPos

        DiscardPile ->
            viewportInfo.discardPilePos

        MyHand p ->
            let
                handCardCount =
                    List.length <| myHandCards cards

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
                      List.length <| inOpponentsHandCards opponentId cards

                  degreePerCard =
                      maxSpread / toFloat handCardCount

                  totalHeight =
                      times (toFloat handCardCount) offsetPerCardV

                  (_, viewportHeight) =
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
                      List.length <| inOpponentsHandCards opponentId cards

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
              let
                  handCardCount =
                      List.length <| myHandCards cards

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


type alias CardId =
    Int


type alias Card =
    { id : CardId
    , location : Location
    , content : CardContent
    }


type Animating
    = Animation Float Props Props Float
    | Settled Props
    | Vanished


cssTransforms : Props -> CardAniAttrs msg
cssTransforms props =
    let
        { x, y } =
            toPixels props.pos

        transforms =
            String.join "" <|
                [ "translate("
                , px x
                , ","
                , px y
                , ")"
                , " "
                , "rotate("
                , fromFloat props.degrees
                , "deg)"
                ]

        innerTransforms =
            String.join "" <|
                [ "rotateY("
                , fromFloat props.flip
                , "deg)"
                ]
    in
    ( [ style "transform" transforms
      , style "opacity" <| fromFloat props.opacity
      ]
    , [ style "transform" innerTransforms ]
    )


interpolate : Animating -> CardAniAttrs msg
interpolate a =
    case a of
        Animation _ from to t ->
            cssTransforms <| interpolate_ from to t

        Settled p ->
            cssTransforms p

        Vanished ->
            ( [ style "display" "none" ], [] )


interpolate_ : Props -> Props -> Float -> Props
interpolate_ from to tRaw =
    let
        t =
            outCubic tRaw

        opacity =
            from.opacity + ((to.opacity - from.opacity) * t)

        direction =
            Vector2d.from from.pos to.pos

        pos =
            move (times t direction) from.pos

        degrees =
            from.degrees + ((to.degrees - from.degrees) * t)

        flip =
            from.flip + ((to.flip - from.flip) * t)
    in
    { pos = pos
    , opacity = opacity
    , degrees = degrees
    , flip = flip
    }


step : Float -> Animating -> Animating
step delta a =
    case a of
        Animation speed from to t ->
            let
                t_ =
                    t + delta * speed
            in
            if t_ > 1.0 then
                Settled to

            else
                Animation speed from to t_

        Settled ps ->
            if ps.opacity == 0.0 then
                Vanished

            else
                a

        Vanished ->
            Vanished


maxSpread =
    35


startSpread =
    maxSpread * -0.5


offsetPerCard =
    vec 50 0
offsetPerCardV =
    vec 0 50



lastHandId : Card -> Int -> Int
lastHandId { location } p =
    case location of
        MyHand p_ ->
            if p > p_ then
                p

            else
                p_

        _ ->
            p


consolidateHandCards : CardsModel -> CardsModel
consolidateHandCards (CardsModel { cards, visuals, viewportInfo }) =
    let
        ( handCards, otherCards ) =
            List.partition isInMyHand cards

        getPos c =
            case c.location of
                MyHand p ->
                    Just ( p, c )

                _ ->
                    Nothing

        orderedByPos : List ( Int, Card )
        orderedByPos =
            List.sortBy Tuple.first <| values <| List.map getPos handCards

        properPositions =
            range 0 <| length handCards - 1

        resetPosition ( _, c ) p =
            { c | location = MyHand p }

        cards_ =
            otherCards ++ List.map2 resetPosition orderedByPos properPositions
    in
    CardsModel
        { cards = cards_
        , visuals = updateAni viewportInfo cards_ visuals
        , viewportInfo = viewportInfo
        }


gotFrame : Float -> CardsModel -> CardsModel
gotFrame delta (CardsModel model) =
    CardsModel
        { model
            | visuals = List.filter (not << isVanished) <| List.map (animate delta) model.visuals
        }


animate : Float -> ( Card, Animating ) -> ( Card, Animating )
animate delta ( c, a ) =
    ( c, step (delta * 0.001) a )


px : Float -> String
px x =
    String.fromFloat x ++ "px"



-- carries CSS attrs for the card and the roation of the inner


type alias CardAniAttrs msg =
    ( List (Attribute msg), List (Attribute msg) )


viewAni : (Card -> CardAniAttrs msg -> Html msg) -> ( Card, Animating ) -> Html msg
viewAni viewCard ( { id, content } as card, a ) =
    viewCard card <| interpolate a


viewAnis : CardsModel -> (Card -> CardAniAttrs msg -> Html msg) -> List (Html msg)
viewAnis (CardsModel { visuals }) viewCard =
    List.map (viewAni viewCard) visuals
