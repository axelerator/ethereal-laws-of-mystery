module Cards exposing (..)

import Animation exposing (Animation)
import Browser.Events exposing (onAnimationFrameDelta)
import Ease exposing (outCubic)
import Hades exposing (CardContent(..), Location(..))
import Html exposing (Attribute, Html, p)
import Html.Attributes exposing (id, style)
import Pixels
import Point2d exposing (toPixels)
import PseudoRandom
import Rectangle2d
import String exposing (fromFloat)
import Vector2d


type Msg
    = Draw
    | Discard CardId
    | GotFrame Float


type alias CardsModelDetails =
    { visuals : Visuals
    , cards : List Card
    , cardIdGen : CardId
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


type alias ViewportInfo =
    { size : Vec
    , handOrigin : Point
    , cardSize : { vec : Vec, width : Float, height : Float, font : Float }
    , centerRowOrigin : Point
    , deckPos : Props
    , discardPilePos : Props
    }


empty : Vec -> CardsModel
empty _ =
    CardsModel
        { cards = []
        , visuals = []
        , cardIdGen = 0
        }


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


draggedOver : Point -> ViewportInfo -> CardsModel -> Maybe CardId
draggedOver pointerPos viewportInfo (CardsModel { visuals }) =
    let
        isInside ( _, a ) =
            case a of
                Settled { pos } ->
                    let
                        cardRect =
                            Rectangle2d.from pos (move viewportInfo.cardSize.vec pos)
                    in
                    Rectangle2d.contains pointerPos cardRect

                Animation _ _ _ _ ->
                    False

                Vanished ->
                    False
    in
    Maybe.map (\( c, _ ) -> c.id) <| List.head <| List.filter isInside <| visuals


addCards : List ( Location, CardContent ) -> CardsModel -> CardsModel
addCards newCards model =
    let
        addCard__ : ( Location, CardContent ) -> CardsModel -> CardsModel
        addCard__ c m =
            addCardNoAni c m |> Tuple.first
    in
    List.foldr addCard__ model newCards


addCard_ : CardPositions -> Location -> CardContent -> CardsModel -> ( CardsModel, CardId )
addCard_ cardPositions location content model =
    let
        ( model_, cardId ) =
            addCardNoAni ( location, content ) model
    in
    ( updateAni cardPositions model_, cardId )


addCardNoAni : ( Location, CardContent ) -> CardsModel -> ( CardsModel, CardId )
addCardNoAni ( location, content ) (CardsModel ({ cards, cardIdGen } as rest)) =
    let
        newCard =
            { id = cardIdGen
            , location = location
            , content = content
            }

        withNewCard =
            newCard :: cards
    in
    ( CardsModel
        { rest | cards = withNewCard, cardIdGen = cardIdGen + 1 }
    , cardIdGen
    )


removeCard : CardPositions -> CardId -> CardsModel -> ( Maybe ( Card, Point ), CardsModel )
removeCard cardPositions cardId ((CardsModel ({ cards, visuals } as rest)) as model) =
    let
        ( extracted_cards, _ ) =
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
                { rest | cards = cards }
                |> updateAni cardPositions
            )

        _ ->
            ( Nothing, model )


moveCardTo : CardPositions -> CardsModel -> CardId -> Location -> CardsModel
moveCardTo cardPositions (CardsModel ({ cards } as rest)) cardId location =
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
        { rest | cards = cards_ }
        |> updateAni cardPositions


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


type alias CardPositions =
    CardsModel -> Location -> Props


updateAni : CardPositions -> CardsModel -> CardsModel
updateAni cardPositions ((CardsModel details) as model) =
    let
        screenPos =
            cardPositions model

        updatedExisting =
            List.map (updateAniFor screenPos details.cards) details.visuals

        newCards =
            findNew details.cards details.visuals

        newPos card =
            ( card, Settled <| screenPos card.location )

        newAnis =
            List.map newPos newCards

        visuals =
            List.filter (not << isVanished) <| updatedExisting ++ newAnis
    in
    CardsModel { details | visuals = visuals }


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


type alias ScreenPos =
    Location -> Props


updateAniFor : ScreenPos -> List Card -> ( Card, Animating ) -> ( Card, Animating )
updateAniFor screenPos cards ( card, ani ) =
    let
        target =
            List.head <| List.filter (hasSameIdAs card) cards
    in
    case target of
        Just targetCard ->
            if targetCard.location == card.location then
                let
                    expectedPos =
                        screenPos card.location
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
                , moveTo screenPos cards ani targetCard.location
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


contentOf : CardsModel -> CardId -> Maybe CardContent
contentOf (CardsModel { cards }) cardId =
    Maybe.map .content <| List.head <| List.filter (\c -> c.id == cardId) cards


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


moveTo : ScreenPos -> List Card -> Animating -> Location -> Animating
moveTo screenPos _ ani location =
    case ani of
        Animation speed from to t ->
            let
                from_ =
                    interpolate_ from to t
            in
            Animation speed from_ (screenPos location) 0

        Settled from ->
            Animation cardMoveSpeed from (screenPos location) 0

        Vanished ->
            Animation cardMoveSpeed (screenPos location) (screenPos location) 0


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


inOpponentsHandCards_ : OpponentId -> CardsModel -> List ( CardId, Location )
inOpponentsHandCards_ opponentId (CardsModel { cards }) =
    List.filter (isInOpponentsHand opponentId) cards
        |> List.map (\c -> ( c.id, c.location ))


numberCenterCards : Card -> Bool
numberCenterCards c =
    isInCenterRow c && isNumberCard c


operatorCenterCards : Card -> Bool
operatorCenterCards c =
    isInCenterRow c && isOperandCard c


swapCenterCards : Card -> Bool
swapCenterCards c =
    isInCenterRow c && isSwapCard c


getCards : CardsModel -> List Card
getCards (CardsModel { cards }) =
    cards


filter : (Card -> Bool) -> CardsModel -> List Card
filter predicate (CardsModel { cards }) =
    List.filter predicate cards


updateCards : CardPositions -> List Card -> CardsModel -> CardsModel
updateCards cardPositions updatedCards model =
    List.foldr updateCard_ model updatedCards
        |> updateAni cardPositions


updateCard_ : Card -> CardsModel -> CardsModel
updateCard_ card (CardsModel details) =
    let
        f c =
            if c.id == card.id then
                card

            else
                c
    in
    CardsModel { details | cards = List.map f details.cards }


idsOf : (Card -> Bool) -> CardsModel -> List CardId
idsOf predicate (CardsModel { cards }) =
    List.map .id <| List.filter predicate cards


idOf : Location -> CardsModel -> Maybe CardId
idOf location cards =
    List.head <| idsOf (\c -> c.location == location) cards


locationOf : CardsModel -> CardId -> Maybe Location
locationOf (CardsModel { cards }) cardId =
    Maybe.map .location <| List.head <| List.filter (\c -> c.id == cardId) cards


discardPileWiggle : List Float
discardPileWiggle =
    PseudoRandom.floatSequence 100 234 ( 0, 10 )


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
