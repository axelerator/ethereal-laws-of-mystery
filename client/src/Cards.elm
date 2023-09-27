module Cards exposing (..)

import Browser.Events exposing (onAnimationFrameDelta)
import Ease exposing (outCubic)
import Html exposing (Attribute, Html, p)
import Html.Attributes exposing (id, style)
import Pixels
import Point2d exposing (toPixels)
import Rectangle2d
import String exposing (fromFloat)
import Vector2d


type CardId
    = CardId Int


type alias Card location cardContent =
    { id : CardId
    , location : location
    , content : cardContent
    }


type Msg
    = Draw
    | Discard CardId
    | GotFrame Float


type alias CardsModelDetails location cardContent =
    { visuals : Visuals location cardContent
    , cards : List (Card location cardContent)
    , cardIdGen : Int
    }


type CardsModel location cardContent
    = CardsModel (CardsModelDetails location cardContent)


type alias OpponentId =
    Int


type alias Model location cardContent =
    { count : Int
    , idGen : Int
    , animatedCards : CardsModel location cardContent
    }


type alias ViewportInfo =
    { size : Vec
    , handOrigin : Point
    , cardSize : { vec : Vec, width : Float, height : Float, font : Float }
    , centerRowOrigin : Point
    , deckPos : Props
    , discardPilePos : Props
    , numOfOpponents : Int
    }


isEmpty : CardsModel location cardContent -> Bool
isEmpty (CardsModel { cards }) =
    List.isEmpty cards


empty : CardsModel location cardContent
empty =
    CardsModel
        { cards = []
        , visuals = []
        , cardIdGen = 0
        }


subscriptions_ : CardsModel location cardContent -> (Float -> msg) -> Sub msg
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


insideOfCard : Point -> ViewportInfo -> CardsModel location cardContent -> Maybe CardId
insideOfCard pointerPos viewportInfo (CardsModel { visuals }) =
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


addCards :
    CardPositions location cardContent
    -> List ( location, cardContent )
    -> CardsModel location cardContent
    -> ( CardsModel location cardContent, List (Card location cardContent) )
addCards cps newCards model =
    let
        addCard__ c ( m, cards ) =
            let
                ( m_, cid ) =
                    addCardNoAni c m
            in
            ( m_, cid :: cards )
        (m__, cids) = List.foldr addCard__ ( model, [] ) newCards
            
    in
      ( updateAni cps m__
      , cids
      )


addCard :
    CardPositions location cardContent
    -> location
    -> cardContent
    -> CardsModel location cardContent
    -> ( CardsModel location cardContent, Card location cardContent )
addCard cardPositions location content model =
    let
        ( model_, card ) =
            addCardNoAni ( location, content ) model
    in
    ( updateAni cardPositions model_, card )


addCardNoAni :
    ( location, cardContent )
    -> CardsModel location cardContent
    -> ( CardsModel location cardContent, Card location cardContent )
addCardNoAni ( location, content ) (CardsModel ({ cards, cardIdGen } as rest)) =
    let
        newId =
            CardId cardIdGen

        newCard =
            { id = newId
            , location = location
            , content = content
            }

        withNewCard =
            newCard :: cards
    in
    ( CardsModel
        { rest | cards = withNewCard, cardIdGen = cardIdGen + 1 }
    , newCard
    )


removeCard :
    CardPositions location cardContent
    -> CardId
    -> CardsModel location cardContent
    -> ( Maybe ( Card location cardContent, Point ), CardsModel location cardContent )
removeCard cardPositions cardId ((CardsModel ({ cards, visuals } as rest)) as model) =
    let
        ( extracted_cards, restCards) =
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
                { rest | cards = restCards }
                |> updateAni cardPositions
            )

        _ ->
            ( Nothing, model )


moveCardTo :
    CardPositions location cardContent
    -> CardsModel location cardContent
    -> CardId
    -> location
    -> CardsModel location cardContent
moveCardTo cardPositions (CardsModel ({ cards } as rest)) cardId location =
    let
        updatelocation card =
            if card.id == cardId then
                { card | location = location }

            else
                card

        cards_ =
            List.map updatelocation cards
    in
    CardsModel
        { rest | cards = cards_ }
        |> updateAni cardPositions


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


type alias Visuals location cardContent =
    List ( Card location cardContent, Animating )


type alias CardPositions location cardContent =
    CardsModel location cardContent -> location -> Props


updateAni :
    CardPositions location cardContent
    -> CardsModel location cardContent
    -> CardsModel location cardContent
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


isVanished : ( Card location cardContent, Animating ) -> Bool
isVanished ( _, a ) =
    case a of
        Vanished ->
            True

        _ ->
            False


findNew : List (Card location cardContent) -> Visuals location cardContent -> List (Card location cardContent)
findNew cards visuals =
    let
        cardsInVisuals =
            List.map (\( c, _ ) -> c) visuals

        isNew card =
            List.isEmpty <| List.filter (hasSameIdAs card) cardsInVisuals
    in
    List.filter isNew cards


hasSameIdAs : Card location cardContent -> Card location cardContent -> Bool
hasSameIdAs { id } card =
    id == card.id


type alias ScreenPos location =
    location -> Props


updateAniFor :
    ScreenPos location
    -> List (Card location cardContent)
    -> ( Card location cardContent, Animating )
    -> ( Card location cardContent, Animating )
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


contentOf : CardsModel location cardContent -> CardId -> Maybe cardContent
contentOf (CardsModel { cards }) cardId =
    Maybe.map .content <| List.head <| List.filter (\c -> c.id == cardId) cards


revealContent : CardsModel location cardContent -> CardId -> cardContent -> CardsModel location cardContent
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


moveTo : ScreenPos location -> List (Card location cardContent) -> Animating -> location -> Animating
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


getCards : CardsModel location cardContent -> List (Card location cardContent)
getCards (CardsModel { cards }) =
    cards


filter : (Card location cardContent -> Bool) -> CardsModel location cardContent -> List (Card location cardContent)
filter predicate (CardsModel { cards }) =
    List.filter predicate cards


updateCards : CardPositions location cardContent -> List (Card location cardContent) -> CardsModel location cardContent -> CardsModel location cardContent
updateCards cardPositions updatedCards model =
    List.foldr updateCard_ model updatedCards
        |> updateAni cardPositions


updateCard_ : Card location cardContent -> CardsModel location cardContent -> CardsModel location cardContent
updateCard_ card (CardsModel details) =
    let
        f c =
            if c.id == card.id then
                card

            else
                c
    in
    CardsModel { details | cards = List.map f details.cards }


idsOf : (Card location cardContent -> Bool) -> CardsModel location cardContent -> List CardId
idsOf predicate (CardsModel { cards }) =
    List.map .id <| List.filter predicate cards


idOf : location -> CardsModel location cardContent -> Maybe CardId
idOf location cards =
    List.head <| idsOf (\c -> c.location == location) cards


locationOf : CardsModel location cardContent -> CardId -> Maybe location
locationOf (CardsModel { cards }) cardId =
    Maybe.map .location <| List.head <| List.filter (\c -> c.id == cardId) cards


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


gotFrame : Float -> CardsModel location cardContent -> CardsModel location cardContent
gotFrame delta (CardsModel model) =
    CardsModel
        { model
            | visuals = List.filter (not << isVanished) <| List.map (animate delta) model.visuals
        }


animate : Float -> ( Card location cardContent, Animating ) -> ( Card location cardContent, Animating )
animate delta ( c, a ) =
    ( c, step (delta * 0.001) a )


px : Float -> String
px x =
    String.fromFloat x ++ "px"



-- carries CSS attrs for the card and the roation of the inner


type alias CardAniAttrs msg =
    ( List (Attribute msg), List (Attribute msg) )


viewAni :
    (Card location cardContent -> CardAniAttrs msg -> Html msg)
    -> ( Card location cardContent, Animating )
    -> Html msg
viewAni viewCard ( { id, content } as card, a ) =
    viewCard card <| interpolate a


viewAnis :
    CardsModel location cardContent
    -> (Card location cardContent -> CardAniAttrs msg -> Html msg)
    -> List (Html msg)
viewAnis (CardsModel { visuals }) viewCard =
    List.map (viewAni viewCard) visuals
