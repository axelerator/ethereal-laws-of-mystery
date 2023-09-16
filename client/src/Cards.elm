module Cards exposing (..)

import Browser.Events exposing (onAnimationFrameDelta)
import Ease exposing (outCubic)
import Html exposing (Attribute, Html, div, p, text)
import Html.Attributes exposing (class, id, style)
import Html.Events exposing (onClick)
import List exposing (length, range)
import Maybe.Extra exposing (values)
import Pixels
import Point2d exposing (toPixels)
import String exposing (fromFloat)
import Vector2d
import Hades exposing (CardContent(..))


type Msg
    = Draw
    | Discard CardId
    | GotFrame Float


type alias CardsModelDetails =
    { visuals : Visuals
    , cards : List Card
    }


type CardsModel
    = CardsModel CardsModelDetails


type alias Model =
    { count : Int
    , idGen : Int
    , animatedCards : CardsModel
    }


init_ : CardsModel
init_ =
    CardsModel
        { cards = []
        , visuals = []
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


addCard : CardsModel -> CardId -> Card -> Location -> CardContent -> CardsModel
addCard (CardsModel { cards, visuals }) newId _ location content =
    let
        newCard =
            { id = newId
            , location = location
            , content = content
            }

        withNewCard =
            newCard :: cards

        visualsWithoutPriorAnis =
          List.filter (\(c, _) -> c.id /= newId) visuals

        withNewCardOnDeck =
            updateAni withNewCard visualsWithoutPriorAnis
    in
    CardsModel
        { cards = withNewCard
        , visuals = withNewCardOnDeck
        }


removeCard : CardId -> CardsModel -> ( Maybe (Card, Point), CardsModel )
removeCard cardId ((CardsModel { cards, visuals }) as model) =
    let
        ( extracted_cards, cards_ ) =
            List.partition ((==) cardId << .id) cards
        visual = List.head <| List.filter ((==) cardId << .id << Tuple.first) visuals 

        pos a =
            case a of
                Just (_, (Animation _ from to t)) ->
                    Just <| .pos <| interpolate_ from to t

                Just (_, (Settled p)) ->
                    Just p.pos

                _ -> Nothing
        
    in
    case (List.head extracted_cards, pos visual) of
        (Just card, Just p) -> 
          ( Just (card, p)
          , CardsModel
              { cards = cards_, visuals = updateAni cards_ visuals }
          )
        _ -> (Nothing, model)


moveCardTo : CardsModel -> CardId -> Location -> CardsModel
moveCardTo (CardsModel { cards, visuals }) cardId location =
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
        { cards = cards_
        , visuals = updateAni cards_ visuals
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
    }


type alias Visuals =
    List ( Card, Animating )


updateAni : List Card -> Visuals -> Visuals
updateAni cards visuals =
    let
        updatedExisting =
            List.map (updateAniFor cards) visuals

        newCards =
            findNew cards visuals

        newPos card =
            ( card, Settled <| screenPos cards card.location )

        newAnis =
            List.map newPos newCards
    in
    updatedExisting ++ newAnis


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


updateAniFor : List Card -> ( Card, Animating ) -> ( Card, Animating )
updateAniFor cards ( card, ani ) =
    let
        target =
            List.head <| List.filter (hasSameIdAs card) cards

    in
    case target of
        Just targetCard ->
            if targetCard.location == card.location then
                let
                    expectedPos =
                        screenPos cards card.location
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
                , moveTo cards ani targetCard.location
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


moveTo : List Card -> Animating -> Location -> Animating
moveTo cards ani location =
    case ani of
        Animation speed from to t ->
            let
                from_ =
                    interpolate_ from to t
            in
            Animation speed from_ (screenPos cards location) 0

        Settled from ->
            Animation cardMoveSpeed from (screenPos cards location) 0

        Vanished ->
            Animation cardMoveSpeed deckPos (screenPos cards location) 0


deckPos : Props
deckPos =
    { pos = point 20 100
    , degrees = 0
    , opacity = 1.0
    }


discardPilePos =
    { pos = point 320 100
    , degrees = 90
    , opacity = 1.0
    }


isInMyHand : Card -> Bool
isInMyHand { location } =
    case location of
        MyHand _ ->
            True

        _ ->
            False


myHandCards : List Card -> List Card
myHandCards cards =
    List.filter isInMyHand cards


screenPos : List Card -> Location -> Props
screenPos cards loc =
    case loc of
        Deck ->
            deckPos

        DiscardPile ->
            discardPilePos

        MyHand p ->
            let
                handCardCount =
                    List.length <| myHandCards cards

                degreePerCard =
                    maxSpread / toFloat handCardCount

                totalWidth =
                    times (toFloat handCardCount) offsetPerCard

                left =
                    move (times -0.5 totalWidth) handOrigin
            in
            { pos = move (times (toFloat p) offsetPerCard) left
            , opacity = 1.0
            , degrees = startSpread + degreePerCard * toFloat p
            }
        InFlight p ->
            { pos = p
            , opacity = 1.0
            , degrees = 10
            }
        CenterRow i ->
            { pos = centerRowSpot i
            , opacity = 1.0
            , degrees = 10
            }

centerRowSpot i =
  move (vec (toFloat i * 1.5 * cardWidth) 0) centerRowOrigin
cardWidth = 100
centerRowOrigin = point 200 100


type alias CardId =
    Int


type Location
    = Deck
    | MyHand Int
    | DiscardPile
    | InFlight Point
    | CenterRow Int


type alias Card =
    { id : CardId
    , location : Location
    , content : CardContent
    }


type Animating
    = Animation Float Props Props Float
    | Settled Props
    | Vanished


cssTransforms : Props -> List (Attribute msg)
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
    in
    [ style "transform" transforms
    , style "opacity" <| fromFloat props.opacity
    ]


interpolate : Animating -> List (Attribute msg)
interpolate a =
    case a of
        Animation _ from to t ->
            cssTransforms <| interpolate_ from to t

        Settled p ->
            cssTransforms p

        Vanished ->
            [ style "display" "none" ]


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
    in
    { pos = pos
    , opacity = opacity
    , degrees = degrees
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


handOrigin =
    point 300 500


maxSpread =
    35


startSpread =
    maxSpread * -0.5


offsetPerCard =
    vec 50 0


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
consolidateHandCards (CardsModel { cards, visuals }) =
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
        , visuals = updateAni cards_ visuals
        }


gotFrame : Float -> CardsModel -> CardsModel
gotFrame delta (CardsModel model) =
    CardsModel
        { model
            | visuals = List.map (animate delta) model.visuals
        }


animate : Float -> ( Card, Animating ) -> ( Card, Animating )
animate delta ( c, a ) =
    ( c, step (delta * 0.001) a )


px : Float -> String
px x =
    String.fromFloat x ++ "px"


viewAni : (Card -> List (Attribute msg) -> Html msg) -> ( Card, Animating ) -> Html msg
viewAni viewCard ( { id, content } as card, a ) =
    viewCard card <| interpolate a


viewAnis : CardsModel -> (Card -> List (Attribute msg) -> Html msg) -> List (Html msg)
viewAnis (CardsModel { visuals }) viewCard =
     List.reverse <| List.map (viewAni viewCard) visuals
