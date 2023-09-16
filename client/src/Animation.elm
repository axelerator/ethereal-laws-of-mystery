module Animation exposing (Animation)

import Pixels
import Point2d


type Screen
    = Screen


type alias Point =
    Point2d.Point2d Pixels.Pixels Screen


type Animation
    = Animation AniDetails
    | Settled SettledDetails
    | Vanished


type alias AniDetails =
    { positions : List (KeyFrames Point)
    }

type alias SettledDetails =
    { positions : Point
    }

type AnimatableAttr
    = APos
    | ARot


type alias KeyFrames a =
    { offset : Int
    , currentFrame : a
    , remainingFrames : List a
    , terminated : Bool
    }

stepKf : KeyFrames a -> ( KeyFrames a, Bool )
stepKf kf =
    if kf.offset > 0 then
        ( { kf | offset = kf.offset - 1 }, False )

    else if kf.terminated then
        ( kf, True )

    else
        case kf.remainingFrames of
            [] ->
                ( { kf | terminated = True }, False )

            f :: fs ->
                ({ kf | currentFrame = f, remainingFrames = fs }, False)
