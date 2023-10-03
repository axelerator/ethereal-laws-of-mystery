
-- generated by elm_rs


module Hades exposing (..)

import Dict exposing (Dict)
import Http
import Json.Decode
import Json.Encode
import Url.Builder


resultEncoder : (e -> Json.Encode.Value) -> (t -> Json.Encode.Value) -> (Result e t -> Json.Encode.Value)
resultEncoder errEncoder okEncoder enum =
    case enum of
        Ok inner ->
            Json.Encode.object [ ( "Ok", okEncoder inner ) ]
        Err inner ->
            Json.Encode.object [ ( "Err", errEncoder inner ) ]


resultDecoder : Json.Decode.Decoder e -> Json.Decode.Decoder t -> Json.Decode.Decoder (Result e t)
resultDecoder errDecoder okDecoder =
    Json.Decode.oneOf
        [ Json.Decode.map Ok (Json.Decode.field "Ok" okDecoder)
        , Json.Decode.map Err (Json.Decode.field "Err" errDecoder)
        ]


type RealmId
    = Lobby
    | Realm (String)


realmIdEncoder : RealmId -> Json.Encode.Value
realmIdEncoder enum =
    case enum of
        Lobby ->
            Json.Encode.string "Lobby"
        Realm inner ->
            Json.Encode.object [ ( "Realm", Json.Encode.string inner ) ]

type ToBackendEnvelope
    = ForRealm (RealmId) (ToBackend)
    | EnterRealm (RealmId)


toBackendEnvelopeEncoder : ToBackendEnvelope -> Json.Encode.Value
toBackendEnvelopeEncoder enum =
    case enum of
        ForRealm t0 t1 ->
            Json.Encode.object [ ( "ForRealm", Json.Encode.list identity [ realmIdEncoder t0, toBackendEncoder t1 ] ) ]
        EnterRealm inner ->
            Json.Encode.object [ ( "EnterRealm", realmIdEncoder inner ) ]

type ToBackend
    = ForLobby (ToLobby)
    | ForGame (ToGame)


toBackendEncoder : ToBackend -> Json.Encode.Value
toBackendEncoder enum =
    case enum of
        ForLobby inner ->
            Json.Encode.object [ ( "ForLobby", toLobbyEncoder inner ) ]
        ForGame inner ->
            Json.Encode.object [ ( "ForGame", toGameEncoder inner ) ]

type ToLobby
    = StartGame
    | WaitForGame (Int)


toLobbyEncoder : ToLobby -> Json.Encode.Value
toLobbyEncoder enum =
    case enum of
        StartGame ->
            Json.Encode.string "StartGame"
        WaitForGame inner ->
            Json.Encode.object [ ( "WaitForGame", Json.Encode.int inner ) ]

type ToGame
    = DrawFromPile
    | Play (Int) (Int)
    | LeaveGame
    | RestartGame


toGameEncoder : ToGame -> Json.Encode.Value
toGameEncoder enum =
    case enum of
        DrawFromPile ->
            Json.Encode.string "DrawFromPile"
        Play t0 t1 ->
            Json.Encode.object [ ( "Play", Json.Encode.list identity [ Json.Encode.int t0, Json.Encode.int t1 ] ) ]
        LeaveGame ->
            Json.Encode.string "LeaveGame"
        RestartGame ->
            Json.Encode.string "RestartGame"

type Location
    = Deck
    | MyHand (Int)
    | TheirHand (Int) (Int)
    | DiscardPile (Int)
    | InFlight (Float) (Float)
    | InFlightOpen (Float) (Float)
    | CenterRow (Int)


locationEncoder : Location -> Json.Encode.Value
locationEncoder enum =
    case enum of
        Deck ->
            Json.Encode.string "Deck"
        MyHand inner ->
            Json.Encode.object [ ( "MyHand", Json.Encode.int inner ) ]
        TheirHand t0 t1 ->
            Json.Encode.object [ ( "TheirHand", Json.Encode.list identity [ Json.Encode.int t0, Json.Encode.int t1 ] ) ]
        DiscardPile inner ->
            Json.Encode.object [ ( "DiscardPile", Json.Encode.int inner ) ]
        InFlight t0 t1 ->
            Json.Encode.object [ ( "InFlight", Json.Encode.list identity [ Json.Encode.float t0, Json.Encode.float t1 ] ) ]
        InFlightOpen t0 t1 ->
            Json.Encode.object [ ( "InFlightOpen", Json.Encode.list identity [ Json.Encode.float t0, Json.Encode.float t1 ] ) ]
        CenterRow inner ->
            Json.Encode.object [ ( "CenterRow", Json.Encode.int inner ) ]

type alias LoginCredentials =
    { username : String
    , password : String
    }


loginCredentialsEncoder : LoginCredentials -> Json.Encode.Value
loginCredentialsEncoder struct =
    Json.Encode.object
        [ ( "username", (Json.Encode.string) struct.username )
        , ( "password", (Json.Encode.string) struct.password )
        ]


type alias RegisterCredentials =
    { username : String
    , password : String
    }


registerCredentialsEncoder : RegisterCredentials -> Json.Encode.Value
registerCredentialsEncoder struct =
    Json.Encode.object
        [ ( "username", (Json.Encode.string) struct.username )
        , ( "password", (Json.Encode.string) struct.password )
        ]


realmIdDecoder : Json.Decode.Decoder RealmId
realmIdDecoder = 
    Json.Decode.oneOf
        [ Json.Decode.string
            |> Json.Decode.andThen
                (\x ->
                    case x of
                        "Lobby" ->
                            Json.Decode.succeed Lobby
                        unexpected ->
                            Json.Decode.fail <| "Unexpected variant " ++ unexpected
                )
        , Json.Decode.map Realm (Json.Decode.field "Realm" (Json.Decode.string))
        ]

type ToFrontendEnvelope
    = Noop
    | Unauthorized
    | FromRealm (ToFrontend)


toFrontendEnvelopeDecoder : Json.Decode.Decoder ToFrontendEnvelope
toFrontendEnvelopeDecoder = 
    Json.Decode.oneOf
        [ Json.Decode.string
            |> Json.Decode.andThen
                (\x ->
                    case x of
                        "Noop" ->
                            Json.Decode.succeed Noop
                        unexpected ->
                            Json.Decode.fail <| "Unexpected variant " ++ unexpected
                )
        , Json.Decode.string
            |> Json.Decode.andThen
                (\x ->
                    case x of
                        "Unauthorized" ->
                            Json.Decode.succeed Unauthorized
                        unexpected ->
                            Json.Decode.fail <| "Unexpected variant " ++ unexpected
                )
        , Json.Decode.map FromRealm (Json.Decode.field "FromRealm" (toFrontendDecoder))
        ]

type ToFrontend
    = ToLobbyFrontend (ToFrontendLobby)
    | ToGameFrontend (Transition)
    | EnteredGame (RealmId) (GameInfo)


toFrontendDecoder : Json.Decode.Decoder ToFrontend
toFrontendDecoder = 
    Json.Decode.oneOf
        [ Json.Decode.map ToLobbyFrontend (Json.Decode.field "ToLobbyFrontend" (toFrontendLobbyDecoder))
        , Json.Decode.map ToGameFrontend (Json.Decode.field "ToGameFrontend" (transitionDecoder))
        , Json.Decode.field "EnteredGame" (Json.Decode.succeed EnteredGame |> Json.Decode.andThen (\x -> Json.Decode.index 0 (realmIdDecoder) |> Json.Decode.map x) |> Json.Decode.andThen (\x -> Json.Decode.index 1 (gameInfoDecoder) |> Json.Decode.map x))
        ]

type ToFrontendLobby
    = GameStart (RealmId)
    | WaitingForMorePlayers


toFrontendLobbyDecoder : Json.Decode.Decoder ToFrontendLobby
toFrontendLobbyDecoder = 
    Json.Decode.oneOf
        [ Json.Decode.map GameStart (Json.Decode.field "GameStart" (realmIdDecoder))
        , Json.Decode.string
            |> Json.Decode.andThen
                (\x ->
                    case x of
                        "WaitingForMorePlayers" ->
                            Json.Decode.succeed WaitingForMorePlayers
                        unexpected ->
                            Json.Decode.fail <| "Unexpected variant " ++ unexpected
                )
        ]

type Transition
    = IDraw (CardContent)
    | TheyDraw (Int) (Int)
    | IPlayed (Location)
    | TheyPlayed (Int) (Location) (Location) (CardContent) (Int)
    | IWon
    | ILost
    | TurnChanged (Int)
    | GameEnded (RealmId)
    | Shuffle (Int)


transitionDecoder : Json.Decode.Decoder Transition
transitionDecoder = 
    Json.Decode.oneOf
        [ Json.Decode.map IDraw (Json.Decode.field "IDraw" (cardContentDecoder))
        , Json.Decode.field "TheyDraw" (Json.Decode.succeed TheyDraw |> Json.Decode.andThen (\x -> Json.Decode.index 0 (Json.Decode.int) |> Json.Decode.map x) |> Json.Decode.andThen (\x -> Json.Decode.index 1 (Json.Decode.int) |> Json.Decode.map x))
        , Json.Decode.map IPlayed (Json.Decode.field "IPlayed" (locationDecoder))
        , Json.Decode.field "TheyPlayed" (Json.Decode.succeed TheyPlayed |> Json.Decode.andThen (\x -> Json.Decode.index 0 (Json.Decode.int) |> Json.Decode.map x) |> Json.Decode.andThen (\x -> Json.Decode.index 1 (locationDecoder) |> Json.Decode.map x) |> Json.Decode.andThen (\x -> Json.Decode.index 2 (locationDecoder) |> Json.Decode.map x) |> Json.Decode.andThen (\x -> Json.Decode.index 3 (cardContentDecoder) |> Json.Decode.map x) |> Json.Decode.andThen (\x -> Json.Decode.index 4 (Json.Decode.int) |> Json.Decode.map x))
        , Json.Decode.string
            |> Json.Decode.andThen
                (\x ->
                    case x of
                        "IWon" ->
                            Json.Decode.succeed IWon
                        unexpected ->
                            Json.Decode.fail <| "Unexpected variant " ++ unexpected
                )
        , Json.Decode.string
            |> Json.Decode.andThen
                (\x ->
                    case x of
                        "ILost" ->
                            Json.Decode.succeed ILost
                        unexpected ->
                            Json.Decode.fail <| "Unexpected variant " ++ unexpected
                )
        , Json.Decode.map TurnChanged (Json.Decode.field "TurnChanged" (Json.Decode.int))
        , Json.Decode.map GameEnded (Json.Decode.field "GameEnded" (realmIdDecoder))
        , Json.Decode.map Shuffle (Json.Decode.field "Shuffle" (Json.Decode.int))
        ]

type CardContent
    = NumberCard (Int)
    | OperatorCard (Operator)
    | SwapOperators


cardContentDecoder : Json.Decode.Decoder CardContent
cardContentDecoder = 
    Json.Decode.oneOf
        [ Json.Decode.map NumberCard (Json.Decode.field "NumberCard" (Json.Decode.int))
        , Json.Decode.map OperatorCard (Json.Decode.field "OperatorCard" (operatorDecoder))
        , Json.Decode.string
            |> Json.Decode.andThen
                (\x ->
                    case x of
                        "SwapOperators" ->
                            Json.Decode.succeed SwapOperators
                        unexpected ->
                            Json.Decode.fail <| "Unexpected variant " ++ unexpected
                )
        ]

type Operator
    = Plus
    | Minus
    | Times


operatorDecoder : Json.Decode.Decoder Operator
operatorDecoder = 
    Json.Decode.oneOf
        [ Json.Decode.string
            |> Json.Decode.andThen
                (\x ->
                    case x of
                        "Plus" ->
                            Json.Decode.succeed Plus
                        unexpected ->
                            Json.Decode.fail <| "Unexpected variant " ++ unexpected
                )
        , Json.Decode.string
            |> Json.Decode.andThen
                (\x ->
                    case x of
                        "Minus" ->
                            Json.Decode.succeed Minus
                        unexpected ->
                            Json.Decode.fail <| "Unexpected variant " ++ unexpected
                )
        , Json.Decode.string
            |> Json.Decode.andThen
                (\x ->
                    case x of
                        "Times" ->
                            Json.Decode.succeed Times
                        unexpected ->
                            Json.Decode.fail <| "Unexpected variant " ++ unexpected
                )
        ]

type alias GameInfo =
    { pileSize : Int
    , discardPile : List (CardContent)
    , center : List (CardContent)
    , hand : List (CardContent)
    , gameState : GameState
    , opponents : List (Opponent)
    , turn : Int
    , myName : String
    }


gameInfoDecoder : Json.Decode.Decoder GameInfo
gameInfoDecoder =
    Json.Decode.succeed GameInfo
        |> Json.Decode.andThen (\x -> Json.Decode.map x (Json.Decode.field "pile_size" (Json.Decode.int)))
        |> Json.Decode.andThen (\x -> Json.Decode.map x (Json.Decode.field "discard_pile" (Json.Decode.list (cardContentDecoder))))
        |> Json.Decode.andThen (\x -> Json.Decode.map x (Json.Decode.field "center" (Json.Decode.list (cardContentDecoder))))
        |> Json.Decode.andThen (\x -> Json.Decode.map x (Json.Decode.field "hand" (Json.Decode.list (cardContentDecoder))))
        |> Json.Decode.andThen (\x -> Json.Decode.map x (Json.Decode.field "game_state" (gameStateDecoder)))
        |> Json.Decode.andThen (\x -> Json.Decode.map x (Json.Decode.field "opponents" (Json.Decode.list (opponentDecoder))))
        |> Json.Decode.andThen (\x -> Json.Decode.map x (Json.Decode.field "turn" (Json.Decode.int)))
        |> Json.Decode.andThen (\x -> Json.Decode.map x (Json.Decode.field "my_name" (Json.Decode.string)))


locationDecoder : Json.Decode.Decoder Location
locationDecoder = 
    Json.Decode.oneOf
        [ Json.Decode.string
            |> Json.Decode.andThen
                (\x ->
                    case x of
                        "Deck" ->
                            Json.Decode.succeed Deck
                        unexpected ->
                            Json.Decode.fail <| "Unexpected variant " ++ unexpected
                )
        , Json.Decode.map MyHand (Json.Decode.field "MyHand" (Json.Decode.int))
        , Json.Decode.field "TheirHand" (Json.Decode.succeed TheirHand |> Json.Decode.andThen (\x -> Json.Decode.index 0 (Json.Decode.int) |> Json.Decode.map x) |> Json.Decode.andThen (\x -> Json.Decode.index 1 (Json.Decode.int) |> Json.Decode.map x))
        , Json.Decode.map DiscardPile (Json.Decode.field "DiscardPile" (Json.Decode.int))
        , Json.Decode.field "InFlight" (Json.Decode.succeed InFlight |> Json.Decode.andThen (\x -> Json.Decode.index 0 (Json.Decode.float) |> Json.Decode.map x) |> Json.Decode.andThen (\x -> Json.Decode.index 1 (Json.Decode.float) |> Json.Decode.map x))
        , Json.Decode.field "InFlightOpen" (Json.Decode.succeed InFlightOpen |> Json.Decode.andThen (\x -> Json.Decode.index 0 (Json.Decode.float) |> Json.Decode.map x) |> Json.Decode.andThen (\x -> Json.Decode.index 1 (Json.Decode.float) |> Json.Decode.map x))
        , Json.Decode.map CenterRow (Json.Decode.field "CenterRow" (Json.Decode.int))
        ]

type GameState
    = Running
    | GameOver (Bool)


gameStateDecoder : Json.Decode.Decoder GameState
gameStateDecoder = 
    Json.Decode.oneOf
        [ Json.Decode.string
            |> Json.Decode.andThen
                (\x ->
                    case x of
                        "Running" ->
                            Json.Decode.succeed Running
                        unexpected ->
                            Json.Decode.fail <| "Unexpected variant " ++ unexpected
                )
        , Json.Decode.map GameOver (Json.Decode.field "GameOver" (Json.Decode.bool))
        ]

type alias Opponent =
    { name : String
    , handSize : Int
    }


opponentDecoder : Json.Decode.Decoder Opponent
opponentDecoder =
    Json.Decode.succeed Opponent
        |> Json.Decode.andThen (\x -> Json.Decode.map x (Json.Decode.field "name" (Json.Decode.string)))
        |> Json.Decode.andThen (\x -> Json.Decode.map x (Json.Decode.field "hand_size" (Json.Decode.int)))


type LoginCredentialsResponse
    = SuccessfullyLoggedInWithCreds
    | LoginWithCredsNotFound


loginCredentialsResponseDecoder : Json.Decode.Decoder LoginCredentialsResponse
loginCredentialsResponseDecoder = 
    Json.Decode.oneOf
        [ Json.Decode.string
            |> Json.Decode.andThen
                (\x ->
                    case x of
                        "SuccessfullyLoggedInWithCreds" ->
                            Json.Decode.succeed SuccessfullyLoggedInWithCreds
                        unexpected ->
                            Json.Decode.fail <| "Unexpected variant " ++ unexpected
                )
        , Json.Decode.string
            |> Json.Decode.andThen
                (\x ->
                    case x of
                        "LoginWithCredsNotFound" ->
                            Json.Decode.succeed LoginWithCredsNotFound
                        unexpected ->
                            Json.Decode.fail <| "Unexpected variant " ++ unexpected
                )
        ]

type RegisterCredentialsResponse
    = SuccessfullyRegisteredWithCreds
    | RegisteredWithCredsError (String)


registerCredentialsResponseDecoder : Json.Decode.Decoder RegisterCredentialsResponse
registerCredentialsResponseDecoder = 
    Json.Decode.oneOf
        [ Json.Decode.string
            |> Json.Decode.andThen
                (\x ->
                    case x of
                        "SuccessfullyRegisteredWithCreds" ->
                            Json.Decode.succeed SuccessfullyRegisteredWithCreds
                        unexpected ->
                            Json.Decode.fail <| "Unexpected variant " ++ unexpected
                )
        , Json.Decode.map RegisteredWithCredsError (Json.Decode.field "RegisteredWithCredsError" (Json.Decode.string))
        ]

