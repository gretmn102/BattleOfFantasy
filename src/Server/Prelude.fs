module Prelude
open Shared
open FsharpMyExtension
open FsharpMyExtension.Either
open FsharpMyExtension.ListZipperCircle2

type 'a Res =
    {
        Return: 'a
        PlayersMsgs: Map<UserId, GetStateResult<GameResponse, Client.GameState> list>
    }

type T =
    | Login of UserId * AsyncReplyChannel<Result<Client.GameState option, LoginError> Res>
    | SelectThreeCardsMove of (UserId * ThreeCards) * AsyncReplyChannel<Result<unit, MoveError> Res>
    | SelectOneAttributeMove of (UserId * AttributeId) * AsyncReplyChannel<Result<unit, MoveError> Res>

type GameStage =
    | SelectThreeCardsStage of Abstr.SelectThreeCards
    | SelectAttributeStage of Abstr.SelectAttribute
    | StartGameStage of Abstr.State
type State =
    {
        Players: Map<UserId, unit>
        GameStage: GameStage option
    }

let toClientGameState currPlayerId (abstrState:Abstr.State) (gameStage:GameStage) : Client.GameState =
    let pls = abstrState.Players
    {
        OtherPlayers =
            Map.remove currPlayerId pls
            |> Map.map (fun userId v ->
                match v with
                | StartHand _ ->
                    Client.StartHand
                | ThreeCards(_) ->
                    Client.ThreeCards
                | ChoosenAttribute _ ->
                    Client.ChoosenAttribute
                | FinalHand x ->
                    Client.FinalHand x
            )

        ClientPlayer =
            let p = pls.[currPlayerId]

            p

        AttributesDeckCount =
            abstrState.AttributesDeck.Length
        CharactersDeckCount =
            abstrState.CharactersDeck.Length
        MoveStage =
            match gameStage with
            | SelectThreeCardsStage _ -> Client.SelectThreeCardsStage
            | SelectAttributeStage _ -> Client.SelectAttributeStage
            | StartGameStage _ -> Client.StartGameStage
    }

let maxPlayers = 3

let justReturn x =
    {
        Return = x
        PlayersMsgs = Map.empty
    }

let startGame playersMsgs state (abstrState:Abstr.State) =
    let players =
        abstrState.Players
        |> Seq.map (fun (KeyValue(k, v)) ->
            match v with
            | FinalHand x ->
                k, x
            | x -> failwithf "Expected FinalHand but %A" x
        )
        |> List.ofSeq

    let playersMsgs =
        playersMsgs
        |> Map.map (fun playerId msgs ->
            GameResponse (StartGame players)::msgs
        )
    let state =
        { state with
            GameStage =
                StartGameStage abstrState
                |> Some
        }
    playersMsgs, state

let exec state = function
    | Login(userId, r) ->
        let playersCount = Map.count state.Players
        if Map.containsKey userId state.Players then

            match state.GameStage with
            | Some gameStage ->
                let abstrState =
                    match gameStage with
                    | SelectThreeCardsStage(abstrState, _)
                    | SelectAttributeStage(abstrState, _)
                    | StartGameStage abstrState ->  abstrState

                toClientGameState userId abstrState gameStage
                |> Some
                |> Ok
                |> justReturn
                |> r.Reply
            | _ ->
                justReturn (Ok None)
                |> r.Reply

            state
        elif playersCount >= maxPlayers then
            justReturn (Error PlayersRecruited)
            |> r.Reply

            state
        else
            let playersMsgs =
                state.Players
                |> Map.map (fun _ v ->
                    [PlayerJoined userId]
                )
                |> Map.add userId []
            let state =
                { state with
                    Players =
                        state.Players
                        |> Map.add userId ()
                }
            if playersCount + 1 = maxPlayers then // start the game
                let playersMsgs, gameStage =
                    let playersIds =
                        state.Players
                        |> Seq.map (fun (KeyValue(playerId, _)) -> playerId)
                        |> List.ofSeq

                    match Abstr.start playersIds with
                    | Abstr.Begin (xs, f) ->
                        match f with
                        | Abstr.WaitSelectThreeCards (abstrState, f) ->
                            let gameStage =
                                SelectThreeCardsStage (abstrState, f)
                            let playersMsgs =
                                xs
                                |> List.fold
                                    (fun playerMsgs (playerId, startHand) ->
                                        let x = toClientGameState playerId abstrState gameStage
                                        Map.add playerId (GameStarted x :: playerMsgs.[playerId]) playerMsgs
                                    )
                                    playersMsgs
                            playersMsgs, gameStage
                        | Abstr.SelectAttribute (_) ->
                            failwith "Abstr.SelectAttribute (_)"

                {
                    Return = Ok None
                    PlayersMsgs = Map.map (fun _ -> List.rev) playersMsgs
                }
                |> r.Reply

                { state with
                    GameStage = Some gameStage
                }
            else
                {
                    Return = Ok None
                    PlayersMsgs = Map.map (fun _ -> List.rev) playersMsgs
                }
                |> r.Reply

                { state with
                    Players =
                        state.Players |> Map.add userId ()
                }
    | SelectThreeCardsMove((userId, threeCards), r) ->
        if Map.containsKey userId state.Players then
            match state.GameStage with
            | Some gameStage ->
                match gameStage with
                | SelectThreeCardsStage (_, f) ->
                    match f (userId, threeCards) with
                    | Right x ->
                        let playersMsgs =
                            state.Players
                            |> Map.map (fun userId' _ ->
                                [GameResponse (SelectedThreeCards userId)]
                            )
                        let playersMsgs, state =
                            match x with
                            | Abstr.WaitSelectThreeCards x ->
                                let playersMsgs = playersMsgs

                                let state =
                                    { state with
                                        GameStage =
                                            SelectThreeCardsStage x
                                            |> Some
                                    }
                                playersMsgs, state
                            | Abstr.SelectAttribute x ->
                                match x with
                                | Abstr.WaitSelectAttribute x ->
                                    let playersMsgs =
                                        playersMsgs
                                        |> Map.map (fun playerId msgs ->
                                            GameResponse StartSelectAttribute::msgs
                                        )
                                    let state =
                                        { state with
                                            GameStage =
                                                SelectAttributeStage x
                                                |> Some
                                        }
                                    playersMsgs, state
                                | Abstr.StartGame abstrState -> startGame playersMsgs state abstrState

                        {
                            Return = Ok ()
                            PlayersMsgs = Map.map (fun _ -> List.rev) playersMsgs
                        }
                        |> r.Reply
                        state
                    | Left err ->
                        justReturn (Error (MovError err))
                        |> r.Reply

                        state
                | SelectAttributeStage _
                | StartGameStage _ as x ->
                    justReturn (Error (StrError (sprintf "expected SelectThreeCardsStage but %A" x)))
                    |> r.Reply

                    state
            | None ->
                r.Reply (justReturn (Error GameHasNotStartedYet))
                state
        else
            r.Reply (justReturn (Error (GetStateError YouAreNotLogin)))
            state
    | SelectOneAttributeMove((userId, attributeId), r) ->
        if Map.containsKey userId state.Players then
            match state.GameStage with
            | Some gameStage ->
                match gameStage with
                | SelectAttributeStage(_, f) ->
                    match f (userId, attributeId) with
                    | Right x ->
                        let playersMsgs =
                            state.Players
                            |> Map.map (fun userId' _ ->
                                [GameResponse (SelectedOneAttribute userId)]
                            )
                        let playersMsgs, state =
                            match x with
                            | Abstr.WaitSelectAttribute x ->
                                let playersMsgs = playersMsgs

                                let state =
                                    { state with
                                        GameStage =
                                            SelectAttributeStage x
                                            |> Some
                                    }
                                playersMsgs, state
                            | Abstr.StartGame abstrState -> startGame playersMsgs state abstrState
                        {
                            Return = Ok ()
                            PlayersMsgs = Map.map (fun _ -> List.rev) playersMsgs
                        }
                        |> r.Reply
                        state
                    | Left err ->
                        justReturn (Error (MovError err))
                        |> r.Reply

                        state
                | SelectThreeCardsStage _
                | StartGameStage _ as x ->
                    justReturn (Error (StrError (sprintf "expected SelectAttributeStage but %A" x)))
                    |> r.Reply

                    state
            | None ->
                r.Reply (justReturn (Error GameHasNotStartedYet))
                state
        else
            r.Reply (justReturn (Error (GetStateError YouAreNotLogin)))
            state

let m =
    let st =
        {
            Players = Map.empty
            GameStage = None
        }
    MailboxProcessor.Start (fun mail ->
        let rec loop (st:State) =
            async {
                let! res = mail.Receive()
                let st =
                    try
                        exec st res
                    with err ->
                        printfn "%A" err
                        st

                return! loop st
            }
        loop st
    )
