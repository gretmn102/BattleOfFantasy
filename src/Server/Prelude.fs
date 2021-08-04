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

type PlayerStatus =
    | Played
    | Leaved

type LeaveResult =
    | ThisUserNotPlayed
    | PlayerLeft
    | AllPlayersLeft

type T =
    | Login of UserId * AsyncReplyChannel<Result<Client.GameState option, LoginError> Res>
    | SelectThreeCardsMove of (UserId * ThreeCards) * AsyncReplyChannel<Result<unit, MoveError> Res>
    | SelectOneAttributeMove of (UserId * AttributeId) * AsyncReplyChannel<Result<unit, MoveError> Res>
    | RestartMove of UserId * AsyncReplyChannel<Result<unit, MoveError> Res>
    | Leave of UserId * AsyncReplyChannel<LeaveResult>
    | RestartGame

type GameStage =
    | SelectThreeCardsStage of Abstr.SelectThreeCards
    | SelectAttributeStage of Abstr.SelectAttribute
    | RestartStage of Abstr.RestartFunction

type State =
    {
        Players: Map<UserId, PlayerStatus>
        GameStage: GameStage option
    }
    static member Empty =
        {
            Players = Map.empty
            GameStage = None
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
                | FinalHand (x, isRestart) ->
                    Client.FinalHand (x, isRestart)
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
            | RestartStage _ -> Client.RestartStage
    }

let maxPlayers = 3

let justReturn x =
    {
        Return = x
        PlayersMsgs = Map.empty
    }

let startGame playersMsgs state ((abstrState, f):Abstr.RestartFunction) =
    let players =
        abstrState.Players
        |> Seq.map (fun (KeyValue(k, v)) ->
            match v with
            | FinalHand(x, isRestart) ->
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
                RestartStage(abstrState, f)
                |> Some
        }
    playersMsgs, state

let beginGame playersMsgs state =
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
                    (fun playersMsgs (playerId, startHand) ->
                        let x = toClientGameState playerId abstrState gameStage
                        Map.add playerId (GameStarted x :: playersMsgs.[playerId]) playersMsgs
                    )
                    playersMsgs
            let state =
                { state with
                    GameStage = Some gameStage
                }
            playersMsgs, state
        | Abstr.SelectAttribute (_) ->
            failwith "Abstr.SelectAttribute (_)"

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
                    | RestartStage (abstrState, _) -> abstrState

                toClientGameState userId abstrState gameStage
                |> Some
                |> Ok
                |> justReturn
                |> r.Reply

                { state with Players = Map.add userId Played state.Players }
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
                        |> Map.add userId Played
                }
            if playersCount + 1 = maxPlayers then // start the game
                let playersMsgs, state = beginGame playersMsgs state

                {
                    Return = Ok None
                    PlayersMsgs = Map.map (fun _ -> List.rev) playersMsgs
                }
                |> r.Reply

                state
            else
                {
                    Return = Ok None
                    PlayersMsgs = Map.map (fun _ -> List.rev) playersMsgs
                }
                |> r.Reply

                { state with
                    Players =
                        state.Players |> Map.add userId Played
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
                | RestartStage _ as x ->
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
                | RestartStage _ as x ->
                    justReturn (Error (StrError (sprintf "expected SelectAttributeStage but %A" x)))
                    |> r.Reply

                    state
            | None ->
                r.Reply (justReturn (Error GameHasNotStartedYet))
                state
        else
            r.Reply (justReturn (Error (GetStateError YouAreNotLogin)))
            state
    | RestartMove(userId, r) ->
        if Map.containsKey userId state.Players then
            match state.GameStage with
            | Some gameStage ->
                match gameStage with
                | RestartStage(_, f) ->
                    match f userId with
                    | Right x ->
                        let playersMsgs =
                            state.Players
                            |> Map.map (fun userId' _ ->
                                [GameResponse (Restart userId)]
                            )
                        let playersMsgs, state =
                            match x with
                            | Abstr.Restart x ->
                                let playersMsgs = playersMsgs

                                let state =
                                    { state with
                                        GameStage =
                                            RestartStage x
                                            |> Some
                                    }
                                playersMsgs, state
                            | Abstr.End ->
                                beginGame playersMsgs state
                        {
                            Return = Ok ()
                            PlayersMsgs = Map.map (fun _ -> List.rev) playersMsgs
                        }
                        |> r.Reply
                        state
                    | Left err ->
                        justReturn (Error (RestartCircleError err))
                        |> r.Reply

                        state
                | SelectThreeCardsStage _
                | SelectAttributeStage _  as x ->
                    justReturn (Error (StrError (sprintf "expected SelectAttributeStage but %A" x)))
                    |> r.Reply

                    state


            | None ->
                r.Reply (justReturn (Error GameHasNotStartedYet))
                state
        else
            r.Reply (justReturn (Error (GetStateError YouAreNotLogin)))
            state
    | Leave(userId, r) ->
        if Map.containsKey userId state.Players then
            let players =
                Map.add userId Leaved state.Players
            let state =
                { state with Players = players }

            if players |> Map.forall (fun _ -> (=) Leaved) then
                AllPlayersLeft
            else
                PlayerLeft
            |> r.Reply

            state
        else
            r.Reply ThisUserNotPlayed
            state
    | RestartGame ->
        State.Empty

let m =
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
        loop State.Empty
    )
