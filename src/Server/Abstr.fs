module Abstr
open FsharpMyExtension
open FsharpMyExtension.ListZipperCircle2
open FsharpMyExtension.Either
open Shared

// Игроки заходят — им раздается по три карты из колоды атрибутов и три из колоды персонажей.
// Они выбирают одну карту персонажа и две карты атрибутов, одну из которых подкидывают сопернику
// Дальше карты вскрываются
type State = {
    Players: Map<PlayerId, PlayerStage>
    AttributesDeck: AttributeId list
    CharactersDeck: CharacterId list
}

type Begin =
    | Begin of (PlayerId * StartHand) list * SelectThreeCardsCircle
and SelectThreeCards = State * (PlayerId * ThreeCards -> Either<SelectAttributeError, SelectThreeCardsCircle>)
and SelectThreeCardsCircle =
    | WaitSelectThreeCards of SelectThreeCards
    | SelectAttribute of SelectAttributeCircle
and SelectAttribute = State * (PlayerId * AttributeId -> Either<SelectAttributeError, SelectAttributeCircle>)
and SelectAttributeCircle =
    | WaitSelectAttribute of SelectAttribute
    | StartGame of RestartFunction
and RestartFunction = State * (PlayerId -> Either<RestartCircleError, RestartCircle>)
and RestartCircle =
    | Restart of RestartFunction
    | End

module List =
    let splitAt' n xs =
        let rec f n acc xs =
            if n > 0 then
                match xs with
                | x::xs ->
                    f (n - 1) (x::acc) xs
                | [] ->
                    n, (List.rev acc, [])
            else
                0, (List.rev acc, xs)
        f n [] xs
    let splitAtTests () =
        [
            splitAt' 0 [] = (0, ([], []))
            splitAt' 3 [1] = (2, ([1], []))
            splitAt' 3 [1; 2] = (1, ([1; 2], []))
            splitAt' 3 [1; 2; 3] = (0, ([1; 2; 3], []))
            splitAt' 3 [1..10] = (0, ([1; 2; 3], [4; 5; 6; 7; 8; 9; 10]))
        ] |> List.forall id

let restartFunction startingGame (state:State) =
    state, fun playerId ->
        match Map.tryFind playerId state.Players with
        | Some stage ->
            match stage with
            | FinalHand (hand, isRestart) ->
                if isRestart then
                    Left YouHaveAlreadySelectedRestart
                else
                    let state =
                        { state with
                            Players =
                                Map.add playerId (FinalHand (hand, true)) state.Players
                        }

                    Right (startingGame state)
            | x ->
                failwithf "Internal error:\nExpected FinalHand but actual %A" x
        | None ->
            Left RestartCircleError.YouDontPlay
    : RestartFunction

let rec startingGame (state:State) =
    let isContinue =
        state.Players
        |> Map.forall (fun _ ->
            function
            | FinalHand(_, startAgain) -> startAgain
            | x -> failwithf "Internal error:\nExpected FinalHand but actual %A" x
        )
        |> not
    if isContinue then
        Restart (restartFunction startingGame state)
    else
        End

let rec selectAttribute (state:State) =
    let isContinue =
        state.Players
        |> Map.forall (fun _ ->
            function
            | ThreeCards _ -> false
            | ChoosenAttribute _ -> true
            | x -> failwithf "Internal error:\nExpected ThreeCards or ChoosenAttribute but actual %A" x
        )
        |> not
    if isContinue then
        WaitSelectAttribute(state, fun (playerId, givenAttribute) ->
            match Map.tryFind playerId state.Players with
            | Some stage ->
                match stage with
                | ThreeCards threeCards ->
                    if threeCards.Attribute1 = givenAttribute || threeCards.Attribute2 = givenAttribute then
                        let hand =
                            {
                                RetainedCharacter = threeCards.Character
                                RetainedAttribute =
                                    if threeCards.Attribute1 = givenAttribute then
                                        threeCards.Attribute2
                                    else
                                        threeCards.Attribute1
                                GivenAttribute = givenAttribute
                            }
                        let state =
                            { state with
                                Players =
                                    Map.add playerId (ChoosenAttribute hand) state.Players
                            }
                        Right (selectAttribute state)
                    else
                        Left YouDontHaveThisAttribute
                | ChoosenAttribute _ -> Left YouHaveAlreadySelectedAttribute
                | x -> failwithf "Internal error:\nExpected ThreeCards or ChoosenAttribute but actual %A" x
            | None ->
                Left YouDontPlay
        )
    else
        let len = state.Players.Count
        let xs = Array.zeroCreate len

        let players =
            state.Players
            |> Seq.map (fun (KeyValue(playerId, x)) ->
                match x with
                | ChoosenAttribute hand ->
                    playerId, hand
                | StartHand(_)
                | ThreeCards(_)
                | FinalHand(_) as x -> failwithf "Expected ChoosenAttribute but %A" x
            )
            |> Array.ofSeq
        players
        |> Array.iteri (fun i (playerId, x) ->
            let (p2Id, p2Hand) = players.[(i + 1) % len]
            let x =
                {
                    Attribute1 = p2Hand.RetainedAttribute
                    Character = p2Hand.RetainedCharacter

                    Attribute2 = x.GivenAttribute
                }
            xs.[(i + 1) % len] <- p2Id, FinalHand (x, false)
        )
        let state =
            { state with
                Players =
                    xs |> Map.ofArray
            }
        StartGame (restartFunction startingGame state)


let rec selectThreeCardsNext (state:State) =
    let isContinue =
        state.Players
        |> Map.forall (fun _ ->
            function
            | StartHand _ -> false
            | ThreeCards _ -> true
            | x -> failwithf "Internal error:\nExpected StartHand or ThreeCards but actual %A" x
        )
        |> not
    if isContinue then
        WaitSelectThreeCards(state, fun (playerId, threeCards) ->
            match Map.tryFind playerId state.Players with
            | Some stage ->
                match stage with
                | StartHand startHand ->
                    if threeCards.Attribute1 = threeCards.Attribute2 then
                        Left TwoEqualAttributes
                    else
                        let isValid =
                            List.contains threeCards.Attribute1 startHand.Attributes
                            && List.contains threeCards.Attribute2 startHand.Attributes
                            && List.contains threeCards.Character startHand.Characters
                        if isValid then
                            let state =
                                { state with
                                    Players =
                                        Map.add playerId (ThreeCards threeCards) state.Players
                                }
                            Right (selectThreeCardsNext state)
                        else
                            Left YouDontHaveThisAttribute
                | ThreeCards(_) -> Left YouHaveAlreadySelectedAttribute
                | x -> failwithf "Internal error:\nExpected StartHand or ThreeCards but actual %A" x
            | None ->
                Left YouDontPlay
        )
    else
        SelectAttribute(selectAttribute state)

let start players =
    let state =
        {
            Players = Map.empty
            AttributesDeck = Init.attributes |> List.map fst |> List.shuffle
            CharactersDeck = Init.characters |> List.map fst |> List.shuffle
        }
    let startHands, state =
        players
        |> List.mapFold (fun state playerId ->
            let _, (atts, attsDeck) = List.splitAt' 3 state.AttributesDeck
            let _, (chars, charsDeck) = List.splitAt' 3 state.CharactersDeck

            let startHand =
                {
                    Attributes = atts
                    Characters = chars
                }
            let state =
                { state with
                    Players =
                        Map.add playerId (StartHand startHand) state.Players
                    AttributesDeck = attsDeck
                    CharactersDeck = charsDeck
                }
            (playerId, startHand), state
        ) state
    Begin(startHands, selectThreeCardsNext state)
