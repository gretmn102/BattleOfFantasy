module Index

open Elmish
open Elmish.Bridge
open Shared

type Deferred<'t> =
    | HasNotStartedYet
    | InProgress
    | Resolved of 't

type Connection =
    | Disconnected
    | Waiting
    | Connected of Result<unit, LoginError>

type ThreeCardsTemplate =
    {
        SelectedAttribute1: AttributeId option
        SelectedAttribute2: AttributeId option
        SelectedCharacter: CharacterId option
    }
    static member toThreeCards x =
        {
            Attribute1 = x.SelectedAttribute1.Value
            Attribute2 = x.SelectedAttribute2.Value
            Character = x.SelectedCharacter.Value
        }
type State =
    {
        PlayerId: UserId
        Connection: Connection
        GameLog: int * Map<int, string>
        GameState: Client.GameState option
        PlayersTable: {| Remain:int; OtherPlayers:UserId Set |}
        ConnectedUsers : User list
        NotificationsVisible: bool

        ThreeCardsTemplate: ThreeCardsTemplate
        SelectOneAttribute: AttributeId option
    }

type ThreeCardsTemplateMsg =
    | SelectAttribute1 of AttributeId
    | DeselectAttribute1
    | SelectAttribute2 of AttributeId
    | DeselectAttribute2
    | SelectCharacter of CharacterId
    | DeselectCharacter

type Msg =
    | RC of RemoteClientMsg
    | ConnectionLost

    | Login
    | ChangeUserId of string

    | RemoveNotify of int

    | ThreeCardsTemplateMsg of ThreeCardsTemplateMsg
    | ThreeCardsMove

    | SelectOneAttribute of AttributeId
    | DeselectOneAttribute
    | SelectOneAttributeMove
let init(): State * Cmd<Msg> =
    let state =
        {
            PlayerId = ""
            GameLog = 0, Map.empty
            GameState = None
            PlayersTable = {| Remain = 0; OtherPlayers = Set.empty |}
            Connection = Disconnected
            ConnectedUsers = []
            NotificationsVisible = false

            ThreeCardsTemplate =
                {
                    SelectedAttribute1 = None
                    SelectedAttribute2 = None
                    SelectedCharacter = None
                }
            SelectOneAttribute = None
        }
    state, Cmd.none

let removeNotifyMs = 1 * 60 * 1000

let update (msg: Msg) (state: State): State * Cmd<Msg> =
    match msg with
    | RC rc ->
        match rc with
        | LoginResult res ->
            let state =
                match res with
                | Ok gameState ->
                    { state with
                        Connection = Connected (Ok ())
                        GameState = gameState
                    }
                | Error err ->
                    { state with
                        Connection = Connected (Error err)
                    }
            state, Cmd.none
        | QueryConnected ->
            match state.Connection with
            | Connected(Ok _) ->
                { Name = state.PlayerId; Color = Black }
                |> SetUser
                |> Bridge.Send
            | Waiting | Disconnected _ | Connected _ -> ()

            state, Cmd.bridgeSend UsersConnected
        | GameMsgs msgs ->
            let gameLogId, gameLog = state.GameLog
            let (gameLogId', gameLog), state =
                msgs
                |> List.fold
                    (fun ((gameLogId, gameLog), (state:State)) x ->
                        let gameLog, state =
                            match x with
                            | GameStarted gameState ->
                                let state =
                                    { state with
                                        GameState = gameState |> Some }
                                (gameLogId, gameLog), state
                            | WaitPlayers count ->
                                let state =
                                    { state with
                                        PlayersTable = {| state.PlayersTable with Remain = count |}
                                    }
                                (gameLogId, gameLog), state
                            | PlayerJoined userId ->
                                let state =
                                    { state with
                                        PlayersTable =
                                            let playersTable = state.PlayersTable
                                            {| playersTable with OtherPlayers = Set.remove userId playersTable.OtherPlayers |}
                                    }
                                (gameLogId, gameLog), state
                            | PlayerLeaved userId ->
                                let state =
                                    { state with
                                        PlayersTable =
                                            let playersTable = state.PlayersTable
                                            {| playersTable with OtherPlayers = Set.add userId playersTable.OtherPlayers |}
                                    }
                                (gameLogId, gameLog), state
                            | GameResponse x ->
                                let gameState =
                                    match state.GameState with
                                    | Some gameState -> gameState
                                    | None ->
                                        failwith "GameState was null"
                                match x with
                                | SelectedThreeCards playerId ->
                                    let state =
                                        if playerId = state.PlayerId then
                                            { state with
                                                GameState =
                                                    { gameState with
                                                        ClientPlayer =
                                                            let threeCardsTemplate = state.ThreeCardsTemplate

                                                            let threeCards =
                                                                ThreeCardsTemplate.toThreeCards threeCardsTemplate

                                                            ThreeCards threeCards
                                                    }
                                                    |> Some
                                            }
                                        else
                                            { state with
                                                GameState =
                                                    { gameState with
                                                        OtherPlayers =
                                                            Map.add
                                                                playerId
                                                                Client.ThreeCards
                                                                gameState.OtherPlayers

                                                    }
                                                    |> Some
                                            }
                                    // match gameState.ClientPlayer with
                                    // (gameLogId + 1, Map.add gameLogId (sprintf "%A" x) gameLog)
                                    (gameLogId, gameLog), state
                                | StartSelectAttribute ->
                                    let state =
                                        { state with
                                            GameState =
                                                { gameState with
                                                    MoveStage = Client.SelectAttributeStage
                                                }
                                                |> Some
                                        }
                                    (gameLogId, gameLog), state

                                | StartGame xs ->
                                    let state =
                                        { state with
                                            GameState =
                                                { gameState with
                                                    MoveStage = Client.StartGameStage
                                                }
                                                |> Some
                                        }
                                    let state =
                                        xs
                                        |> List.fold (fun state (playerId, cards) ->
                                            if playerId = state.PlayerId then
                                                { state with
                                                    GameState =
                                                        { gameState with
                                                            ClientPlayer =
                                                                FinalHand cards
                                                        }
                                                        |> Some
                                                }
                                            else
                                                { state with
                                                    GameState =
                                                        { gameState with
                                                            OtherPlayers =
                                                                Map.add
                                                                    playerId
                                                                    (Client.FinalHand cards)
                                                                    gameState.OtherPlayers
                                                        }
                                                        |> Some
                                                }
                                        ) state

                                    (gameLogId, gameLog), state
                                | SelectedOneAttribute playerId ->
                                    let state =
                                        if playerId = state.PlayerId then
                                            { state with
                                                GameState =
                                                    { gameState with
                                                        ClientPlayer =
                                                            let threeCardsTemplate = state.ThreeCardsTemplate

                                                            let threeCards =
                                                                ThreeCardsTemplate.toThreeCards threeCardsTemplate
                                                            let givenAttribute = state.SelectOneAttribute.Value
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
                                                            ChoosenAttribute hand
                                                    }
                                                    |> Some
                                            }
                                        else
                                            { state with
                                                GameState =
                                                    { gameState with
                                                        OtherPlayers =
                                                            Map.add
                                                                playerId
                                                                Client.ChoosenAttribute
                                                                gameState.OtherPlayers
                                                    }
                                                    |> Some
                                            }
                                    (gameLogId, gameLog), state

                        gameLog, state
                    )
                    ((gameLogId, gameLog), state)
            let state =
                { state with
                    GameLog = gameLogId', gameLog
                }
            let cmd =
                let xs =
                    [gameLogId..gameLogId' - 1]
                    |> List.map (fun i ->
                        async {
                            do! Async.Sleep removeNotifyMs
                            return RemoveNotify i
                        }
                        |> Cmd.OfAsync.result
                    )
                Cmd.batch xs
            state, cmd
        | MoveResult x ->
            let state =
                match x with
                | Ok _ -> state
                | Error err ->
                    let i, log = state.GameLog

                    { state with
                        GameLog = i + 1, Map.add i (sprintf "%A" err) log }
            state, Cmd.none

        | GetUsers(_)
        | AddUser(_)
        | RemoveUser(_)
        | AddMsg(_)
        | AddMsgs(_) as x ->
            printfn "not implemented yet1 '%A'" x
            state, Cmd.none
    | ChangeUserId userId ->
        { state with PlayerId = userId }, Cmd.none
    | RemoveNotify i ->
        let state =
            { state with
                GameLog =
                    let i', gameLog = state.GameLog
                    i', Map.remove i gameLog }
        state, Cmd.none

    | ThreeCardsMove ->
        let cmd =
            ThreeCardsTemplate.toThreeCards state.ThreeCardsTemplate
            |> Shared.ThreeCardsMove
            |> Cmd.bridgeSend
        // TODO: здесь должен быть какой-нибудь Result
        state, cmd
    | ThreeCardsTemplateMsg x ->
        let threeCardsTemplate = state.ThreeCardsTemplate
        let state =
            let f state selectedAttribute att =
                match selectedAttribute with
                | None -> state
                | Some prevAtt ->
                    match state.GameState with
                    | Some gameState ->
                        match gameState.ClientPlayer with
                        | StartHand startHand ->
                            let atts = List.filter ((<>) att) startHand.Attributes
                            let atts = prevAtt::atts

                            { state with
                                GameState =
                                    { gameState with
                                        ClientPlayer =
                                            StartHand { startHand with Attributes = atts }
                                    }
                                    |> Some
                            }
                        | _ -> state
                    | None -> state

            match x with
            | SelectAttribute1 att ->
                let state =
                    { state with
                        ThreeCardsTemplate =
                            { threeCardsTemplate with
                                SelectedAttribute1 = Some att }
                    }
                f state threeCardsTemplate.SelectedAttribute1 att
            | SelectAttribute2 att ->
                let state =
                    { state with
                        ThreeCardsTemplate =
                            { threeCardsTemplate with
                                SelectedAttribute2 = Some att }
                    }
                f state threeCardsTemplate.SelectedAttribute2 att
            | DeselectAttribute1 ->
                let state =
                    match threeCardsTemplate.SelectedAttribute1 with
                    | Some att ->
                        let state =
                            { state with
                                ThreeCardsTemplate =
                                    { threeCardsTemplate with
                                        SelectedAttribute1 = None }

                            }
                        state
                    | None -> state
                state
            | DeselectAttribute2 ->
                let state =
                    match threeCardsTemplate.SelectedAttribute2 with
                    | Some att ->
                        let state =
                            { state with
                                ThreeCardsTemplate =
                                    { threeCardsTemplate with
                                        SelectedAttribute2 = None }
                            }
                        state
                    | None -> state
                state
            | SelectCharacter att ->
                let state =
                    { state with
                        ThreeCardsTemplate =
                            { threeCardsTemplate with
                                SelectedCharacter = Some att }
                    }
                match threeCardsTemplate.SelectedCharacter with
                | None -> state
                | Some prevAtt ->
                    match state.GameState with
                    | Some gameState ->
                        match gameState.ClientPlayer with
                        | StartHand startHand ->
                            let atts = List.filter ((<>) att) startHand.Characters
                            let atts = prevAtt::atts

                            { state with
                                GameState =
                                    { gameState with
                                        ClientPlayer =
                                            StartHand { startHand with Characters = atts }
                                    }
                                    |> Some
                            }
                        | _ -> state
                    | None -> state
            | DeselectCharacter ->
                let state =
                    match threeCardsTemplate.SelectedCharacter with
                    | Some att ->
                        let state =
                            { state with
                                ThreeCardsTemplate =
                                    { threeCardsTemplate with
                                        SelectedCharacter = None }

                            }
                        state
                    | None -> state
                state

        state, Cmd.none

    | SelectOneAttribute att ->
        let state =
            { state with
                SelectOneAttribute = Some att
            }
        state, Cmd.none
    | DeselectOneAttribute ->
        let state =
            { state with
                SelectOneAttribute = None
            }
        state, Cmd.none
    | SelectOneAttributeMove ->
        match state.SelectOneAttribute with
        | Some att ->
            let cmd =
                Shared.SelectOneAttributeMove att
                |> Cmd.bridgeSend
            // TODO: здесь должен быть какой-нибудь Result
            state, cmd
        | None -> state, Cmd.none

    | Login ->
        match state.PlayerId with
        | "" -> state, Cmd.none
        | name ->
            { state with Connection = Waiting }, Cmd.bridgeSend( SetUser { Name = name; Color = Black } )
    | ConnectionLost ->
        { state with Connection = Disconnected }, Cmd.none

open Fable.React
open Fable.React.Props
open Fulma
open Fable.FontAwesome

let navBrand =
    Navbar.Brand.div [ ] [
        Navbar.Item.a [
            Navbar.Item.Props [ Href "https://gretmn102.github.io/" ]
            Navbar.Item.IsActive true
        ] [
            img [
                Src "/favicon.png"
                Alt "Logo"
            ]
        ]
    ]

let containerBox (state : State) (dispatch : Msg -> unit) =
    Box.box' [ ] [
        let loginBox =
            Field.div [ Field.HasAddons ] [
                Control.p [ Control.IsExpanded ] [
                    Input.text [
                        Input.Value state.PlayerId
                        Input.Placeholder "UserId"
                        Input.OnChange (fun x ->
                            ChangeUserId x.Value
                            |> dispatch)
                        Input.Props [
                            OnKeyDown (fun e ->
                                if e.key = "Enter" then
                                    dispatch Login
                            )
                        ]
                    ]
                ]
                Control.p [ ] [
                    Button.a [
                        Button.OnClick (fun _ -> dispatch Login)
                    ] [
                        Fa.i [ Fa.Solid.SignInAlt ] []
                    ]
                ]
            ]
        match state.Connection with
        | Connected x ->
            match x with
            | Ok () ->
                match state.GameState with
                | Some gameState ->
                    Columns.columns [] [
                        Column.column [] [
                            match gameState.MoveStage with
                            | Client.SelectThreeCardsStage ->
                                match gameState.ClientPlayer with
                                | StartHand hand ->
                                    let threeCardsTemplate = state.ThreeCardsTemplate

                                    div [] [
                                        div [] (
                                            let atributes =
                                                [
                                                    match threeCardsTemplate.SelectedAttribute1 with
                                                    | Some att -> att
                                                    | None -> ()
                                                    match threeCardsTemplate.SelectedAttribute2 with
                                                    | Some att -> att
                                                    | None -> ()
                                                ]
                                                |> Set.ofList
                                            let isEnabled =
                                                not (atributes.Count = 2)
                                            hand.Attributes
                                            |> List.choose (fun attributeId ->
                                                if Set.contains attributeId atributes then None
                                                else
                                                    Button.span [
                                                        Button.Disabled (not isEnabled)
                                                        Button.OnClick (fun _ ->
                                                            match threeCardsTemplate.SelectedAttribute1 with
                                                            | None ->
                                                                dispatch (ThreeCardsTemplateMsg (SelectAttribute1 attributeId))
                                                            | Some _ ->
                                                                match threeCardsTemplate.SelectedAttribute2 with
                                                                | None ->
                                                                    dispatch (ThreeCardsTemplateMsg (SelectAttribute2 attributeId))
                                                                | Some _ -> ()
                                                        )
                                                    ] [
                                                        attributeId
                                                        // |> getLetterById gameState.Language
                                                        |> fun x -> string x
                                                        |> str
                                                    ]
                                                    |> Some
                                            )
                                        )
                                        div [] [
                                            match threeCardsTemplate.SelectedAttribute1 with
                                            | Some attributeId ->
                                                Button.span [
                                                    Button.OnClick (fun _ ->
                                                        dispatch (ThreeCardsTemplateMsg DeselectAttribute1))
                                                ] [
                                                    attributeId
                                                    // |> getLetterById gameState.Language
                                                    |> fun x -> string x
                                                    |> str
                                                ]
                                            | None -> ()

                                            match threeCardsTemplate.SelectedAttribute2 with
                                            | Some attributeId ->
                                                Button.span [
                                                    Button.OnClick (fun _ ->
                                                        dispatch (ThreeCardsTemplateMsg DeselectAttribute2))
                                                ] [
                                                    attributeId
                                                    // |> getLetterById gameState.Language
                                                    |> fun x -> string x
                                                    |> str
                                                ]
                                            | None -> ()
                                        ]
                                    ]

                                    div [] [
                                        div [] (
                                            let characters =
                                                [
                                                    match threeCardsTemplate.SelectedCharacter with
                                                    | Some char -> char
                                                    | None -> ()
                                                ]
                                            let isEnabled =
                                                List.isEmpty characters

                                            hand.Characters
                                            |> List.choose (fun characterId ->
                                                if List.contains characterId characters then None
                                                else
                                                    Button.span [
                                                        Button.Disabled (not isEnabled)
                                                        Button.OnClick (fun _ ->
                                                            if isEnabled then
                                                                dispatch (ThreeCardsTemplateMsg (SelectCharacter characterId))
                                                        )
                                                    ] [
                                                        characterId
                                                        // |> getLetterById gameState.Language
                                                        |> fun x -> string x
                                                        |> str
                                                    ]
                                                    |> Some
                                            )
                                        )

                                        div [] [
                                            match threeCardsTemplate.SelectedCharacter with
                                            | Some attributeId ->
                                                Button.span [
                                                    Button.OnClick (fun _ ->
                                                        dispatch (ThreeCardsTemplateMsg DeselectCharacter))
                                                ] [
                                                    attributeId
                                                    // |> getLetterById gameState.Language
                                                    |> fun x -> string x
                                                    |> str
                                                ]
                                            | None -> ()
                                        ]
                                    ]

                                    Control.p [] [
                                        Button.a [
                                            let isEnabled =
                                                Option.isSome threeCardsTemplate.SelectedAttribute1
                                                && Option.isSome threeCardsTemplate.SelectedAttribute2
                                                && Option.isSome threeCardsTemplate.SelectedCharacter

                                            Button.Disabled (not isEnabled)
                                            Button.OnClick (fun _ ->
                                                if isEnabled then dispatch ThreeCardsMove)
                                        ] [
                                            Fa.i [ Fa.Solid.Walking ] []
                                        ]
                                    ]
                                | ThreeCards _
                                | ChoosenAttribute _
                                | FinalHand _ ->
                                    div [] [
                                        str "wait for SelectAttributeStage"
                                    ]
                            | Client.SelectAttributeStage ->
                                match gameState.ClientPlayer with
                                | ThreeCards threeCards ->
                                    div [] [
                                        div [] (
                                            let characters =
                                                [
                                                    match state.SelectOneAttribute with
                                                    | Some char -> char
                                                    | None -> ()
                                                ]
                                            let isEnabled =
                                                List.isEmpty characters

                                            [ threeCards.Attribute1; threeCards.Attribute2 ]
                                            |> List.choose (fun attributeId ->
                                                if List.contains attributeId characters then None
                                                else
                                                    Button.span [
                                                        Button.Disabled (not isEnabled)
                                                        Button.OnClick (fun _ ->
                                                            if isEnabled then
                                                                dispatch (SelectOneAttribute attributeId)
                                                        )
                                                    ] [
                                                        attributeId
                                                        // |> getLetterById gameState.Language
                                                        |> fun x -> string x
                                                        |> str
                                                    ]
                                                    |> Some
                                            )
                                        )

                                        div [] [
                                            match state.SelectOneAttribute with
                                            | Some attributeId ->
                                                Button.span [
                                                    Button.OnClick (fun _ ->
                                                        dispatch DeselectOneAttribute)
                                                ] [
                                                    attributeId
                                                    // |> getLetterById gameState.Language
                                                    |> fun x -> string x
                                                    |> str
                                                ]
                                            | None -> ()
                                        ]
                                        Control.p [] [
                                            Button.a [
                                                let isEnabled =
                                                    Option.isSome state.SelectOneAttribute

                                                Button.Disabled (not isEnabled)
                                                Button.OnClick (fun _ ->
                                                    if isEnabled then dispatch SelectOneAttributeMove)
                                            ] [
                                                Fa.i [ Fa.Solid.Walking ] []
                                            ]
                                        ]
                                    ]
                                | StartHand(_)
                                | ChoosenAttribute _
                                | FinalHand(_) ->
                                    div [] [
                                        str "wait for StartGameStage"
                                    ]
                            | Client.StartGameStage ->
                                match state.GameState with
                                | Some gameState ->
                                    div [] [
                                        str (sprintf "%A" gameState.ClientPlayer)
                                    ]
                                    div [] [
                                        str (sprintf "%A" gameState.OtherPlayers)
                                    ]
                                | None -> ()
                        ]
                    ]
                | None ->
                    div [] [
                        str "wait for players"
                    ]
            | Error err ->
                loginBox
                div [] [str (sprintf "%A" err)]
        | Waiting ->
            div [] [
                Fa.i [ Fa.IconOption.Size Fa.ISize.Fa3x; Fa.Solid.Spinner; Fa.Spin ] []
            ]
        | Disconnected ->
            loginBox

    ]

let view (state : State) (dispatch : Msg -> unit) =
    Hero.hero [
        // Hero.Color IsPrimary
        Hero.IsFullHeight
        Hero.Props [
            Style [
                // Background """linear-gradient(rgba(0, 0, 0, 0.5), rgba(0, 0, 0, 0.5)), url("https://unsplash.it/1200/900?random") no-repeat center center fixed"""
                BackgroundSize "cover"
            ]
        ]
    ] [
        Hero.head [ ] [
            Navbar.navbar [ ] [
                Container.container [ ] [ navBrand ]
            ]
        ]

        Hero.body [ ] [
            Container.container [ ] [
                Column.column [
                    // Column.Width (Screen.All, Column.Is6)
                    // Column.Offset (Screen.All, Column.Is3)
                ] [
                    Heading.p [ Heading.Modifiers [ Modifier.TextAlignment (Screen.All, TextAlignment.Centered) ] ] [ str "BattleOfFantasy" ]
                    containerBox state dispatch
                ]
            ]

            if state.NotificationsVisible then
                state.GameLog
                |> snd
                |> Seq.map (fun (KeyValue(i, x)) ->
                    li [] [
                        Notification.notification [
                        ] [
                            Notification.delete [
                                Props [
                                    OnClick (fun e -> RemoveNotify i |> dispatch)
                                ]
                            ] []
                            div [] [
                                str (sprintf "%A" x)
                            ]
                        ]
                    ]
                )
                |> List.ofSeq
                |> ul [
                    Style [
                        Position PositionOptions.Absolute
                        ZIndex 1
                        Bottom 0
                        Right 0
                    ]
                ]
        ]
    ]
