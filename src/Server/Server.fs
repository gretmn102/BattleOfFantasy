module Server

open Elmish.Bridge
open Elmish

open Shared

// open FsharpMyExtension
// open FsharpMyExtension.Either

open Prelude

type ServerMsg =
    | RS of RemoteServerMsg
    | Closed
type State =
    | Connected of User * (RemoteClientMsg -> unit)
    | Disconnected

let connections =
    ServerHub<State,ServerMsg,RemoteClientMsg>()

type History<'a> =
    {
        Get: unit -> 'a list
        Put: 'a -> unit
        ChangeName: string -> string -> unit
    }

let history =
    let mb =
        MailboxProcessor.Start
         (fun (mb:MailboxProcessor<Choice<AsyncReplyChannel<Msgs list>,Msgs, string*string>>) ->
             let rec loop l =
                async {
                    let! msg = mb.Receive()
                    match msg with
                    | Choice1Of3 r ->
                        r.Reply l
                        return! loop l
                    | Choice2Of3 m ->
                        return! loop (m::l |> List.truncate 50)
                    | Choice3Of3 (o,n) ->
                        return! loop (l |> List.map (function ClientMsg(c,m) when c = o -> ClientMsg(n,m)| m -> m))}
             loop [])
    {
        Get = fun () -> mb.PostAndReply (fun e -> Choice1Of3 e)
        Put = fun m -> mb.Post(Choice2Of3 m)
        ChangeName = fun o n -> mb.Post(Choice3Of3 (o,n))
    }

let sendAllMessages currentUserId clientDispatch playersMsgs =
    connections.GetModels()
    |> List.iter (
        function
        | Connected (user, dispatch) ->
            let userId = user.Name
            if currentUserId <> userId then
                match Map.tryFind userId playersMsgs with
                | Some msgs ->
                    dispatch (GameMsgs msgs)
                | None -> ()
        | Disconnected -> ()
    )

    Map.tryFind currentUserId playersMsgs
    |> Option.iter (GameMsgs >> clientDispatch)

let update clientDispatch msg (state:State) =
    match msg with
    | Closed ->
        match state with
        | Disconnected -> ()
        | Connected (u, _) ->
            let res = m.PostAndReply(fun r -> Leave(u.Name, r))
            match res with
            | ThisUserNotPlayed -> ()
            | PlayerLeft -> ()
            | AllPlayersLeft ->
                printfn "all players left — restart game"
                m.Post RestartGame

            connections.BroadcastClient(RemoveUser u.Name)
            let msg =
                {
                    Time = System.DateTime.Now
                    Content = u.Name + " left the room"
                }
                |> SysMsg
            history.Put msg
            connections.BroadcastClient(AddMsg msg)
        Disconnected, Cmd.none
    | RS msg ->
        let nameInUse name =
            connections.GetModels()
            |> List.exists (function
                | Disconnected -> false
                | Connected ({ Name = n }, _) -> n = name)
        match state, msg with
        | _, UsersConnected ->
            let users =
                connections.GetModels()
                |> List.choose (function
                    | Disconnected -> None
                    | Connected (u, _) -> Some u)
            clientDispatch (GetUsers users)
            clientDispatch (AddMsgs (history.Get()))
            state, Cmd.none
        | state, SetUser u ->
            let f u =
                let x = m.PostAndReply(fun r -> Login(u.Name, r))

                x.Return
                |> LoginResult
                |> clientDispatch

                sendAllMessages u.Name clientDispatch x.PlayersMsgs

                let state =
                    match x.Return with
                    | Ok x ->
                        Connected (u, clientDispatch)
                    | _ -> state

                state, Cmd.none

            match state with
            | Disconnected ->
                if nameInUse u.Name then
                    #if DEBUG
                    f u
                    #else
                    SysMsg {
                        Time = System.DateTime.Now
                        Content = "Name is in use"
                    }
                    |> AddMsg
                    |> clientDispatch

                    clientDispatch (LoginResult (Error YouAlreadyLogin))
                    state, Cmd.none
                    #endif
                else
                    connections.BroadcastClient(AddUser u)
                    let msg = SysMsg {Time=System.DateTime.Now; Content = u.Name + " joined the room"}
                    history.Put msg
                    connections.BroadcastClient(AddMsg msg)

                    f u
            | Connected (u, _) -> f u
        | userState, GameServerMsg gameServerMsg ->
            match userState with
            | Connected (u, _) ->
                let x =
                    match gameServerMsg with
                    | Shared.ThreeCardsMove word ->
                        m.PostAndReply(fun r -> SelectThreeCardsMove((u.Name, word), r))
                    | Shared.SelectOneAttributeMove attributeId ->
                        m.PostAndReply(fun r -> SelectOneAttributeMove((u.Name, attributeId), r))
                    | Shared.RestartMove ->
                        m.PostAndReply(fun r -> RestartMove(u.Name, r))
                x.Return
                |> MoveResult
                |> clientDispatch

                sendAllMessages u.Name clientDispatch x.PlayersMsgs
            | Disconnected ->
                Error (GetStateError YouAreNotLogin)
                |> MoveResult
                |> clientDispatch
            state, Cmd.none
        | Disconnected, SendMsg m ->
            state, Cmd.none
        | Connected(u, _), SendMsg m ->
            if System.String.IsNullOrWhiteSpace m then
                ()
            else
                let msg = ClientMsg (u.Name,{Content=m;Time = System.DateTime.Now})
                history.Put msg
                connections.BroadcastClient(AddMsg msg)
            state, Cmd.none

let init (clientDispatch:Dispatch<RemoteClientMsg>) () =
    clientDispatch QueryConnected
    Disconnected, Cmd.none

open Saturn.Application
let server =
    Bridge.mkServer Remote.socketPath init update
    |> Bridge.withConsoleTrace
    |> Bridge.whenDown Closed
    |> Bridge.withServerHub connections
    |> Bridge.run Giraffe.server

let port =
    match System.Environment.GetEnvironmentVariable("PORT") with
    | null -> uint16 8086
    | port -> uint16 port
let publicPath = System.IO.Path.GetFullPath "./public"
let app =
  application {
    use_static publicPath
    use_router server
#if !DEBUG
    disable_diagnostics
#endif
    app_config Giraffe.useWebSockets
    url ("http://0.0.0.0:" + port.ToString() + "/")
  }

run app