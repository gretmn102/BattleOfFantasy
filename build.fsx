#r "paket: groupref build //"
#load "./.fake/build.fsx/intellisense.fsx"
#r "netstandard"

open Fake.Core
open Fake.DotNet
open Fake.IO
open Farmer
open Farmer.Builders
open Fake.JavaScript

Target.initEnvironment ()

let sharedPath = Path.getFullName "./src/Shared"
let serverPath = Path.getFullName "./src/Server"
let deployDir = Path.getFullName "./deploy"
let sharedTestsPath = Path.getFullName "./tests/Shared"
let serverTestsPath = Path.getFullName "./tests/Server"

let dotnet cmd workingDir =
    let result = DotNet.exec (DotNet.Options.withWorkingDirectory workingDir) cmd ""
    if result.ExitCode <> 0 then failwithf "'dotnet %s' failed in %s" cmd workingDir

Target.create "Clean" (fun _ -> Shell.cleanDir deployDir)

Target.create "InstallClient" (fun _ -> Npm.install id)

let bundle () =
    let runtimeOption = if Environment.isUnix then "--runtime linux-x64" else ""
    dotnet (sprintf "publish -c Release -o \"%s\" %s" deployDir runtimeOption) serverPath
    Npm.run "build" id

Target.create "Bundle" (fun _ -> bundle())
Target.create "JustBundle" (fun _ -> bundle())

Target.create "Azure" (fun _ ->
    let web = webApp {
        name "SAFE"
        zip_deploy "deploy"
    }
    let deployment = arm {
        location Location.WestEurope
        add_resource web
    }

    deployment
    |> Deploy.execute "SAFE" Deploy.NoParameters
    |> ignore
)
let run () =
    dotnet "build" sharedPath
    [ async { dotnet "watch run" serverPath }
      async { Npm.run "start" id } ]
    |> Async.Parallel
    |> Async.RunSynchronously
    |> ignore

Target.create "Run" (fun _ -> run ())

Target.create "JustRun" (fun _ -> run ())

let runTests () =
    dotnet "build" sharedTestsPath
    [ async { dotnet "watch run" serverTestsPath }
      async { Npm.run "test:live" id } ]
    |> Async.Parallel
    |> Async.RunSynchronously
    |> ignore
Target.create "RunTests" (fun _ -> runTests ())
Target.create "JustRunTests" (fun _ -> runTests ())

open Fake.Core.TargetOperators

"Clean"
    ==> "InstallClient"
    ==> "Bundle"
    ==> "Azure"

"Clean"
    ==> "InstallClient"
    ==> "Run"

"Clean"
    ==> "InstallClient"
    ==> "RunTests"

"Clean"
    ==> "InstallClient"
    ==> "Run"

Target.runOrDefaultWithArguments "Bundle"
