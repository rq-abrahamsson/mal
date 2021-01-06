module REPL
  open FSharpPlus
  open System
  open Common
  open Common.Types
  open Common.Reader
  open Common.Writer
  open Common.Eval

  let READ (input : string) =
    readString input

  let EVAL env (ast : Expression) =
    eval env ast

  let PRINT ast =
    match ast with
    | Ok ast -> printfn "%s" (writeString ast)
    | Error errorString -> printfn "%s" errorString

  let REP env input =
    input
    |> READ
    |> map (EVAL env)
    |> PRINT

  let readLine () =
    printf "user> "
    Console.ReadLine()

  let rec run env =
    match readLine () with
    | null -> 0
    | input ->
      REP env input
      run env

  [<EntryPoint>]
  let main _argv =
    let env = Env.evalEnv()
    run env
