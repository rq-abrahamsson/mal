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
    | Ok ast -> printfn "%A" (writeString ast)
    | Error errorString -> printfn "%s" errorString

  let REP input =
    input
    |> READ
    |> map (EVAL (Env.environment()))
    |> PRINT

  let readLine () =
    printf "user> "
    Console.ReadLine()

  [<EntryPoint>]
  let rec main _argv =
      match readLine () with
      | null -> 0
      | input ->
        REP input
        main _argv
