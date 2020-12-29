module REPL
  open System
  open Common.Reader
  open Common.Writer

  let READ (input : string) =
    readString input

  let EVAL ast =
    ast

  let PRINT ast =
    match ast with
    | Ok ast -> printfn "%s" (writeString ast)
    | Error errorString -> printfn "%s" errorString

  let REP input =
    input
    |> READ
    |> EVAL
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
