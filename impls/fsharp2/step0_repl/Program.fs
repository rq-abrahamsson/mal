module REPL
  open System

  let READ input =
    input

  let EVAL data =
    data

  let PRINT input =
    printfn "%s" input

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
