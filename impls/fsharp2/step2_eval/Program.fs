module REPL
  open FSharpPlus
  open System
  open Common
  open Common.Types
  open Common.Reader
  open Common.Writer

  let READ (input : string) =
    readString input

  let rec evalAst env (ast : Expression) : Expression =
    match ast with
    | ESymbol symbol ->
      Env.get env symbol
    | EList l ->
      l |> List.map (eval env) |> Expression.EList
    | EVector v ->
      v |> List.map (eval env) |> Expression.EVector
    | expression -> expression

  and eval env (ast : Expression) : Expression =
    match ast with
    | EList [] as emptyList -> emptyList
    | EList _ as expression ->
      let res = expression |> evalAst env
      match res with
      | EList((EBuiltInFunc f)::rest) -> f rest
      | _ -> failwith "Expected function"
    | expression -> expression |> evalAst env

  let EVAL env (ast : Expression) =
    eval env ast

  let PRINT ast =
    match ast with
    | Ok ast -> printfn "%s" (writeString ast)
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
