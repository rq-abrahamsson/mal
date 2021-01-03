module REPL
  open FSharpPlus
  open System
  open Common.Types
  open Common.Reader
  open Common.Writer

  let READ (input : string) =
    readString input

  let rec evalAst env (ast : obj) =
    printfn "AST %A" ast
    match (unbox ast) with
    | ESymbol symbol ->
      env |> Map.find symbol |> box
//      ast
    | EList l | EVector l ->
      let t = l |> List.map (eval env) |> box
      printfn "||%A" t
      t
    | a -> a |> box
//    | EKeyword kw ->
//      kw |> box
//    | EBool v ->
//      v |> box
//    | ENumber n ->
//      n |> box
//    | EString s ->
//      s |> box
  and eval env (ast : obj) : obj =
    printfn "------%A" ast
//    if (ast :? Expression list) && (unbox ast) |> List.isEmpty then
    if (ast :? Expression) then
      match (unbox ast) with
      | EList l when l |> List.isEmpty ->
        EList l |> box
      | EList _ as ast ->
        let tmp = evalAst env ast
        printfn "tmp %A" tmp
        let f = (tmp |> box :?> Expression list)
        printfn "f %A" f

        ast |> box
//        let f = (tmp |> box :?> Func list).Head
//        let args = (tmp |> box :?> obj list).Tail
//        (f (args |> box) |> box)

//        match (tmp |> unbox) with
//        | Expression.EBuiltInFunc(f)::b ->
//          printfn "tmp %A" f
//          printfn "tmp %A" b
//          f (b |> unbox)
        | _ -> failwith "Did not get a list back when evaluating list"
      | ast ->
        evalAst env ast |> box
    else
      ast
//    elif ast :? Expression list then
//      let tmp = evalAst env ast
//      printfn "tmp %A" tmp
//      match (tmp |> unbox) with
//      | f::b -> f (b |> unbox)
//      | _ -> failwith "Did not get a list back when evaluating list"
//      ast

//      env |> Map.find symbol |> box
//    else //elif ast :? Expression then
//      evalAst env ast
//      ()

//    match ast with
//    | e as list ->
//      if list |> List.isEmpty then
//        ast
//        |> box
//      else
//        let tmp = ast
//        tmp |> box
//    | ast ->
//      Common.Eval.evalAst env ast

  let EVAL env (ast : Expression) =
    eval env (ast |> box) :?> Expression
//    if (ast :? Expression) && (ast |> unbox)  then
//    elif ast :? Expression then
//      Common.Eval.evalAst (ast |> unbox) env
//    elif (ast :? Expression list) |> List.isEmpty then
//
//      (ast |> unbox)

  let PRINT ast =
    match ast with
    | Ok ast -> printfn "%A" (writeString ast)
    | Error errorString -> printfn "%s" errorString

  let REP input =
    input
    |> READ
    |> map (EVAL (Common.Env.replEnv()))
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
