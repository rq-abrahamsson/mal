module Common.Eval

  open Common.Types

  let rec evalAst env (ast : Expression) =
    match ast with
    | Expression.ESymbol symbol ->
//      let tmp =
//      printfn "tmp: %A" tmp
      env |> Map.find symbol |> box

//    | Expression.EList l | Expression.EVector l ->
//      l |> List.map (fun x -> evalAst x env)

