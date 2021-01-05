module Common.Eval

  open Common.Types

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
