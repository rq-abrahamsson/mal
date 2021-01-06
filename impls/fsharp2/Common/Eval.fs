module Common.Eval

  open Common.Types
  open Common

  let rec iterPairs f (l : 'a list) =
    match l with
    | [] -> ()
    | fst::[ snd ] ->
      f fst snd
    | fst::snd::rest ->
      f fst snd
      iterPairs f rest
    | _ -> ()
  let rec evalAst (env : EnvironmentChain) (ast : Expression) : Expression =
    match ast with
    | ESymbol symbol ->
      Env.get env symbol
    | EList l ->
      l |> List.map (eval env) |> Expression.EList
    | EVector v ->
      v |> List.map (eval env) |> Expression.EVector
    | expression -> expression

  and defBang env (ast : Expression list) =
      match ast with
      | [ESymbol s; expression] ->
        let expression = eval env expression
        Env.set env s expression
        expression
      | _ -> failwith "Wrong number of args to def!"

  and setBinding env fst snd =
    let s =
      match fst with
      | ESymbol s -> s
      | _ -> failwith "Expected symbol"
    let expression = eval env snd
    Env.set env s expression
  and letStar env (ast : Expression list) =
      match ast with
      | [bindings; expression] ->
        let newEnv = Env.init env
        let binder = setBinding newEnv
        match bindings with
        | EList l | EVector l ->
          l |> iterPairs binder
        | _ -> failwith "Expected list or vector with let*"
        eval newEnv expression
      | _ -> failwith "Wrong number of args to let*"

  and eval env (ast : Expression) : Expression =
    match ast with
    | EList [] as emptyList -> emptyList
    | EList (ESymbol "def!"::rest) -> defBang env rest
    | EList (ESymbol "let*"::rest) -> letStar env rest
    | EList _ as expression ->
      match expression |> evalAst env with
      | EList((EBuiltInFunc f)::args) -> f args
      | _ -> failwith "Expected function"
    | expression -> expression |> evalAst env
