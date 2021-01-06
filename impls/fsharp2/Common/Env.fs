module Common.Env
  open System.Collections.Generic
  open Common.Types

  let rec find (chain : EnvironmentChain) symbol : Environment option =
    match chain with
    | [] -> None
    | env::rest ->
      match env.TryGetValue symbol with
      | (true, _) -> Some env
      | (false, _) -> find rest symbol

  let get (env : EnvironmentChain) symbol : Expression =
    match find env symbol with
    | Some env ->
      match env.TryGetValue symbol with
      | (true, v) -> v
      | (false, _) -> failwith "Symbol not found"
    | None -> failwith "Symbol not found"

  let set (env : EnvironmentChain) symbol (value : Expression) : unit =
    match env with
    | head::_ -> head.[symbol] <- value
    | _ -> failwith "Environment not found"

  let init (outer : EnvironmentChain) : EnvironmentChain =
    Dictionary<string, Expression>()::outer

  let twoNumberOp (f : int64 -> int64 -> Expression) = function
    | [ENumber(a); ENumber(b)] -> f a b
    | [_; _] -> failwith "Ang mismatch"
    | _ -> failwith "Wrong number of operators"

  let add = twoNumberOp (fun a b -> a + b |> ENumber)
  let subtract = twoNumberOp (fun a b -> a - b |> ENumber)
  let multiply = twoNumberOp (fun a b -> a * b |> ENumber)
  let divide = twoNumberOp (fun a b -> a / b |> ENumber)

  let evalEnv() : EnvironmentChain =
    let env = init []
    set env "+" (EBuiltInFunc add)
    set env "-" (EBuiltInFunc subtract)
    set env "*" (EBuiltInFunc multiply)
    set env "/" (EBuiltInFunc divide)
    env
