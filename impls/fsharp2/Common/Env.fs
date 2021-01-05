module Common.Env
  open Common.Types
  open Common

  type Environment = Map<string, Expression>
  let twoNumberOp (f : int64 -> int64 -> Expression) = function
    | [ENumber(a); ENumber(b)] -> f a b
    | [_; _] -> failwith "Ang mismatch"
    | _ -> failwith "Wrong number of operators"

  let add = twoNumberOp (fun a b -> a + b |> ENumber)
  let subtract = twoNumberOp (fun a b -> a - b |> ENumber)
  let multiply = twoNumberOp (fun a b -> a * b |> ENumber)
  let divide = twoNumberOp (fun a b -> a / b |> ENumber)

  let get (env : Environment) symbol : Types.Expression =
    env |> Map.find symbol

  let environment() =
    Map.empty
      .Add("+", EBuiltInFunc add)
      .Add("-", EBuiltInFunc subtract)
      .Add("*", EBuiltInFunc multiply)
      .Add("/", EBuiltInFunc divide)