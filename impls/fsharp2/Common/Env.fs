module Common.Env
open Common.Types

  let replEnv() =
    Map.empty
      .Add("+", Expression.EBuiltInFunc (fun (x : obj list) -> x |> unbox |> List.fold (+) |> box))
      .Add("-", Expression.EBuiltInFunc (fun (x : obj list) -> x |> unbox |> List.fold (-) |> box))
      .Add("*", Expression.EBuiltInFunc (fun (x : obj list) -> x |> unbox |> List.fold (*) |> box))
      .Add("/", Expression.EBuiltInFunc (fun (x : obj list) -> x |> unbox |> List.fold (/) |> box))