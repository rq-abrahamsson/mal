module Common.Writer

  open Common.Types

  let rec private expressionToString(ast : Expression) =
    match ast with
    | EString str -> sprintf "\"%s\"" str
    | ESymbol str -> sprintf "%s" str
    | EKeyword str -> sprintf ":%s" str
    | ENumber nr -> sprintf "%d" nr
    | EBool true -> "true"
    | EBool false -> "false"
    | EList l -> "(" + (l |> List.map expressionToString |> String.concat " ") + ")"
    | EVector v -> "[" + (v |> List.map expressionToString |> String.concat " ") + "]"
    | EBuiltInFunc f -> "fun(" + string f + ")"
    | ENil -> "nil"

  let writeString ast =
    ast
    |> expressionToString
    |> sprintf "%s"


