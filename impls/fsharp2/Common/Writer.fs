module Common.Writer

  open Common.Reader

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

  let writeString ast =
    ast
    |> expressionToString
    |> sprintf "%s"


