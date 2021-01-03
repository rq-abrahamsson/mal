module Common.Types

  type Expression =
    | ENil
    | EString of string
    | EKeyword of string
    | ESymbol of string
    | ENumber of int64
    | EBool of bool
    | EList of Expression list
    | EVector of Expression list
    | EBuiltInFunc of (obj list -> obj)

