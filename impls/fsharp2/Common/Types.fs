module Common.Types

  open System.Collections.Generic

  type Expression =
    | ENil
    | EString of string
    | EKeyword of string
    | ESymbol of string
    | ENumber of int64
    | EBool of bool
    | EList of Expression list
    | EVector of Expression list
    | EBuiltInFunc of (Expression list -> Expression)
  type Environment = Dictionary<string, Expression>
  type EnvironmentChain = Environment list

