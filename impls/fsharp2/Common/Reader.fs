namespace Common
open FParsec

module Reader =

  type Expression =
    | EString of string
    | EKeyword of string
    | ESymbol of string
    | ENumber of int64
    | EBool of bool
    | EList of Expression list
    | EVector of Expression list

  let private ws = spaces
  let private str s = pstring s
  let private chr c = pchar c
  let private stringLiteral =
    let normalCharSnippet = manySatisfy (fun c -> c <> '"' && c <> '\\')
    between (str "\"") (str "\"") normalCharSnippet

  let private keywordLiteral =
    chr ':' >>. many1Satisfy (fun c -> c <> '"' && c <> '\\' && c <> ' ' && c <> ')' && c <> '(')
  let private symbolLiteral =
    many1Satisfy (fun c -> c <> '"' && c <> '\\' && c <> ' ' && c <> ')' && c <> '(')

  let private estring = stringLiteral |>> EString
  let private ekeyword : Parser<Expression, unit> = keywordLiteral |>> EKeyword
  let private esymbol : Parser<Expression, unit> = symbolLiteral |>> ESymbol
  let private etrue = stringReturn "true" (EBool true)
  let private efalse = stringReturn "false" (EBool false)
  let private enumber = pint64 |>> ENumber

  let private evalue, evalueRef = createParserForwardedToRef<Expression, unit>()
  let private listBetweenStrings sOpen sClose pElement f =
    between (str sOpen) (str sClose)
            (ws >>. sepBy pElement (str " " <|> str ",") |>> f)
//            (sepBy pElement spaces1 |>> f)
//            (ws >>. sepBy (pElement .>> ws) (str "," >>. ws) |>> f)

  let private elist = listBetweenStrings "(" ")" evalue EList
  let private evector = listBetweenStrings "[" "]" evalue EVector
  do evalueRef := choice [elist; evector; enumber; estring; ekeyword; esymbol; etrue; efalse]

  let private expression = ws >>. evalue .>> ws .>> eof

  let private runToResult p str : Result<_, _> =
    match run p str with
    | Success(result, _, _) ->
      Result.Ok result
    | Failure(errorMsg, _, _) ->
      Result.Error errorMsg

  let readString str = runToResult expression str
