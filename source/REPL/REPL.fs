// Copyright (c) Kagurazaka K. Shinkai. All Rights Reserved. See LICENSE.txt in
// the project root for license information.

namespace KkShinkai.Regex.REPL

module REPL =

  open KkShinkai.Regex.Parse
  open KkShinkai.Regex.AST
  open KkShinkai.Regex.Automata

  let x = ((+) <| "\n")

  let welcome () =
    [ [ @"     ___"
        @"    / _ \___ ___ ______ __"
        @"   / , _/ -_) _ `/ -_) \ /"
        @"  /_/|_|\__/\_, /\__/_\_\ "
        @"           /___/"]
      |> List.map (fun s -> s + "\n" |> Markup.mark)
      |> Markup.concat
      Markup.mark "\nLittle Regex (version beta-0.0.1)\n"
      Markup.mark "Copyright (c) 2021 Kk Shinkai. All rights reserved.\n"
      Markup.mark "For help type 'help' or 'usage'.\n" ]
    |> Markup.concat
    |> Markup.println

  let private printTokens tokens =
    let showContent = function
    | EOI      -> Markup.text " $ " + Markup.border "         (eoi) "
    | Midline  -> Markup.text " | " + Markup.border "     (midline) "
    | Asterisk -> Markup.text " * " + Markup.border "    (asterisk) "
    | LParen   -> Markup.text " ( " + Markup.border "  (left paren) "
    | RParen   -> Markup.text " ) " + Markup.border " (right paren) "
    | Char c   ->
      [ Markup.text " character '"
        Markup.code (string c)
        Markup.text "'    " ]
      |> Markup.concat

    [ Markup.border "\n .-----.------------------.-------.\n"
      Markup.border " |"; Markup.text " idx "; Markup.border "|"
      Markup.text "      content     "; Markup.border "|"; Markup.text "  pos  "
      Markup.border "|\n"
      Markup.border " |-----+------------------+-------|"]
    |> Markup.concat
    |> Markup.println

    tokens
    |> Seq.iteri (fun index token ->
      let position =
        if token.Start = token.End
          then string token.Start
          else sprintf "%d:%d" token.Start token.End
      [ Markup.border " |"
        Markup.text (sprintf " %3d " index)
        Markup.border "|"
        token.Content |> showContent
        Markup.border "|"
        Markup.text (sprintf " %5s " position)
        Markup.border "|" ]
      |> Markup.concat
      |> Markup.println)
    Markup.border " '-----'------------------'-------'\n"
    |> Markup.println

  let rec private printTree indent last regex =
    indent + if last then "`- " else "|- "
    |> Markup.border
    |> Markup.print
    let indent = indent + if last then "  " else "| "
    match regex with
    | Chr c ->
      (Markup.text "char " + Markup.code $"'{c}'")
      |> Markup.println
    | Concat (r1, r2) ->
      (Markup.text "concat " + Markup.code "·")
      |> Markup.println
      r1 |> printTree indent false
      r2 |> printTree indent true
    | Alter (r1, r2) ->
      (Markup.text "alter " + Markup.code "|")
      |> Markup.println
      r1 |> printTree indent false
      r2 |> printTree indent true
    | Star r ->
      (Markup.text "star " + Markup.code "*")
      |> Markup.println
      r |> printTree indent true

  let printAST ast =
    ast
    |> printTree "  " true
    ""
    |> Markup.text
    |> Markup.println

  let reportLexError err =
    (Markup.error "error: " + Markup.text err)
    |> Markup.println

  let reportParseError err =
    (Markup.error "error: " + Markup.text err)
    |> Markup.println

  let reportWrongNumberOfParameters commandName except actual =
    [ Markup.error "error: "
      Markup.text
        ( $"command '{commandName}' except {except} arguments, but actually " +
          $"get {actual}." +
          if except < actual then
            " Perhaps you are trying to insert a space in the regex or " +
            "string, but the space will be treated as a parameter separator " +
            "in REPL."
          else "" )]
    |> Markup.concat
    |> Markup.println

  let rec private loop () =
    ">>> " |> Markup.border |> Markup.print
    let command =
      System.Console.ReadLine().Split [|' '|]
      |> Array.toList

    if command |> List.head = "" then
      if List.length command <> 1 then
        "error: "
        |> Markup.error
        |> Markup.print
        $"extra spaces before the input command, which is illegal."
        |> Markup.text
        |> Markup.println
      loop ()
    else
      let arguments = command |> List.tail
      match command |> List.head with
      | "help" | "usage" -> showUsage
      | "lex"   -> arguments |> showLexResult
      | "parse" -> arguments |> showParseResult
      | "match" -> arguments |> showMatchResult
      | "quit" -> ()
      | command ->
        "error: "
        |> Markup.error
        |> Markup.print
        $"unknown command '{command}', type 'help' or 'usage' for help."
        |> Markup.text
        |> Markup.println
        loop ()

  and showUsage =
    [ Markup.note "\nusage:"

      Markup.border " 1) "; Markup.code "help"
      Markup.border " or "; Markup.code "usage"
      Markup.textln " : show help message"

      Markup.border "       2) "; Markup.code "lex <regex>"
      Markup.textln " : show the result of lexical analysis"

      Markup.border "       3) "; Markup.code "match <regex> <string>"
      Markup.textln " : check if the string and regex match"

      Markup.border "       4) "; Markup.code "quit"
      Markup.textln " : exit"

      Markup.text "\nFor example, try to input command `"
      Markup.code "parse a(a|b)*a|b*";
      Markup.textln "` below.\n"]
    |> Markup.concat
    |> Markup.print
    loop ()
  
  and showLexResult arguments =
    match arguments with
    | [regex] ->
      try
        let tokenStream =
          regex
          |> Lexer.analyze
        "\n            Token Stream"
        |> Markup.text
        |> Markup.print
        tokenStream
        |> printTokens
      with
      | LexError e -> e |> reportLexError
    | _ ->
      reportWrongNumberOfParameters "lex" 1 (arguments |> List.length)
    loop ()

  and showParseResult arguments =
    match arguments with
    | [regex] ->
      try
        let ast =
          regex
          |> Lexer.analyze
          |> Parser.analyse
        [ Markup.text "\n  ast "
          Markup.code $"\"{regex}\"" ]
        |> Markup.concat
        |> Markup.println
        ast
        |> printAST
      with
      | LexError e -> e |> reportLexError
      | ParseError e -> e |> reportParseError
    | _ ->
      reportWrongNumberOfParameters "parse" 1 (arguments |> List.length)
    loop ()

  and showMatchResult arguments =
    match arguments with
    | [regex; str] ->
      try
        let result =
          regex
          |> Lexer.analyze
          |> Parser.analyse
          |> NFA.ofRegex
          |> DFA.ofNFA
          |> DFA.run str
        ( if result
            then Markup.success "match"
            else Markup.error "mismatch" )
        |> Markup.println
      with
      | LexError e -> e |> reportLexError
      | ParseError e -> e |> reportParseError
    | _ ->
      reportWrongNumberOfParameters "match" 2 (arguments |> List.length)
    loop ()

  let run () =
    welcome ()
    loop ()
