// Copyright (c) Kagurazaka K. Shinkai. All Rights Reserved. See LICENSE.txt in
// the project root for license information.

namespace KkShinkai.Regex.Parse

exception ParseError of string
exception FatalError of string

/// <summary>
/// Action set of syntax-directed translation (SDT). The location of the
/// semantic action is as follows:
///
/// <code>
/// 0) Regex -> Simple "|" Regex { Action }
/// 1) Regex -> Simple           { Action }
/// 2) Simple -> Simple Single   { Action }
/// 3) Simple -> Single          { Action }
/// 4) Single -> Base "*"        { Action }
/// 5) Single -> Base            { Action }
/// 6) Base -> "(" Regex ")"     { Action }
/// 7) Base -> char              { Action }
/// </code>
/// </summary>
type SDT<'input, 'context> =
  { ``Regex -> Simple "|" Regex``
      : 'context -> 'context -> 'context
    
    ``Regex -> Simple``
      : 'context -> 'context
    
    ``Simple -> Simple Single``
      : 'context -> 'context -> 'context
    
    ``Simple -> Single``
      : 'context -> 'context
    
    ``Single -> Base "*"``
      : 'context -> 'context
    
    ``Single -> Base``
      : 'context -> 'context
    
    ``Base -> "(" Regex ")"``
      : 'context -> 'context
    
    ``Base -> char``
      : 'input -> 'context }

module SDT =
  open KkShinkai.Regex.AST

  /// <summary>
  /// SDT that translate regular expression from token stream to AST.
  /// <code>
  /// 0) Regex -> Simple "|" Regex { Alter (Simple.val, Regex.val) }
  /// 1) Regex -> Simple           { Simple.val }
  /// 2) Simple -> Simple Single   { Concat (Simple.val, Single.val) }
  /// 3) Simple -> Single          { Single.val }
  /// 4) Single -> Base "*"        { Star Base.val }
  /// 5) Single -> Base            { Base.val }
  /// 6) Base -> "(" Regex ")"     { Regex.val }
  /// 7) Base -> char              { Chr char.val }
  /// </code>
  /// </summary>
  let parseActions =
    { ``Regex -> Simple "|" Regex`` =
        fun simple regex -> Alter (simple, regex)

      ``Regex -> Simple`` =
        fun simple -> simple

      ``Simple -> Simple Single`` =
        fun simple single -> Concat (simple, single)

      ``Simple -> Single`` =
        fun single -> single

      ``Single -> Base "*"`` =
        fun regex -> Star regex

      ``Single -> Base`` =
        fun regex -> regex

      ``Base -> "(" Regex ")"`` =
        fun regex -> regex

      ``Base -> char`` =
        fun chr -> Chr chr }

module Parser =
  open KkShinkai.Regex.AST

  type private Action =
    | Shift of int
    | Reduce of int
    | Stuck
    | Accept

  type private Symbol =
    | NonTerminator of Terminator
    | Terminator of Token
  and private Terminator =
    | Regex | Simple | Single | Base

  let unreachable () =
    raise (FatalError
      ("this error is caused by a wrong LALR table, and it is logically " +
       "impossible to happen."))


  let private getNonTerminatorIndex = function
    | Regex  -> 0
    | Simple -> 1
    | Single -> 2
    | Base   -> 3

  let private getTerminatorIndex (token:Token) =
    match token.Content with
    | LParen   -> 0
    | RParen   -> 1
    | Char _   -> 2
    | Midline  -> 3
    | Asterisk -> 4
    | EOI      -> 5

  let private getSymbolIndex = function
    | Terminator t     -> getTerminatorIndex t
    | NonTerminator nt -> getNonTerminatorIndex nt

  let private symbolsCount p = [|3; 1; 2; 1; 2; 1; 3; 1|].[p]


  let private actionTable = array2D [|
  (*.-------.----------.----------.----------.----------.----------.----------.*)
  (*| st\tk |    '('   |   ')'    |   char   |   '|'    |   '*'    |   eos    |*)
  (*|-------|----------|----------|----------|----------|----------|----------|*)
  (*|  0 *)[| Shift  5 ; Stuck    ; Shift  6 ; Stuck    ; Stuck    ; Stuck    |]
  (*|  1 *)[| Stuck    ; Stuck    ; Stuck    ; Stuck    ; Stuck    ; Accept   |]
  (*|  2 *)[| Shift  5 ; Reduce 1 ; Shift  6 ; Shift  7 ; Stuck    ; Reduce 1 |]
  (*|  3 *)[| Reduce 3 ; Reduce 3 ; Reduce 3 ; Reduce 3 ; Stuck    ; Reduce 3 |]
  (*|  4 *)[| Reduce 5 ; Reduce 5 ; Reduce 5 ; Reduce 5 ; Shift 9  ; Reduce 5 |]
  (*|  5 *)[| Shift  5 ; Stuck    ; Shift  6 ; Stuck    ; Stuck    ; Stuck    |]
  (*|  6 *)[| Reduce 7 ; Reduce 7 ; Reduce 7 ; Reduce 7 ; Reduce 7 ; Reduce 7 |]
  (*|  7 *)[| Shift  5 ; Stuck    ; Shift  6 ; Stuck    ; Stuck    ; Stuck    |]
  (*|  8 *)[| Reduce 2 ; Reduce 2 ; Reduce 2 ; Reduce 2 ; Stuck    ; Reduce 2 |]
  (*|  9 *)[| Reduce 4 ; Reduce 4 ; Reduce 4 ; Reduce 4 ; Stuck    ; Reduce 4 |]
  (*| 10 *)[| Stuck    ; Shift 12 ; Stuck    ; Stuck    ; Stuck    ; Stuck    |]
  (*| 11 *)[| Stuck    ; Reduce 0 ; Stuck    ; Stuck    ; Stuck    ; Reduce 0 |]
  (*| 12 *)[| Reduce 6 ; Reduce 6 ; Reduce 6 ; Reduce 6 ; Reduce 6 ; Reduce 6 |]
  (*'-------'----------'----------'----------'----------'----------'----------'*)
  |]

  let private gotoTable = array2D [|
  (*.-------.----------.----------.----------.----------.*)
  (*| st\tm |  Regex/0 | Simple/1 | Single/2 |  Base/3  |*)
  (*|-------|----------|----------|----------|----------|*)
  (*|  0 *)[| Some   1 ; Some   2 ; Some   3 ; Some   4 |]
  (*|  1 *)[| None     ; None     ; None     ; None     |]
  (*|  2 *)[| None     ; None     ; Some   8 ; Some   4 |]
  (*|  3 *)[| None     ; None     ; None     ; None     |]
  (*|  4 *)[| None     ; None     ; None     ; None     |]
  (*|  5 *)[| Some  10 ; Some   2 ; Some   3 ; Some   4 |]
  (*|  6 *)[| None     ; None     ; None     ; None     |]
  (*|  7 *)[| Some  11 ; Some   2 ; Some   3 ; Some   4 |]
  (*|  8 *)[| None     ; None     ; None     ; None     |]
  (*|  9 *)[| None     ; None     ; None     ; None     |]
  (*| 10 *)[| None     ; None     ; None     ; None     |]
  (*| 11 *)[| None     ; None     ; None     ; None     |]
  (*| 12 *)[| None     ; None     ; None     ; None     |]
  (*'-------'----------'----------'----------'----------'*)
  |]

  let private getAction state token = actionTable.[state, token |> getTerminatorIndex]
  let private getGoto state t = gotoTable.[state, t]

  let private getLeftOf n = [|0; 0; 1; 1; 2; 2; 3; 3|].[n]
  let private getLeftSymbolOf n = NonTerminator [|Regex; Regex; Simple; Simple; Single; Single; Base; Base|].[n]

  type private Configuration =
    { Input: list<Token>
      Stack: list<int>
      Symbols: list<Symbol>
      AST: list<Regex> }

  /// <summary>Implementation of LALR automata analysis algorithm.</summary>
  let private runAnalyse sdt (tokenStream:list<Token>) =
      let configuration =
        { Input = tokenStream
          Stack = [0]
          Symbols = []
          AST = [] }

      let rec analyseIter cfg =
        match getAction (cfg.Stack |> List.head) (cfg.Input |> List.head) with
        | Shift n ->
          analyseIter
            { cfg with
                Input   = cfg.Input.Tail
                Stack   = n :: cfg.Stack
                Symbols = Terminator (cfg.Input |> List.head) :: cfg.Symbols }
        | Reduce n ->
          let newStack =
            cfg.Stack
            |> List.skip (n |> symbolsCount)
          let reducedSymbol, newSymbols =
            cfg.Symbols
            |> List.splitAt (n |> symbolsCount)
          let target =
            match getGoto (newStack |> List.head) (n |> getLeftOf) with // this should called 'State', not 'Shift
            | Some n -> n
            | None -> unreachable ()
          let newCfgAST =
            match n with
            | 0 ->
              let reduced, restCfgAST = cfg.AST |> List.splitAt 2
              sdt.``Regex -> Simple "|" Regex``
                (reduced |> List.item 1)
                (reduced |> List.item 0) :: restCfgAST
            | 1 ->
              let reduced, restCfgAST = cfg.AST |> List.splitAt 1
              sdt.``Regex -> Simple``
                (reduced |> List.item 0) :: restCfgAST
            | 2 ->
              let reduced, restCfgAST = cfg.AST |> List.splitAt 2
              sdt.``Simple -> Simple Single``
                (reduced |> List.item 1)
                (reduced |> List.item 0) :: restCfgAST
            | 3 ->
              let reduced, restCfgAST = cfg.AST |> List.splitAt 1
              sdt.``Simple -> Single``
                (reduced |> List.item 0) :: restCfgAST
            | 4 ->
              let reduced, restCfgAST = cfg.AST |> List.splitAt 1
              sdt.``Single -> Base "*"``
                (reduced |> List.item 0) :: restCfgAST
            | 5 ->
              let reduced, restCfgAST = cfg.AST |> List.splitAt 1
              sdt.``Single -> Base``
                (reduced |> List.item 0) :: restCfgAST
            | 6 ->
              let reduced, restCfgAST = cfg.AST |> List.splitAt 1
              sdt.``Base -> "(" Regex ")"``
                (reduced |> List.item 0) :: restCfgAST
            | 7 ->
              match reducedSymbol |> List.head with
              | Terminator tok ->
                match tok.Content with
                | Char c -> sdt.``Base -> char`` c :: cfg.AST
                | _ -> unreachable ()
              | _ -> unreachable ()
            | _ -> unreachable ()
          analyseIter
            { cfg with
                Stack = target :: newStack
                Symbols = (getLeftSymbolOf n) :: newSymbols
                AST = newCfgAST }
        | Accept -> cfg.AST |> List.head
        | Stuck ->
          let tk = cfg.Input |> List.head
          let content = tk.Content |> Token.showContent
          let pos = tk.Start
          raise (ParseError
            (sprintf "unexpected %s at position %d." content pos))
      analyseIter configuration

  /// <summary>Run LALR parsing.</summary>
  /// <returns>AST.</returns>
  let analyse = runAnalyse SDT.parseActions
