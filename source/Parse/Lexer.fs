// Copyright (c) Kagurazaka K. Shinkai. All Rights Reserved. See LICENSE.txt in
// the project root for license information.

namespace KkShinkai.Regex.Parse

exception LexError of string

module Lexer =

  let private isPrintable chr =
    int chr >= 32 && int chr <= 126

  let private decodeEscaped char =
    ['s', ' '; '\\', '\\'; '*', '*'; '|', '|'; '(', ')'; ')', ')']
    |> Map.ofList
    |> Map.tryFind char

  let rec private lexAll tokens position = function
    | [] ->
      ({ Content = EOI; Start = position; End = position } :: tokens)
      |> List.rev
    | '\\'::cs ->
      cs
      |> lexEscapedCharacter tokens position
    | cs ->
      cs
      |> lexSimpleCharacter tokens position

  and private lexEscapedCharacter tokens position = function
    | [] ->
      raise (LexError
        ( $"unexpected end of regular expression at position {position + 1}," +
          "an escape character should followed after '\\'." ))
    | c::cs ->
    let c =
      match decodeEscaped c with
      | Some c -> c
      | None ->
        raise (LexError
          ( if c = 'n' || c = 'r'
              then "'\\n' and '\\r' n are deliberately not supported. Using " +
                   "the Enter key in the REPL does not produce a newline " +
                   "character. In other words, this regular expression will " +
                   "never match any input string."
              else $"illegal escape character '\\{c}' at position {position + 1}." ))
    cs
    |> lexAll
      ({ Content = Char c; Start = position; End = position + 1 } :: tokens)
      (position + 2)

  and private lexSimpleCharacter tokens position cs =
    let content =
      match cs |> List.head with
      | '*' -> Asterisk
      | '|' -> Midline
      | '(' -> LParen
      | ')' -> RParen
      | c when c |> isPrintable -> Char c
      | _ ->
        raise (LexError
          ( $"unsupported character at position {position}, it is an " +
            "unprintable character." ))
    cs
    |> List.tail
    |> lexAll
      ({ Content = content; Start = position; End = position } :: tokens)
      (position + 1)

  /// <summary>Run lexical analysis.</summary>
  /// <param name="source">Regular expression source code.</param>
  /// <returns>Token stream.</returns>
  let analyze source =
    lexAll (* tokenStream=*)[] (* position=*)0 (source |> List.ofSeq)
