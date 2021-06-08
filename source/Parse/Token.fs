// Copyright (c) Kagurazaka K. Shinkai. All Rights Reserved. See LICENSE.txt in
// the project root for license information.

namespace KkShinkai.Regex.Parse

type TokenContent =
  | EOI
  | Midline
  | Asterisk
  | LParen
  | RParen
  | Char of char

/// <summary>Token of the regular exprssion.
///
/// The lexical definition is as follows:
/// <code>
/// EOI              ::= '\n'
/// VerticalLine     ::= '|'
/// Asterisk         ::= '*'
/// LeftParenthesis  ::= '('
/// RightParenthesis ::= ')'
/// Character        ::= a-z | A-Z | 0-9
///                    | '\n' | '\r'
///                    | '\*' | '\|' | '\\' | '\(' | '\)'
///                    | "other printable ASCII characters"
/// </code>
/// </summary>
type Token =
  { Content: TokenContent
    Start: int
    End: int }

module Token =
  let showContent = function
    | EOI -> "end of regex"
    | Midline -> "vertical line '|'"
    | Asterisk -> "sterisk '*'"
    | LParen -> "left parenthesis '('"
    | RParen -> "right parenthesis ')'"
    | Char c -> $"character '{c}'"
