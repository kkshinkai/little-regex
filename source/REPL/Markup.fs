// Copyright (c) Kagurazaka K. Shinkai. All Rights Reserved. See LICENSE.txt in
// the project root for license information.

namespace KkShinkai.Regex.REPL

open System

type StyledText =
  { Content: string
    Color: option<ConsoleColor> }

/// <summary>Provide console color output service based on ANSI escape
/// code. It is a light-weight wrapper of the module `System.Console`.</summary>
type Markup =
  { Content: list<StyledText> }
  
  static member (+) (t1, t2) = { Content = t1.Content @ t2.Content }

module Markup =
  
  let empty = { Content = [] }
  let concat = List.fold (+) empty

  let private colored c str =
    { Content = [{ Content = str; Color = Some c }] }

  let text str =
    { Content = [{ Content = str; Color = None }] }
  let textln str =
    str
    |> (+) <| "\n"
    |> text

  open type ConsoleColor

  let mark    = colored DarkYellow
  let success = colored Green
  let error   = colored Red
  let note    = colored Blue
  let border  = colored DarkGray
  let code    = colored DarkMagenta

  let print markup = 
    markup.Content
    |> List.iter (fun text -> 
      match text.Color with
      | Some c ->
          let temp = Console.ForegroundColor
          Console.ForegroundColor <- c
          printf "%s" text.Content
          Console.ForegroundColor <- temp
      | None ->
          printf "%s" text.Content)

  let println m = m + text "\n" |> print
