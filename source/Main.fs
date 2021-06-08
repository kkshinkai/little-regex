// Copyright (c) Kagurazaka K. Shinkai. All Rights Reserved. See LICENSE.txt in
// the project root for license information.

namespace KkShinkai.Regex

module Main =
  open KkShinkai.Regex.REPL

  [<EntryPoint>]
  let main _ =
    REPL.run ()
    0
