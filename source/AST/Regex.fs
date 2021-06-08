// Copyright (c) Kagurazaka K. Shinkai. All Rights Reserved. See LICENSE.txt in
// the project root for license information.

namespace KkShinkai.Regex.AST

type Regex =
  | Chr of char             // literal character
  | Concat of Regex * Regex // concatenation
  | Alter of Regex * Regex  // alternation
  | Star of Regex           // kleene star
