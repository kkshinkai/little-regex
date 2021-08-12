> ⚠️ **Deprecated**: Do not continue to adapt Unicode parsing, move to https://github.com/kkshinkai/lex (is Standard ML)
    
Little Regex
============

A small program I written while learning F#.

Features
--------

-   [x] Generate AST from regular expression (LALR parser / SDT);
-   [x] Generate DFA from AST (Thompson's construction);
-   [x] Convert NFA to DFA (Subset construction);
-   [ ] Minimize DFA (Hopcroft Algorithm);
-   [ ] Support UTF-8;

Quick Start
-----------

Install [.NET Core] and run the following commands:

    cd <path-to-proj-dir>
    dotnet run

[.NET Core]: https://dotnet.microsoft.com/download

Examples
--------

    >>> parse (a|b)*|(c|d)*

    ast "(a|b)*|(c|d)*"
    `- alter |
      |- alter |
      | |- char 'a'
      | `- char 'b'
      `- alter |
        |- char 'c'
        `- char 'd'

    >>> match (a|b)* ababbb
    match

    >>> match (a|b)* abbbc
    mismatch

    >>> match (a|b)*(c*|d) aabababbccc 
    match

    >>> match (a|b)*(c*|d) aabababbd 
    match

    >>> match (a|b)*(c*|d) aabababbcd
    mismatch

License
-------

Microsoft Reciprocal License (MS-RL), see [LICENSE.txt] for details.

[LICENSE.txt]: ./LICENSE.txt
