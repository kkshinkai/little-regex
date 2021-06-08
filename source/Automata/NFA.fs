// Copyright (c) Kagurazaka K. Shinkai. All Rights Reserved. See LICENSE.txt in
// the project root for license information.

namespace KkShinkai.Regex.Automata

/// <summary>
/// The input type of NFA.
/// </summary>
type Input<'input when 'input: comparison> =
  | Direct of 'input
  | Epsilon

/// <summary>
/// An implementation of nondeterministic finite automaton (NFA).
/// </summary>
type NFA<'state, 'input when 'state: comparison and 'input: comparison> =
  { /// <summary>A finite set of states.</summary>
    States: Set<'state>

    /// <summary>A finite set of input symbols, the input may be
    /// epsilon.</summary>
    InputSymbols: Set<'input>

    /// <summary>A transition function whose type is
    /// <code>States * Input -> StateSet</code>. For a certain state, receiving
    /// an input may lead to multiple states.</summary>
    Trans: Map<'state * Input<'input>, Set<'state>>

    /// <summary>An initial or start state.</summary>
    InitialState: 'state

    /// <summary>A set of states or final states.</summary>
    FinalStates: Set<'state> }

module NFA =

  open KkShinkai.Regex.AST

  /// <summary>Rename the status in DFA with the given sequence</summary>
  let renameStates newStates nfa =
    let states =
      newStates
      |> Seq.take (nfa.States |> Set.count)
      |> Set.ofSeq

    let stateMapping =
      (nfa.States, states)
      ||> Seq.zip
      |> Map.ofSeq

    let initialState =
      stateMapping
      |> Map.find nfa.InitialState
    let finalStates =
      nfa.FinalStates
      |> Set.map (fun s -> Map.find s stateMapping)

    { States = states
      InputSymbols = nfa.InputSymbols
      Trans = nfa.Trans
        |> Map.toSeq
        |> Seq.map (fun ((state, input), states) ->
          (stateMapping |> Map.find state, input),
          states |> Set.map (fun state -> stateMapping |> Map.find state))
        |> Map.ofSeq
      InitialState = initialState
      FinalStates = finalStates }

  /// <summary>If an NFA has more than one final state, create a single, new
  /// final state and connect all the existing final states to it.</summary>
  let private mergeFinalStates nfa =
    let nfa =
      nfa
      |> renameStates (Seq.initInfinite id)
    if Set.count nfa.FinalStates <= 1 then
      nfa
    else
      let newFinalStates = Set.ofList [(nfa.States |> Seq.max) + 1]
      let trans =
        Set.fold
          (fun t f -> t |> Map.add (f, Epsilon) newFinalStates)
          nfa.Trans nfa.FinalStates
      { nfa with
          States = nfa.States + newFinalStates
          Trans = trans
          FinalStates = newFinalStates }

  let private constructFromChr c =
    { States       = Set.ofList [0; 1]
      InputSymbols = Set.ofList [c]
      Trans        = Map.ofList [(0, Direct c), Set.ofList [1]]
      InitialState = 0
      FinalStates  = Set.ofList [1] }

  let private constructFromConcat nfa1 nfa2 =
    let nfa1 =
      nfa1
      |> mergeFinalStates
    let nfa2 =
      nfa2
      |> mergeFinalStates
      |> renameStates (Seq.initInfinite (fun i -> i + Set.count nfa1.States))

    { States       = nfa1.States + nfa2.States
      InputSymbols = nfa1.InputSymbols + nfa2.InputSymbols
      Trans        =
        (nfa1.Trans |> Map.toSeq, nfa2.Trans |> Map.toSeq)
        ||> Seq.append
        |> Map.ofSeq
        |> Map.add (nfa1.FinalStates |> Seq.head, Epsilon) (Set.ofList [nfa2.InitialState])
      InitialState = nfa1.InitialState
      FinalStates  = nfa2.FinalStates }

  let private constructFromAlter nfa1 nfa2 =
    let nfa1 =
      nfa1
      |> mergeFinalStates
    let nfa2 =
      nfa2
      |> mergeFinalStates
      |> renameStates (Seq.initInfinite (fun i -> i + Set.count nfa1.States))

    let newInitialState = Set.count nfa1.States + Set.count nfa2.States
    let newFinalState = newInitialState + 1

    let newTrans =
      (nfa1.Trans |> Map.toSeq, nfa2.Trans |> Map.toSeq)
      ||> Seq.append
      |> Map.ofSeq
      |> Map.add
        (newInitialState, Epsilon)
        (Set.ofList [nfa1.InitialState; nfa2.InitialState])
      |> Map.add
        (nfa1.FinalStates |> Seq.head, Epsilon)
        (Set.ofList [newFinalState])
      |> Map.add
        (nfa2.FinalStates |> Seq.head, Epsilon)
        (Set.ofList [newFinalState])
    { States = nfa1.States + nfa2.States + Set.ofList [newInitialState; newFinalState]
      InputSymbols = nfa1.InputSymbols + nfa2.InputSymbols
      Trans = newTrans
      InitialState =  newInitialState
      FinalStates = Set.ofList [newFinalState] }

  let private constructFromStar nfa =
    let nfa =
      nfa
      |> mergeFinalStates

    let newInitialState = nfa.States |> Set.count
    let newFinalState = newInitialState + 1

    let newTrans =
      nfa.Trans
      |> Map.add
        (nfa.FinalStates |> Seq.head, Epsilon)
        (Set.ofList [nfa.InitialState; newFinalState])
      |> Map.add
        (newInitialState, Epsilon)
        (Set.ofList [nfa.InitialState; newFinalState])
    { States = nfa.States + Set.ofList [newInitialState; newFinalState]
      InputSymbols = nfa.InputSymbols
      Trans = newTrans
      InitialState = newInitialState
      FinalStates = Set.ofList [newFinalState] }

  let rec private thompsonConstruct = function
    | Chr c -> c |> constructFromChr
    | Concat (r1, r2) ->
      (r1 |> thompsonConstruct, r2 |> thompsonConstruct)
      ||> constructFromConcat
    | Alter (r1, r2) ->
      (r1 |> thompsonConstruct, r2 |> thompsonConstruct)
      ||> constructFromAlter
    | Star r ->
      r
      |> thompsonConstruct
      |> constructFromStar

  /// <summary>Convert the AST of regular expression into NFA using Thompson's
  /// construction.</summary>
  let ofRegex = thompsonConstruct

module private NFAHelper =
  let move input nfa states =
    states
    |> Seq.choose (fun s -> nfa.Trans |> Map.tryFind (s, input))
    |> Seq.fold (+) Set.empty

  let rec epsilonClosure nfa states =
    let newStates =
      states
      |> Set.fold
        (fun states state ->
          states + (set [state] |> move Epsilon nfa))
        Set.empty

    if Set.isSubset newStates states
      then states
      else (states + newStates) |> epsilonClosure nfa
