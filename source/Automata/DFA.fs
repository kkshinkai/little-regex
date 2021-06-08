// Copyright (c) Kagurazaka K. Shinkai. All Rights Reserved. See LICENSE.txt in
// the project root for license information.

namespace KkShinkai.Regex.Automata

/// <summary>
/// An implementation of deterministic finite automaton (DFA).
/// </summary>
type DFA<'state, 'input when 'state: comparison and 'input: comparison> =
  { /// <summary>A finite set of states.</summary>
    States: Set<'state>
    /// <summary>A finite set of input symbols (aka. alphabet).</summary>
    InputSymbols: Set<'input>
    /// <summary>A transition function whose type is
    /// <code>States * InputSymbols -> States</code></summary>
    Trans: Map<'state * 'input, 'state>
    /// <summary>An initial or start state.</summary>
    InitialState: 'state
    /// <summary>A set of accept states, <c>FinalStates</c> should be the subset
    /// of <code>States</code>.</summary>
    FinalStates: Set<'state> }

module DFA =

  /// <summary>Rename the status in DFA with the given sequence. The most
  /// commonly used sequence is 1, 2, 3, ..., n (aka. <c>Seq.initInfinite</c>),
  /// which will return a DFA with a status number starting from zero.</summary>
  let private renameStates newStates dfa =
    let states =
      newStates
      |> Seq.take (dfa.States |> Set.count)
      |> Set.ofSeq

    let stateMapping =
      (dfa.States, states)
      ||> Seq.zip
      |> Map.ofSeq

    let newInitialState =
      stateMapping
      |> Map.find dfa.InitialState

    let newFinalState =
      dfa.FinalStates
      |> Set.map (fun s -> stateMapping |> Map.find s)
    
    let newTrans =
      dfa.Trans
      |> Map.toSeq
      |> Seq.map (fun ((inState, chr), outState) ->
        (stateMapping |> Map.find inState, chr),
        stateMapping |> Map.find outState)
      |> Map.ofSeq

    { States = states
      Trans = newTrans
      InputSymbols = dfa.InputSymbols
      InitialState = newInitialState
      FinalStates = newFinalState }

  let rec private subsetConstructIter dfaStates dfaTrans pendingStates (nfa: NFA<_, _>) =
    match pendingStates with
    | head :: tail ->
      let newStates, newTrans =
        nfa.InputSymbols
        |> Seq.fold
          (fun (pendingStates, trans) input ->
            let nextState =
              head
              |> NFAHelper.move (Direct input) nfa
              |> NFAHelper.epsilonClosure nfa
            
            if Set.count nextState = 0 then
              (pendingStates, trans)
            else
              let newPendingStates =
                if ((dfaStates |> Set.contains nextState) || (pendingStates |> List.contains nextState))
                  then pendingStates
                  else nextState :: pendingStates
              let newTrans =
                trans
                |> Map.add (head, input) nextState
              (newPendingStates, newTrans))
          ([], Map.empty)

      nfa
      |> subsetConstructIter
        (* dfaStates =*) (dfaStates |> Set.add head)
        (* dfaTrans =*) (Seq.concat [Map.toSeq newTrans; Map.toSeq dfaTrans] |> Map.ofSeq)
        (* pendingStates =*) (newStates @ tail)
    | [] -> (dfaStates, dfaTrans)

  let private subsetConstruct (nfa: NFA<_, _>) =
    let newInitialState = set [nfa.InitialState] |> NFAHelper.epsilonClosure nfa
    let newStates, newTrans =
      nfa
      |> subsetConstructIter
        Set.empty
        Map.empty
        [newInitialState]

    let finalStates =
      newStates
      |> Set.filter (fun state ->
        state
        |> Set.intersect nfa.FinalStates
        |> Set.isEmpty
        |> not)

    { States = newStates
      Trans = newTrans
      InputSymbols = nfa.InputSymbols
      InitialState = newInitialState
      FinalStates = finalStates }
    |> renameStates (Seq.initInfinite id)

  /// <summary>Convert NFA into DFA using subset construction (aka. powerset
  /// construction)</summary>
  /// <param name="nfa">The NFA to be converted.</param>
  /// <returns>The converted DFA.</returns>
  let ofNFA nfa =
    nfa
    |> subsetConstruct
    |> renameStates (Seq.initInfinite id)

  /// <summary>Run DFA with the input sequences.</summary>
  /// <param name="inputs">The input sequences for DFA.</param>
  /// <param name="dfa">DFA.</param>
  /// <returns>Whether DFA can successfully reach the final state after
  /// receiving the input sequence.</returns>
  let run inputs dfa =
    let finalState =
      inputs
      |> Seq.scan
        (fun currentState input ->
          match currentState with
          | None -> None
          | Some state -> dfa.Trans |> Map.tryFind (state, input))
        (Some dfa.InitialState)
      |> Seq.last
    match finalState with
    | None -> false
    | Some s -> dfa.FinalStates |> Set.contains s
