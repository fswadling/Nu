module Tests

open Xunit
open StateMachine

type IsAgeAnswered = bool
type IsNameComplimented = bool

type SharingDateEvent = 
    | Response of string
    | AgeAskedEvent
    | NameComplimentedEvent

type FoldedSharingDataEvent =
    string * IsAgeAnswered * IsNameComplimented

module FoldedSharingDataEvent =
    let foldAgeAskedEvents (answer, isAgeAsked) = function
        | Response answer -> answer, isAgeAsked
        | AgeAskedEvent -> answer, true
        | _ -> (answer, isAgeAsked)

    let foldNameComplimentedEvents (answer, isNameComplimented) = function
        | Response answer -> answer, isNameComplimented
        | NameComplimentedEvent -> answer, true
        | _ -> answer, isNameComplimented

type SharingDataState =
    string * string array * IsAgeAnswered * IsNameComplimented

module SharingDataState =
    let combine
        ((prompt1, answers1, isAgeAsked1, isNameComplimented1))
        ((prompt2, answers2, isAgeAsked2, isNameComplimented2))
        : SharingDataState =
        $"{prompt1}, {prompt2}",
        Array.append answers1 answers2,
        isAgeAsked1 || isAgeAsked2,
        isNameComplimented1 || isNameComplimented2

let state2Events
    (state: SharingDataState)
    (event: SharingDateEvent)
    : SharingDateEvent seq =
    match event, state with
    | Response _, (_,_,true,true) ->
        [AgeAskedEvent; NameComplimentedEvent]
    | Response _, (_,_,true,_) ->
        [AgeAskedEvent]
    | Response _, (_,_,_,true) ->
        [NameComplimentedEvent]
    | NameComplimentedEvent, (_,_,true,_) ->
        [AgeAskedEvent]
    | AgeAskedEvent, (_,_,_,true) ->
        [NameComplimentedEvent]
    | _, _ ->
        []

let sm1 =
    ParallelStateMachineBuilder(SharingDataState.combine) {
        let! age =
            stateMachine {
                let! age = 
                    await
                        ("How old are you?", [| "15"; "16"; "17" |], false)
                        (function
                        | (ans,_) 
                            when
                                Array.contains ans [| "15"; "16"; "17" |] ->
                            Some ans
                        | _ -> None)

                do!
                    await
                        ("I also need to compliment your name", [||], true)
                        (function
                        | (_, true) ->
                            Some () 
                        | _ -> None)

                do!
                    await
                        ("That's a nice age!", 
                        [| "Yes it is a nice age" |],
                        false)
                        (function
                        | (ans,_) 
                            when 
                                Array.contains 
                                    ans [| "Yes it is a nice age" |] ->
                            Some ()
                        | _ -> None)

                return age
            }
            |> StateMachine.unfoldEvent
                FoldedSharingDataEvent.foldNameComplimentedEvents 
                    ("", false)
            |> StateMachine.mapState
                (fun (ans, options, isAgeAsked) -> 
                    (ans, options, isAgeAsked, false))
        and! name =
            stateMachine {
                let! name = 
                    await
                        ("Whats your name?", [| "Jim"; "Jack" |], false)
                        (function
                            | (ans, _) 
                                when Array.contains ans [| "Jim"; "Jack" |] ->
                                Some ans
                            | _ -> None)

                do!
                    await
                        ("I also need your age.", [||], false)
                        (function
                            | (_, true) -> Some ()
                            | _ -> None)

                let! _, _ = 
                    await
                        ($"{name} is a nice name!",
                        [| "Yes it is a nice name" |],
                        true)
                        (function
                            | (ans, isAgeAsked) 
                                when 
                                    Array.contains
                                        ans
                                        [| "Yes it is a nice name" |] ->
                                Some (ans, isAgeAsked)
                            | _ -> None)

                return name
            }
            |> StateMachine.unfoldEvent
                FoldedSharingDataEvent.foldAgeAskedEvents ("", false)
            |> StateMachine.mapState
                (fun (ans, options, isNameComplimented) -> 
                    (ans, options, false, isNameComplimented))

        return name, age
    }
    |> StateMachine.reprocess state2Events
    |> StateMachine.unmapEvent Response
    |> StateMachine.mapState
        (fun (prompt, answers, _, _) -> prompt, answers)

[<Fact>]
let ``Check that cross communicating state machines work`` () =
    let (Yield (ef, continuation)) = sm1
    let (Yield (ef, continuation)) = continuation "sdsd"
    let (Yield (ef, continuation)) = continuation "sdsd"
    let (Yield (ef, continuation)) = continuation "Jim"
    let (Yield (ef, continuation)) = continuation "sdsd"
    let (Yield (ef, continuation)) = continuation "sdsd"
    let (Yield (ef, continuation)) = continuation "15"
    let (Yield (ef, continuation)) = continuation "sdsd"
    let (Yield (ef, continuation)) = continuation "sdsd"
    let (Yield (ef, continuation)) = continuation "Yes it is a nice age"
    let x = continuation "Yes it is a nice name"
    let x = ef
    ()

type Event =
    | Response of string
    | AgeAnswered

let combineEffects (prompt, answers) (prompt2, answers2, _) =
     $"{prompt}, {prompt2}", Array.append answers answers2

let events2Effects (_,_,isAgeAnswered) =
    if isAgeAnswered 
    then seq { yield AgeAnswered } 
    else Seq.empty

let foldPrimaryEvents (ans, isAgeAnswered) event =
    match event with 
    | AgeAnswered -> (ans,true)
    | Response ans -> (ans, isAgeAnswered)

let primary = 
    stateMachine {
        let! name = 
            await
                ("Whats your name?", [| "Jim"; "Jack" |])
                (function
                    | (ans, _)
                        when Array.contains ans [| "Jim"; "Jack" |] ->
                        Some ans
                    | _ -> None)

        do!
            await
                ("I also need your age.", [||])
                (function
                    | (_, true) -> Some ()
                    | _ -> None)

        let! _, _ = 
            await
                ($"{name} is a nice name!",
                [| "Yes it is a nice name" |])
                (function
                    | (ans, isAgeAsked) when
                        Array.contains ans [| "Yes it is a nice name" |] ->
                        Some (ans, isAgeAsked)
                    | _ -> None)

        return name
    }
    |> StateMachine.unfoldEvent foldPrimaryEvents ("", false)

let secondaryEventChooser = function 
    | Response ans -> Some ans 
    | _ -> None

let secondary =
    stateMachine {
        let! age = 
            await
                ("How old are you?", [| "15"; "16"; "17" |], false)
                (function
                | ans when Array.contains ans [| "15"; "16"; "17" |] ->
                    Some ans
                | _ -> None)

        do!
            await
                ("That's a nice age!", [| "Yes it is a nice age" |], true)
                (function
                | ans when
                    Array.contains ans [| "Yes it is a nice age" |] ->
                    Some ()
                | _ -> None)

        return age
    }
    |> StateMachine.unchooseEvent secondaryEventChooser

let stateMachine =
    onLeftWithUpstreamCommunication
        combineEffects
        events2Effects
        primary
        secondary
    |> StateMachine.unmapEvent Response

[<Fact>]
let ``Check that one directional state machine communicating works`` () =
    let (Yield (ef, continuation)) = stateMachine
    let (Yield (ef, continuation)) = continuation "Jim"
    let (Yield (ef, continuation)) = continuation "15"
    let (Yield (ef, continuation)) = continuation "Yes it is a nice age"
    let x = continuation "Yes it is a nice name"
    let x = ef
    ()