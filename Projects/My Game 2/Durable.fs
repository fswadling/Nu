namespace MyGame2

open System.Diagnostics

module Durable =
    [<NoEquality; NoComparison>]
    type Durable<'TEvent, 'TEffect, 'TResult> = 
        | Suspend of ('TEvent -> Durable<'TEvent, 'TEffect, 'TResult>)
        | Yield of 'TEffect * Durable<'TEvent, 'TEffect, 'TResult>
        | Return of 'TResult

    let retn = Return

    let rec map f = function
        | Return result -> Return (f result)
        | Suspend cont -> Suspend (fun evt -> map f (cont evt))
        | Yield (effect, next) -> Yield (effect, map f next)

    let rec bind f = function
        | Return result -> f result
        | Suspend cont -> Suspend (fun evt -> bind f (cont evt))
        | Yield (effect, next) -> Yield (effect, bind f next)

    let rec apply
        (durable1: Durable<'TEvent,'TEffect,'TResult1>)
        (durable2: Durable<'TEvent,'TEffect,'TResult2>)
        : Durable<'TEvent,'TEffect,('TResult1 * 'TResult2)> =
        match durable1, durable2 with
        | Suspend continuation1, Suspend continuation2 ->
            let continuation event =
                let stateMachine1 = continuation1 event
                let stateMachine2 = continuation2 event
                apply stateMachine1 stateMachine2

            Suspend continuation

        | Suspend continuation, Return result ->
            let continuation event =
                let stateMachine = continuation event
                apply stateMachine (Return result)

            Suspend continuation

        | Return result, Suspend continuation ->
            let continuation event =
                let stateMachine = continuation event
                apply (Return result) stateMachine

            Suspend continuation

        | Yield (effect, next), other ->
            Yield (effect, apply next other)

        | other, Yield (effect, next) ->
            Yield (effect, apply other next)

        | Return result1, Return result2 ->
            Return (result1, result2)

    let rec unmapEvent
        (mapper: 'TEvent1 -> 'TEvent2)
        (durable: Durable<'TEvent2,_,_>)
        : Durable<'TEvent1,_,_> =

        match durable with
        | Return result -> Return result
        | Yield (effect, next) -> Yield (effect, unmapEvent mapper next)
        | Suspend continuation ->

        let newContinuation =
            mapper
            >> continuation
            >> unmapEvent mapper

        Suspend newContinuation

    let rec unchooseEvent
        (chooser: 'TEvent1 -> 'TEvent2 option)
        (durable: Durable<'TEvent2,_,_>)
        : Durable<'TEvent1,_,_> =

        match durable with
        | Return result -> Return result
        | Yield (effect, next) -> Yield (effect, unchooseEvent chooser next)
        | Suspend continuation ->

        let newContinuation =
            chooser
            >> Option.map continuation
            >> Option.defaultValue durable
            >> unchooseEvent chooser

        Suspend newContinuation

    let rec unfoldEvent
        (folder: 'TAggregate -> 'TEvent -> 'TAggregate)
        (seed: 'TAggregate)
        (durable: Durable<'TAggregate,_,_>)
        : Durable<'TEvent,_,_> =

        match durable with
        | Return r -> Return r
        | Yield (effect, next) ->
            let newSeed = folder seed effect
            Yield (effect, unfoldEvent folder newSeed next)
        | Suspend continuation ->
            let newContinuation event =
                let seed = folder seed event
                let durable = continuation seed
                unfoldEvent folder seed durable
            Suspend newContinuation

    let rec mapState
        (mapper: 'TEffect -> 'TEffect2)
        (durable: Durable<'TEvent, 'TEffect, 'TResult>)
        : Durable<'TEvent, 'TEffect2, 'TResult> =

        match durable with
        | Return result -> Return result
        | Yield (effect, next) -> Yield (mapper effect, mapState mapper next)
        | Suspend continuation ->

        let newContinuation =
            continuation 
            >> mapState mapper

        Suspend newContinuation

    let rec foldState
        (seed: 'TEffect2)
        (folder: 'TEffect2 -> 'TEffect -> 'TEffect2)
        (durable: Durable<_, 'TEffect, _>)
        : Durable<_, 'TEffect2, _> =

        match durable with
        | Return result -> Return result
        | Yield (effect, next) ->
            let newSeed = folder seed effect
            Yield (newSeed, foldState newSeed folder next)
        | Suspend continuation ->
            let newContinuation event =
                let durable = continuation event
                foldState seed folder durable
            Suspend newContinuation

    let zip
        (events: 'TEvent seq)
        (durable: Durable<'TEvent,'TEffect,'TResult>)
        : Durable<'TEvent,'TEffect,'TResult> =

        let rec step (event: 'TEvent) (d: Durable<'TEvent,'TEffect,'TResult>) : Durable<'TEvent,'TEffect,'TResult> =
            match d with
            | Return result -> Return result
            | Yield (_, next) -> step event next
            | Suspend continuation -> continuation event

        events |> Seq.fold (fun d e -> step e d) durable

    let rec untilChoose chooser (durable: Durable<_, 'TEffect, _>) =
        match durable with
        | Return result -> Return result
        | Yield (effect, next) -> Yield (effect, untilChoose chooser next)
        | Suspend continuation ->

        let newContinuation event =
            let chosen = chooser event
            match chosen with
            | Some chosenEvent -> Return chosenEvent
            | None ->
            let durable = continuation event
            untilChoose chooser durable

        Suspend newContinuation

    let untilPredicate predicate (durable: Durable<_, 'TEffect, _>) =
        untilChoose
            (fun effect -> if predicate effect then Some () else None)
            durable

    let untilEvent endEvent (durable: Durable<_, 'TEffect, _>) =
        untilPredicate 
            (fun event -> event = endEvent) 
            durable

    let untilAnyEvent (durable: Durable<_, 'TEffect, _>) =
        untilPredicate
            (fun _ -> true)
            durable

    let rec suspend = Suspend (fun _ -> suspend)

    let (!) = suspend

    let (-->) durable event  = untilEvent event durable

    let indexed (durable: Durable<_, _, _>) =
        foldState 
            (0, Unchecked.defaultof<_>) (fun (ix, _) effect -> (ix + 1, effect))
            durable

    /// Run two durables in parallel (fire-and-forget).
    /// Returns when main returns, canceling the background.
    let rec spawn
        (main: Durable<'TEvent, 'TEffect, 'TResult>)
        (background: Durable<'TEvent, 'TEffect, unit>)
        : Durable<'TEvent, 'TEffect, 'TResult> =

        match main, background with
        // Main returned - done, ignore background
        | Return result, _ -> Return result

        // Background returned - continue with just main
        | main, Return () -> main

        // Main emits - Yield and recurse
        | Yield (mainState, next), bg ->
            Yield (mainState, spawn next bg)

        // Background emits - Yield and recurse  
        | main, Yield (bgState, next) ->
            Yield (bgState, spawn main next)

        // Both suspend - feed same event to both
        | Suspend mainCont, Suspend bgCont ->

            let continuation event =
                let nextMain = mainCont event
                let nextBg = bgCont event
                spawn nextMain nextBg

            Suspend continuation

    /// Run two durables in parallel, waiting for both to complete.
    /// Returns main's result only after both have finished.
    let rec join
        (main: Durable<'TEvent, 'TEffect, 'TResult>)
        (background: Durable<'TEvent, 'TEffect, unit>)
        : Durable<'TEvent, 'TEffect, 'TResult> =

        match main, background with
        // Both returned - done
        | Return result, Return () -> Return result

        // Main returned but background hasn't - continue running background
        | Return result, bg -> 
            bg |> map (fun () -> result)

        // Background returned - continue with just main
        | main, Return () -> main

        // Main emits - Yield and recurse
        | Yield (mainState, next), bg ->
            Yield (mainState, join next bg)

        // Background emits - Yield and recurse  
        | main, Yield (bgState, next) ->
            Yield (bgState, join main next)

        // Both suspend - feed same event to both
        | Suspend mainCont, Suspend bgCont ->

            let continuation event =
                join (mainCont event) (bgCont event)

            Suspend continuation

    let rec toState (durable: Durable<'TEvent, 'TEffect, 'TResult>) : 'TEffect seq =
        match durable with
        | Suspend _ -> Seq.empty
        | Yield (effect, next) -> seq { yield effect; yield! toState next }
        | Return _ -> Seq.empty

    let rec toContinuation durable =
        match durable with
        | Suspend cont -> Some cont
        | Yield (_, next) -> toContinuation next
        | Return _ -> None

    /// Marker type for parallel execution in the CE
    [<NoEquality; NoComparison>]
    type Parallel<'TEvent, 'TEffect> = 
        /// Fire-and-forget: returns when main returns, cancels background
        | Spawn of Durable<'TEvent, 'TEffect, unit>
        /// Wait for both: returns main's result after both complete
        | Join of Durable<'TEvent, 'TEffect, unit>

    type DurableBuilder() =
        [<DebuggerHidden; DebuggerStepThrough>]
        member _.Bind(m: Durable<'a,'b,'c>, f: 'c -> Durable<'a,'b,'d>) : Durable<'a,'b,'d> =
            bind f m

        /// Special Bind for Parallel - handles Spawn and Join
        [<DebuggerHidden; DebuggerStepThrough>]
        member _.Bind(
            marker: Parallel<'TEvent, 'TEffect>,
            continuation: unit -> Durable<'TEvent, 'TEffect, 'TResult>) 
            : Durable<'TEvent, 'TEffect, 'TResult> =
            match marker with
            | Spawn background -> spawn (continuation ()) background
            | Join background -> join (continuation ()) background

        [<DebuggerHidden; DebuggerStepThrough>]
        member _.Yield(value) = Yield (value, Return ())

        [<DebuggerHidden; DebuggerStepThrough>]
        member _.YieldFrom(m: Durable<'a,'b,'c>) : Durable<'a,'b,'c> = m

        /// YieldFrom for Parallel - passes through for Combine to handle
        [<DebuggerHidden; DebuggerStepThrough>]
        member _.YieldFrom(marker: Parallel<'TEvent, 'TEffect>) : Parallel<'TEvent, 'TEffect> = marker

        [<DebuggerHidden; DebuggerStepThrough>]
        member _.Return(m) =
            retn m

        [<DebuggerHidden; DebuggerStepThrough>]
        member _.ReturnFrom(m) =
            m

        [<DebuggerHidden; DebuggerStepThrough>]
        member _.Combine(a: Durable<'a,'b,'c>, b: unit -> Durable<'a,'b,'d>) : Durable<'a,'b,'d> =
            a |> bind (fun _ -> b ())

        /// Special Combine for Parallel - handles Spawn and Join
        [<DebuggerHidden; DebuggerStepThrough>]
        member _.Combine(
            marker: Parallel<'TEvent, 'TEffect>,
            continuation: unit -> Durable<'TEvent, 'TEffect, 'TResult>)
            : Durable<'TEvent, 'TEffect, 'TResult> =
            match marker with
            | Spawn background -> spawn (continuation ()) background
            | Join background -> join (continuation ()) background

        [<DebuggerHidden; DebuggerStepThrough>]
        member _.MergeSources(durable1, durable2) =
            apply durable1 durable2

        [<DebuggerHidden; DebuggerStepThrough>]
        member _.Zero() =
            Return ()

        [<DebuggerHidden; DebuggerStepThrough>]
        member _.Delay(f) = f

        [<DebuggerHidden; DebuggerStepThrough>]
        member _.Run(f) = f ()

        [<DebuggerHidden; DebuggerStepThrough>]
        member this.While(guard, body: unit -> Durable<'a,'b,unit>) =
            if not (guard())
            then this.Zero ()
            else bind (fun _ -> this.While(guard, body)) (body ())

        [<DebuggerHidden; DebuggerStepThrough>]
        member _.For(sequence, f) =
            sequence
            |> Seq.map f
            |> Seq.fold (fun sm next -> bind (fun () -> next) sm) (Return ())

    let durable = DurableBuilder()
