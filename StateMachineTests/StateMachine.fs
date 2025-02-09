
module StateMachine
    open System.Diagnostics

    type Continuation<'TEvent, 'TState, 'TResult> = 
        'TEvent -> StateMachine<'TEvent, 'TState, 'TResult>
    and StateMachine<'TEvent, 'TState, 'TResult> = 
        | Yield of 'TState * Continuation<'TEvent, 'TState, 'TResult>
        | Return of 'TResult

    let rec map 
        (mapper: 'TResult -> 'TNextResult)
        (stateMachine: StateMachine<_,_,'TResult>)
        : StateMachine<_,_,'TNextResult> =
        match stateMachine with
        | Return result -> Return (mapper result)
        | Yield (state, continuation) -> 

        let continuation = 
            continuation 
            >> map mapper

        Yield (state, continuation)

    let rec bind 
        (mapper: 'TResult -> StateMachine<_,_,'TNextResult>)
        (stateMachine: StateMachine<_,_,'TResult>)
        : StateMachine<_,_,'TNextResult> =

        match stateMachine with
        | Return result -> mapper result
        | Yield (state, continuation) ->

        let continuation =
            continuation 
            >> (bind mapper)

        Yield (state, continuation)

    let rec apply
        (combineStates: 'TState -> 'TState -> 'TState)
        (stateMachine1: StateMachine<_,_,'TResult1>)
        (stateMachine2: StateMachine<_,_,'TResult2>)
        : StateMachine<_,_,('TResult1 * 'TResult2)> =
        match stateMachine1, stateMachine2 with
        | Yield (state1, continuation1), Yield (state2, continuation2) ->

            // Both state machines yield, so yield the combination of the two
            let state = combineStates state1 state2
            let continuation event =
                let stateMachine1 = continuation1 event
                let stateMachine2 = continuation2 event
                apply combineStates stateMachine1 stateMachine2

            Yield (state, continuation)
        | Yield (state, continuation), Return result ->

            // First state machine yields, second returns, so continue with 
            // the first
            let continuation event =
                let stateMachine = continuation event
                apply combineStates stateMachine (Return result)

            Yield (state, continuation)
        | Return result, Yield (state, continuation) ->

            // First state machine returns, second yields, so continue with 
            // the second
            let continuation event =
                let stateMachine = continuation event
                apply combineStates (Return result) stateMachine

            Yield (state, continuation)
        | Return result1, Return result2 ->
            // Both state machines return, so return the combined result
            Return (result1, result2)

    let rec await 
        (state: 'TState)
        (chooser: 'TEvent -> 'TResult option)
        : StateMachine<'TEvent,'TState,'TResult> =

        let continuation event =
            chooser event
            |> Option.map Return
            |> Option.defaultValue (await state chooser)

        Yield (state, continuation)

    let rec unmapEvent
        (mapper: 'TEvent1 -> 'TEvent2)
        (stateMachine: StateMachine<'TEvent2,_,_>)
        : StateMachine<'TEvent1,_,_> =

        match stateMachine with
        | Return result -> Return result
        | Yield (state, continuation) ->

        let newContinuation =
            mapper
            >> continuation
            >> unmapEvent mapper

        Yield (state, newContinuation)

    let rec unchooseEvent
        (chooser: 'TEvent1 -> 'TEvent2 option)
        (stateMachine: StateMachine<'TEvent2,_,_>)
        : StateMachine<'TEvent1,_,_> =

        match stateMachine with
        | Return result -> Return result
        | Yield (state, continuation) ->

        let continuation =
            chooser
            >> Option.map continuation
            >> Option.defaultValue stateMachine
            >> unchooseEvent chooser

        Yield (state, continuation)

    let rec unfoldEvent
        (folder: 'TAggregate -> 'TEvent -> 'TAggregate)
        (seed: 'TAggregate)
        (stateMachine: StateMachine<'TAggregate,_,_>) 
        : StateMachine<'TEvent,_,_> =

        match stateMachine with
        | Return r -> Return r
        | Yield (state, continuation) ->

        let continuation event =
            let seed = folder seed event
            let stateMachine = continuation seed
            unfoldEvent folder seed stateMachine

        Yield (state, continuation)

    let rec mapState
        (mapper: 'TState -> 'TState2)
        (stateMachine: StateMachine<_, 'TState, _>)
        : StateMachine<_, 'TState2, _> =

        match stateMachine with
        | Return result -> Return result
        | Yield (state, continuation) ->

        let continuation =
            continuation 
            >> mapState mapper

        Yield (mapper state, continuation)

    let rec zip 
        (events: 'TEvent seq)
        (stateMachine: StateMachine<'TEvent,_,_>)
        : StateMachine<'TEvent,_,_> =

        let folder stateMachine event =
            match stateMachine with
            | Return result -> Return result
            | Yield (_, continuation) -> continuation event

        Seq.fold folder stateMachine events

    let rec onEither
        combineReturns
        combineStates
        (stateMachine1: StateMachine<_,_,_>)
        (stateMachine2: StateMachine<_,_,_>): StateMachine<_,_,_> =

        match stateMachine1, stateMachine2 with
        // Both state machines return, so combine the results 
        // and return.
        | Return result1, Return result2 ->
            let result = combineReturns result1 result2
            Return result

        // One state machine has returned. Return its result and
        // finish.
        | Return r, _
        | _, Return r -> Return r

        // Neither state machine has finished. combine the states
        // and create a new continuation that calls onEither
        // recursively.
        | Yield (state1, continuation1), Yield (state2, continuation2) ->

        let newState = combineStates state1 state2

        let newContinuation event =
            let stateMachine1 = continuation1 event
            let stateMachine2 = continuation2 event

            onEither
                combineReturns
                combineStates
                stateMachine1
                stateMachine2

        Yield (newState, newContinuation)

    let rec onLeft
        combineStates
        (primaryStateMachine: StateMachine<_,_,_>)
        (secondaryStateMachine: StateMachine<_,_,_>): StateMachine<_,_,_> =
        match primaryStateMachine, secondaryStateMachine with

        // Left state machine has returned. Return its result and
        // finish.
        | Return result, _ -> Return result

        // Right state machine has returned. 
        // Ignore its result and continue with left
        | left, Return _ -> left

        // Neither state machine has finished. combine the states
        // and create a new continuation that calls orWhen 
        // recursively.
        | Yield (primaryState, primaryContinuation),
          Yield (secondaryState, secondaryContinuation) ->

        let newState = combineStates primaryState secondaryState

        let newContinuation event =
            let stateMachine2 = secondaryContinuation event
            let stateMachine1 = primaryContinuation event

            onLeft combineStates stateMachine1 stateMachine2

        Yield (newState, newContinuation)

    let onRight
        combineStates
        (secondaryStateMachine: StateMachine<_,_,_>)
        (primaryStateMachine: StateMachine<_,_,_>): StateMachine<_,_,_> =
        onLeft combineStates primaryStateMachine secondaryStateMachine

    let rec onLeftWithUpstreamCommunication
        (combineStates: 'TPrimaryState -> 'TSecondaryState -> 'TPrimaryState)
        (state2Events: 'TSecondaryState -> 'TEvent seq)
        (primaryStateMachine: StateMachine<_,'TPrimaryState,_>)
        (secondaryStateMachine: StateMachine<_,'TSecondaryState,_>)
        : StateMachine<_,'TPrimaryState,_> =

        match secondaryStateMachine with
        | Return _ -> primaryStateMachine
        | Yield (secondaryState, secondaryContinuation) ->

        let stateEvents = state2Events secondaryState
        let primaryStateMachine = zip stateEvents primaryStateMachine

        match primaryStateMachine with
        | Return x -> Return x
        | Yield (state, primaryContinuation) ->

        let newContinuation event =
            let secondaryStateMachine = secondaryContinuation event
            let primaryStateMachine = primaryContinuation event

            onLeftWithUpstreamCommunication
                combineStates
                state2Events
                primaryStateMachine
                secondaryStateMachine

        Yield (state, newContinuation)

    let rec onRightWithUpstreamCommunication
        (combineStates: 'TPrimaryState -> 'TSecondaryState -> 'TPrimaryState)
        (state2Events: 'TSecondaryState -> 'TEvent seq)
        (secondaryStateMachine: StateMachine<_,'TSecondaryState,_>)
        (primaryStateMachine: StateMachine<_,'TPrimaryState,_>)
        : StateMachine<_,'TPrimaryState,_> =
        onLeftWithUpstreamCommunication
            combineStates
            state2Events
            primaryStateMachine
            secondaryStateMachine

    let rec private reprocess'
        // RemainingEvents contains as its first element the previous
        // event that was applied. We check this previous event
        // against the new state for any more events created by
        // state2Events, and append those to the end of remainingEvents.
        // Afterwards, we discard the previous event and try the next event
        // in the sequence on the continuations. If there are no more events, 
        // we end  the recursive reprococessing and create a new continuation.

        // Note: i'm using a seq as an immutable queue here...
        // Probably not the best choice but I don't want to 
        // deal with ImmutableQueue's lack of functions
        // or bring in an extra dependency.
        (remainingEvents: 'TEvent seq)
        (state2Events: 'TState -> 'TEvent -> 'TEvent seq)
        (stateMachine: StateMachine<'TEvent, 'TState, 'TResult>) =

        match stateMachine with
        | Return result -> Return result
        | Yield (state, continuation) ->

        // This takes the head off the sequence, checks
        // if it produces any new events with the given state,
        // and appends those events to the rest of the sequence.
        let remainingEvents = 
            remainingEvents
            |> Seq.tryHead
            |> Option.map (state2Events state)
            |> Option.defaultValue Seq.empty
            |> Seq.append
                (if Seq.isEmpty remainingEvents 
                 then Seq.empty
                 else Seq.tail remainingEvents)

        let nextContinuation event =
            event
            |> continuation
            |> reprocess' (Seq.singleton event) state2Events

        // If there is a remaining event in queue, loop the processing, 
        // if not, yield a continuation.
        remainingEvents
        |> Seq.tryHead
        |> Option.map (continuation >> reprocess' remainingEvents state2Events)
        |> Option.defaultValue (Yield (state, nextContinuation))

    let reprocess
        (state2Events: 'TState -> 'TEvent -> 'TEvent seq)
        (stateMachine: StateMachine<'TEvent, 'TState, 'TResult>) =
        reprocess' Seq.empty state2Events stateMachine

    type StateMachineBuilder() =
        [<DebuggerStepThrough>]
        member _.Bind(m, f) =
            bind f m

        [<DebuggerStepThrough>]
        member _.Return(m) =
            Return m

        [<DebuggerStepThrough>]
        member _.ReturnFrom(m) =
            m

        [<DebuggerStepThrough>]
        member _.Combine(a,b) =
            a |> bind (fun _ -> b ())

        [<DebuggerStepThrough>]
        member _.Zero() =
            Return ()

        [<DebuggerStepThrough>]
        member this.While(guard, body) =
            // evaluate test function
            if not (guard())
            then
                //exit loop
                this.Zero ()
            else
                // evaluate the body function
                this.Bind(body (), fun _ ->
                    // call recursively
                    this.While(guard, body))

        [<DebuggerStepThrough>]
        member _.For(sequence, f) =
            sequence
            |> Seq.map f
            |> Seq.fold (fun sm next -> bind (fun () -> next) sm) (Return ())

    let stateMachine = StateMachineBuilder()

    type ParallelStateMachineBuilder<'TState>
        (combineStates: 'TState -> 'TState -> 'TState) =
        inherit StateMachineBuilder()

        [<DebuggerStepThrough>]
        member _.MergeSources(stateMachine1, stateMachine2) =
            apply combineStates stateMachine1 stateMachine2