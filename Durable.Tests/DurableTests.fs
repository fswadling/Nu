module DurableTests

open NUnit.Framework
open Durable.Tests.Durable

[<TestFixture>]
type DurableTests() =

    // ===== Basic Construction Tests =====

    [<Test>]
    member _.``Return creates a Return case``() =
        let d = Return 42
        match d with
        | Return x -> Assert.That(x, Is.EqualTo(42))
        | _ -> Assert.Fail("Expected Return")

    [<Test>]
    member _.``Yield creates an Yield case with continuation``() =
        let d = Yield ("effect", Return ())
        match d with
        | Yield (effect, next) ->
            Assert.That(effect, Is.EqualTo("effect"))
            match next with
            | Return () -> ()
            | _ -> Assert.Fail("Expected Return () as continuation")
        | _ -> Assert.Fail("Expected Yield")

    [<Test>]
    member _.``Suspend creates a Suspend case with continuation``() =
        let d = Suspend (fun _ -> Return 1)
        match d with
        | Suspend _ -> ()
        | _ -> Assert.Fail("Expected Suspend")

    // ===== Map Tests =====

    [<Test>]
    member _.``map transforms Return result``() =
        let d = Return 10 |> map ((*) 2)
        match d with
        | Return x -> Assert.That(x, Is.EqualTo(20))
        | _ -> Assert.Fail("Expected Return")

    [<Test>]
    member _.``map transforms result through Yield``() =
        let d = Yield ("A", Return 10) |> map ((*) 2)
        match d with
        | Yield ("A", Return 20) -> ()
        | _ -> Assert.Fail("Expected Yield with mapped result")

    [<Test>]
    member _.``map transforms result through Suspend``() =
        let d = Suspend (fun _ -> Return 10) |> map ((*) 2)
        match d with
        | Suspend cont ->
            match cont () with
            | Return 20 -> ()
            | _ -> Assert.Fail("Expected mapped result after continuation")
        | _ -> Assert.Fail("Expected Suspend")

    // ===== Bind Tests =====

    [<Test>]
    member _.``bind chains Return values``() =
        let d = Return 10 |> bind (fun x -> Return (x * 2))
        match d with
        | Return 20 -> ()
        | _ -> Assert.Fail("Expected Return 20")

    [<Test>]
    member _.``bind chains through Yield``() =
        let d = Yield ("A", Return 10) |> bind (fun x -> Yield ("B", Return (x * 2)))
        match d with
        | Yield ("A", Yield ("B", Return 20)) -> ()
        | _ -> Assert.Fail("Expected chained Emits")

    [<Test>]
    member _.``bind chains through Suspend``() =
        let d = Suspend (fun e -> Return e) |> bind (fun x -> Return (x + 1))
        match d with
        | Suspend cont ->
            match cont 10 with
            | Return 11 -> ()
            | _ -> Assert.Fail("Expected bound result")
        | _ -> Assert.Fail("Expected Suspend")

    // ===== Yield Chaining Tests =====

    [<Test>]
    member _.``multiple Emits chain correctly``() =
        let d = 
            Yield ("A", Return ())
            |> bind (fun () -> Yield ("B", Return ()))
            |> bind (fun () -> Yield ("C", Return ()))
        
        match d with
        | Yield ("A", Yield ("B", Yield ("C", Return ()))) -> ()
        | _ -> Assert.Fail("Expected A -> B -> C chain")

    [<Test>]
    member _.``toEffect collects all emitted effects``() =
        let d : Durable<unit, string, unit> = Yield ("A", Yield ("B", Yield ("C", Return ())))
        let effects = toEffect d |> Seq.toList
        Assert.That(effects, Is.EqualTo<string list>(["A"; "B"; "C"]))

    [<Test>]
    member _.``toEffect stops at Suspend``() =
        let d : Durable<unit, string, unit> = Yield ("A", Yield ("B", Suspend (fun _ -> Return ())))
        let effects = toEffect d |> Seq.toList
        Assert.That(effects, Is.EqualTo<string list>(["A"; "B"]))

    // ===== Suspend Tests =====

    [<Test>]
    member _.``Suspend waits for event then continues``() =
        let d = Suspend (fun event -> Return event)
        match d with
        | Suspend cont ->
            match cont "event!" with
            | Return "event!" -> ()
            | _ -> Assert.Fail("Expected event to be returned")
        | _ -> Assert.Fail("Expected Suspend")

    [<Test>]
    member _.``toContinuation returns continuation from Suspend``() =
        let d = Suspend (fun e -> Return e)
        match toContinuation d with
        | Some cont ->
            match cont "test" with
            | Return "test" -> ()
            | _ -> Assert.Fail("Expected continuation to work")
        | None -> Assert.Fail("Expected Some continuation")

    [<Test>]
    member _.``toContinuation skips through Emits to find Suspend``() =
        let d = Yield ("A", Yield ("B", Suspend (fun e -> Return e)))
        match toContinuation d with
        | Some cont ->
            match cont "test" with
            | Return "test" -> ()
            | _ -> Assert.Fail("Expected continuation to work")
        | None -> Assert.Fail("Expected Some continuation")

    [<Test>]
    member _.``toContinuation returns None for Return``() =
        let d = Return ()
        match toContinuation d with
        | None -> ()
        | Some _ -> Assert.Fail("Expected None for Return")

    // ===== mapEffect Tests =====

    [<Test>]
    member _.``mapEffect transforms emitted effects``() =
        let d : Durable<unit, int, unit> = Yield (1, Yield (2, Return ())) |> mapEffect ((*) 10)
        let effects = toEffect d |> Seq.toList
        Assert.That(effects, Is.EqualTo<int list>([10; 20]))

    // ===== foldEffect Tests =====

    [<Test>]
    member _.``foldEffect accumulates emitted effects``() =
        let d = Yield (1, Yield (2, Yield (3, Return ()))) |> foldEffect 0 (+)
        let effects = toEffect d |> Seq.toList
        Assert.That(effects, Is.EqualTo<int list>([1; 3; 6]))

    // ===== zip Tests =====

    [<Test>]
    member _.``zip feeds events to Suspend continuations``() =
        let d = Suspend (fun e -> Suspend (fun e2 -> Return (e + e2)))
        let result = zip [1; 2] d
        match result with
        | Return 3 -> ()
        | _ -> Assert.Fail("Expected Return 3")

    [<Test>]
    member _.``zip skips Emits when feeding events``() =
        let d : Durable<int, int, int> = Yield (0, Suspend (fun e -> Return e))
        let result = zip [42] d
        match result with
        | Return 42 -> ()
        | _ -> Assert.Fail("Expected Return 42")

    // ===== apply Tests =====

    [<Test>]
    member _.``apply combines two Return values``() =
        let d = apply (Return 1) (Return 2)
        match d with
        | Return (1, 2) -> ()
        | _ -> Assert.Fail("Expected Return (1, 2)")

    [<Test>]
    member _.``apply emits effects from both durables``() =
        let d1 : Durable<unit, string, int> = Yield ("A", Return 1)
        let d2 : Durable<unit, string, int> = Yield ("B", Return 2)
        let d = apply d1 d2
        let effects = toEffect d |> Seq.toList
        Assert.That(effects, Is.EqualTo<string list>(["A"; "B"]))

    // ===== repeat Tests =====

    [<Test>]
    member _.``repeat creates Yield then Suspend loop``() =
        let d = repeat "tick"
        match d with
        | Yield ("tick", Suspend cont) ->
            match cont () with
            | Yield ("tick", Suspend _) -> ()
            | _ -> Assert.Fail("Expected another Yield/Suspend")
        | _ -> Assert.Fail("Expected Yield then Suspend")

    // ===== untilChoose Tests =====

    [<Test>]
    member _.``untilChoose terminates when chooser returns Some``() =
        let d = repeat "waiting" |> untilChoose (fun e -> if e = "stop" then Some "stopped" else None)
        match d with
        | Yield ("waiting", Suspend cont) ->
            match cont "go" with
            | Yield ("waiting", Suspend cont2) ->
                match cont2 "stop" with
                | Return "stopped" -> ()
                | _ -> Assert.Fail("Expected Return after stop")
            | _ -> Assert.Fail("Expected continue after go")
        | _ -> Assert.Fail("Expected Yield then Suspend")

    // ===== spawn Tests =====

    [<Test>]
    member _.``spawn returns when main returns``() =
        let main = Return 42
        let bg : Durable<unit, string, unit> = Yield ("bg", Return ())
        let spawned = spawn main bg
        match spawned with
        | Return 42 -> ()
        | _ -> Assert.Fail("Expected Return 42")

    [<Test>]
    member _.``spawn continues with main when background returns``() =
        let main : Durable<unit, string, int> = Yield ("main", Return 42)
        let bg : Durable<unit, string, unit> = Return ()
        let spawned = spawn main bg
        match spawned with
        | Yield ("main", Return 42) -> ()
        | _ -> Assert.Fail("Expected main to continue after bg returns")

    [<Test>]
    member _.``spawn emits from both branches``() =
        let main : Durable<unit, string, int> = Yield ("main1", Yield ("main2", Return 1))
        let bg : Durable<unit, string, unit> = Yield ("bg1", Yield ("bg2", Return ()))
        let spawned = spawn main bg
        let effects = toEffect spawned |> Seq.toList
        // Main emits first ("main1"), then recurses with bg
        // After main's second Yield ("main2"), main returns, so bg emissions stop
        Assert.That(effects, Is.EqualTo<string list>(["main1"; "main2"]))

    [<Test>]
    member _.``spawn suspends when both suspend``() =
        let main : Durable<int, int, int> = Yield (10, Suspend (fun e -> Return e))
        let bg : Durable<int, int, unit> = Yield (5, Suspend (fun _ -> Return ()))
        let spawned = spawn main bg
        // Main emits first (10), then bg emits (5), then both suspend
        match spawned with
        | Yield (10, Yield (5, Suspend _)) -> ()
        | _ -> Assert.Fail("Expected Yield 10 then Yield 5 then Suspend")

    [<Test>]
    member _.``spawn sends same event to both branches``() =
        let main : Durable<int, int, int> = Suspend (fun e -> Return e)
        let bg : Durable<int, int, unit> = Suspend (fun e -> Yield (e * 10, Return ()))
        let spawned = spawn main bg
        match spawned with
        | Suspend cont ->
            match cont 5 with
            | Return 5 -> () // main got 5, returned it
            | _ -> Assert.Fail("Expected Return 5 from main")
        | _ -> Assert.Fail("Expected Suspend")

    [<Test>]
    member _.``spawn with list effects``() =
        let main : Durable<unit, string list, int> = Yield (["main"], Return 42)
        let bg : Durable<unit, string list, unit> = Yield (["bg"], Return ())
        let spawned = spawn main bg
        let effects = toEffect spawned |> Seq.toList
        // Main emits first, then returns - bg never gets to Yield
        Assert.That(effects, Is.EqualTo<string list list>([["main"]]))

    [<Test>]
    member _.``spawn interleaves emits when both have suspends``() =
        // This test shows interleaving when both branches have work to do
        let main : Durable<int, string, int> = 
            Yield ("main1", Yield ("main-wait", Suspend (fun e -> Yield ("main2", Return e))))
        let bg : Durable<int, string, unit> = 
            Yield ("bg1", Yield ("bg-wait", Suspend (fun _ -> Yield ("bg2", Return ()))))
        let spawned = spawn main bg
        
        // Collect effects before suspend
        let initialStates = toEffect spawned |> Seq.toList
        // main1 emits, main-wait emits, bg1 emits, bg-wait emits - then both suspend
        Assert.That(initialStates, Is.EqualTo<string list>(["main1"; "main-wait"; "bg1"; "bg-wait"]))

    [<Test>]
    member _.``spawn background completing mid-run continues main``() =
        let main : Durable<int, string, int> = 
            Yield ("main-wait1", Suspend (fun e -> 
                Yield ("main-wait2", Suspend (fun e2 -> Return (e + e2)))))
        let bg : Durable<int, string, unit> = 
            Yield ("bg-wait", Suspend (fun _ -> Return ()))
        let spawned = spawn main bg
        
        // First: main emits, then bg emits, then both suspend
        match spawned with
        | Yield ("main-wait1", Yield ("bg-wait", Suspend cont)) ->
            let afterFirst = cont 10
            // bg returned, main continues alone
            match afterFirst with
            | Yield ("main-wait2", Suspend cont2) ->
                match cont2 20 with
                | Return 30 -> ()
                | _ -> Assert.Fail("Expected Return 30")
            | _ -> Assert.Fail("Expected main to continue alone")
        | _ -> Assert.Fail("Expected Yield then Suspend")

    // ===== join Tests =====

    [<Test>]
    member _.``join returns when both return``() =
        let main = Return 42
        let bg : Durable<unit, string, unit> = Return ()
        let joined = join main bg
        match joined with
        | Return 42 -> ()
        | _ -> Assert.Fail("Expected Return 42")

    [<Test>]
    member _.``join waits for background when main returns first``() =
        let main : Durable<unit, string, int> = Return 42
        let bg : Durable<unit, string, unit> = Yield ("bg1", Yield ("bg2", Return ()))
        let joined = join main bg
        // Main returned but bg hasn't - should continue running bg
        let effects = toEffect joined |> Seq.toList
        Assert.That(effects, Is.EqualTo<string list>(["bg1"; "bg2"]))
        // And should eventually return main's result
        match joined with
        | Yield (_, Yield (_, Return 42)) -> ()
        | _ -> Assert.Fail("Expected bg to Yield then Return 42")

    [<Test>]
    member _.``join continues with main when background returns first``() =
        let main : Durable<unit, string, int> = Yield ("main1", Yield ("main2", Return 42))
        let bg : Durable<unit, string, unit> = Return ()
        let joined = join main bg
        let effects = toEffect joined |> Seq.toList
        Assert.That(effects, Is.EqualTo<string list>(["main1"; "main2"]))

    [<Test>]
    member _.``join with suspending background waits after main returns``() =
        let main : Durable<int, string, int> = Yield ("main", Return 42)
        let bg : Durable<int, string, unit> = Yield ("bg-wait", Suspend (fun _ -> Return ()))
        let joined = join main bg
        // Main emits and returns, bg emits and suspends
        // join should continue running bg
        match joined with
        | Yield ("main", Yield ("bg-wait", Suspend cont)) ->
            match cont 0 with
            | Return 42 -> ()
            | _ -> Assert.Fail("Expected Return 42 after bg completes")
        | _ -> Assert.Fail("Expected main Yield, bg Yield, then suspend")

    [<Test>]
    member _.``join emits from both branches``() =
        let main : Durable<unit, string, int> = Yield ("main1", Yield ("main2", Return 1))
        let bg : Durable<unit, string, unit> = Yield ("bg1", Yield ("bg2", Return ()))
        let joined = join main bg
        let effects = toEffect joined |> Seq.toList
        // Main emits first, then bg emits - all effects should be collected
        Assert.That(effects, Is.EqualTo<string list>(["main1"; "main2"; "bg1"; "bg2"]))

    // ===== Join CE Tests =====

    [<Test>]
    member _.``Join CE waits for background``() =
        let d : Durable<unit, string, unit> = durable {
            do! Join (durable {
                yield "bg1"
                yield "bg2"
            })
            yield "after"
        }
        let effects = toEffect d |> Seq.toList
        // Main is just "after", bg has "bg1", "bg2"
        // Main returns immediately, but join waits for bg
        Assert.That(effects, Is.EqualTo<string list>(["after"; "bg1"; "bg2"]))

    [<Test>]
    member _.``Join CE with yield! waits for background``() =
        let d : Durable<unit, string, unit> = durable {
            yield! Join (durable {
                yield "bg1"
                yield "bg2"
            })
            yield "after"
        }
        let effects = toEffect d |> Seq.toList
        Assert.That(effects, Is.EqualTo<string list>(["after"; "bg1"; "bg2"]))

    // ===== Spawn CE Tests (do!) =====

    [<Test>]
    member _.``Spawn with do! spawns continuation with background``() =
        let d : Durable<unit, string, unit> = durable {
            yield "before"
            do! Spawn (durable {
                yield "bg1"
                yield "bg2"
            })
            yield "after"
        }
        let effects = toEffect d |> Seq.toList
        // "before" emits, then spawn happens: "after" is main, bg is background
        // Main emits first in spawn, so "after" before bg effects
        Assert.That(effects, Is.EqualTo<string list>(["before"; "after"]))

    [<Test>]
    member _.``Spawn with do! and suspending background``() =
        let d : Durable<int, string, unit> = durable {
            yield "before"
            do! Spawn (durable {
                yield "bg-start"
                yield! repeat "bg-wait" |> untilAnyEvent
                yield "bg-end"
            })
            yield "main-after"
            yield! repeat "main-wait" |> untilAnyEvent
            yield "main-end"
        }
        let effects = toEffect d |> Seq.toList
        // Main emits all its effects first (including the wait), then bg emits
        Assert.That(effects, Is.EqualTo<string list>(["before"; "main-after"; "main-wait"; "bg-start"; "bg-wait"]))

    [<Test>]
    member _.``Spawn with do! main completing ends spawn``() =
        let d : Durable<unit, string, int> = durable {
            do! Spawn (durable {
                yield "bg1"
                yield "bg2"
                yield "bg3"
            })
            yield "main"
            return 42
        }
        match d with
        | Yield ("main", Return 42) -> ()
        | _ -> Assert.Fail("Expected main to complete, ignoring bg")

    [<Test>]
    member _.``Spawn with do! multiple spawns``() =
        let d : Durable<unit, string, unit> = durable {
            do! Spawn (durable { yield "bg1" })
            yield "middle"
            do! Spawn (durable { yield "bg2" })
            yield "end"
        }
        let effects = toEffect d |> Seq.toList
        // First spawn: main is (middle; second spawn; end), bg1 is background
        // After middle emits, second spawn: main is (end), bg2 is background
        Assert.That(effects, Is.EqualTo<string list>(["middle"; "end"]))

    // ===== Spawn CE Tests (yield!) =====

    [<Test>]
    member _.``Spawn with yield! spawns continuation with background``() =
        let d : Durable<unit, string, unit> = durable {
            yield "before"
            yield! Spawn (durable {
                yield "bg1"
                yield "bg2"
            })
            yield "after"
        }
        let effects = toEffect d |> Seq.toList
        Assert.That(effects, Is.EqualTo<string list>(["before"; "after"]))

    [<Test>]
    member _.``Spawn with yield! and suspending background``() =
        let d : Durable<int, string, unit> = durable {
            yield "before"
            yield! Spawn (durable {
                yield "bg-start"
                yield! repeat "bg-wait" |> untilAnyEvent
            })
            yield "main-after"
            yield! repeat "main-wait" |> untilAnyEvent
        }
        let effects = toEffect d |> Seq.toList
        Assert.That(effects, Is.EqualTo<string list>(["before"; "main-after"; "main-wait"; "bg-start"; "bg-wait"]))

    [<Test>]
    member _.``Spawn with yield! multiple spawns``() =
        let d : Durable<unit, string, unit> = durable {
            yield! Spawn (durable { yield "bg1" })
            yield "middle"
            yield! Spawn (durable { yield "bg2" })
            yield "end"
        }
        let effects = toEffect d |> Seq.toList
        Assert.That(effects, Is.EqualTo<string list>(["middle"; "end"]))

    // ===== CE Builder Tests =====

    [<Test>]
    member _.``CE yield creates Yield``() =
        let d = durable { yield "A" }
        match d with
        | Yield ("A", Return ()) -> ()
        | _ -> Assert.Fail("Expected Yield")

    [<Test>]
    member _.``CE multiple yields chain``() =
        let d : Durable<unit, string, unit> = durable {
            yield "A"
            yield "B"
            yield "C"
        }
        let effects = toEffect d |> Seq.toList
        Assert.That(effects, Is.EqualTo<string list>(["A"; "B"; "C"]))

    [<Test>]
    member _.``CE return creates Return``() =
        let d = durable { return 42 }
        match d with
        | Return 42 -> ()
        | _ -> Assert.Fail("Expected Return 42")

    [<Test>]
    member _.``CE let! binds result``() =
        let d = durable {
            let! x = Return 10
            return x * 2
        }
        match d with
        | Return 20 -> ()
        | _ -> Assert.Fail("Expected Return 20")

    [<Test>]
    member _.``CE yield! inserts durable``() =
        let inner : Durable<unit, string, unit> = durable {
            yield "A"
            yield "B"
        }
        let d = durable {
            yield! inner
            yield "C"
        }
        let effects = toEffect d |> Seq.toList
        Assert.That(effects, Is.EqualTo<string list>(["A"; "B"; "C"]))

    [<Test>]
    member _.``CE for iterates and emits``() =
        let d : Durable<unit, int, unit> = durable {
            for i in 1..3 do
                yield i
        }
        let effects = toEffect d |> Seq.toList
        Assert.That(effects, Is.EqualTo<int list>([1; 2; 3]))

    [<Test>]
    member _.``CE if/else branches correctly``() =
        let makeD flag : Durable<unit, string, unit> = durable {
            if flag then
                yield "yes"
            else
                yield "no"
        }
        Assert.That(makeD true |> toEffect |> Seq.toList, Is.EqualTo<string list>(["yes"]))
        Assert.That(makeD false |> toEffect |> Seq.toList, Is.EqualTo<string list>(["no"]))

// ===== Property-Based Tests =====

module DurablePropertyTests =
    open FsCheck
    open FsCheck.NUnit
    open Durable.Tests.Durable

    // Helper to compare durables structurally (for Return and Yield cases)
    let rec durableEquals (d1: Durable<'e, 's, 'r>) (d2: Durable<'e, 's, 'r>) : bool =
        match d1, d2 with
        | Return r1, Return r2 -> r1 = r2
        | Yield (s1, n1), Yield (s2, n2) -> s1 = s2 && durableEquals n1 n2
        | Suspend _, Suspend _ -> false  // Can't compare functions
        | _ -> false

    // ===== Functor Laws =====

    [<Property>]
    let ``Functor identity: map id = id`` (x: int) =
        let d = Return x
        durableEquals (map id d) d

    [<Property>]
    let ``Functor identity through Yield: map id = id`` (x: int) (s: string) =
        let d = Yield (s, Return x)
        durableEquals (map id d) d

    [<Property>]
    let ``Functor composition: map (f >> g) = map f >> map g`` (x: int) =
        let f = (+) 1
        let g = (*) 2
        let d = Return x
        durableEquals (map (f >> g) d) ((map f >> map g) d)

    [<Property>]
    let ``Functor composition through Yield`` (x: int) (s: string) =
        let f = (+) 1
        let g = (*) 2
        let d = Yield (s, Return x)
        durableEquals (map (f >> g) d) ((map f >> map g) d)

    // ===== Monad Laws =====

    [<Property>]
    let ``Monad left identity: Return x |> bind f = f x`` (x: int) =
        let f n = Return (n * 2)
        durableEquals (Return x |> bind f) (f x)

    [<Property>]
    let ``Monad right identity: m |> bind Return = m`` (x: int) =
        let d = Return x
        durableEquals (d |> bind Return) d

    [<Property>]
    let ``Monad right identity through Yield`` (x: int) (s: string) =
        let d = Yield (s, Return x)
        durableEquals (d |> bind Return) d

    [<Property>]
    let ``Monad associativity: (m |> bind f) |> bind g = m |> bind (fun x -> f x |> bind g)`` (x: int) =
        let f n = Return (n + 1)
        let g n = Return (n * 2)
        let d = Return x
        let left = (d |> bind f) |> bind g
        let right = d |> bind (fun n -> f n |> bind g)
        durableEquals left right

    // ===== mapEffect Laws =====

    [<Property>]
    let ``mapEffect identity: mapEffect id = id`` (x: int) (s: int) =
        let d = Yield (s, Return x)
        durableEquals (mapEffect id d) d

    [<Property>]
    let ``mapEffect composition: mapEffect (f >> g) = mapEffect f >> mapEffect g`` (x: int) (s: int) =
        let f = (+) 1
        let g = (*) 2
        let d = Yield (s, Return x)
        durableEquals (mapEffect (f >> g) d) ((mapEffect f >> mapEffect g) d)

    // ===== toEffect Properties =====

    [<Property>]
    let ``toEffect collects all emitted effects in order`` (effects: int list) =
        let d = 
            effects 
            |> List.rev 
            |> List.fold (fun acc s -> Yield (s, acc)) (Return ())
        (toEffect d |> Seq.toList) = effects

    [<Property>]
    let ``toEffect length equals number of Emits`` (effects: int list) =
        let d = 
            effects 
            |> List.rev 
            |> List.fold (fun acc s -> Yield (s, acc)) (Return ())
        (toEffect d |> Seq.length) = effects.Length

    // ===== foldEffect Properties =====

    [<Property>]
    let ``foldEffect with (+) accumulates sum`` (values: NonEmptyArray<int>) =
        let valuesList = values.Get |> Array.toList
        let d = 
            valuesList 
            |> List.rev 
            |> List.fold (fun acc s -> Yield (s, acc)) (Return ())
        let folded = foldEffect 0 (+) d
        let lastState = toEffect folded |> Seq.last
        lastState = List.sum valuesList

    // ===== apply Properties =====

    [<Property>]
    let ``apply with two Returns creates tuple`` (x: int) (y: string) =
        match apply (Return x) (Return y) with
        | Return (a, b) -> a = x && b = y
        | _ -> false

    // ===== zip Properties =====

    [<Property>]
    let ``zip with empty list preserves structure`` (x: int) (s: string) =
        let d = Yield (s, Return x)
        durableEquals (zip [] d) d
