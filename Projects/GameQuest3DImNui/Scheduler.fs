namespace GameQuest3DImNui
open Nu

module Scheduler =

    type Schedule<'T> = 'T -> World -> World
    type Scheduler<'T1, 'T2> = Schedule<'T1> -> Schedule<'T2>

    let join (fn1 : Scheduler<'T1, 'T2>) (fn2 : Scheduler<'T2, 'T3>) =
        fun (fn: Schedule<'T1>) -> fn2 (fn1 fn)

    let map (f: 'T1 -> 'T2) (scheduler: Scheduler<'T1,_>): Scheduler<'T2,_> =
        scheduler
        |> join (fun fn i -> fn (f i))

    let bind (f: 'T1 -> Scheduler<'T2,_>) (scheduler: Scheduler<'T1,_>): Scheduler<'T2,_> =
        scheduler
        |> join (fun fn i -> (f i) fn i)

    type SchedulerBuilder () =
        member _.Bind(m, f) =
            bind f m

        member _.Return(m) =
            fun fn _ -> fn m

    let scheduler = new SchedulerBuilder()
