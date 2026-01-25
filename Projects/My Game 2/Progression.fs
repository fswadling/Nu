namespace MyGame2

open Durable
open Prime

module Progression =
    let main : Durable<Advent, Op, unit> = 
        durable
          { AddScenario (NoZone, Placeholder)

            do! (!) }

    let initialCue =
        { Name = "main"
          Ops = main |> Durable.toState |> FDeque.ofSeq
          SignalCondition = None }
