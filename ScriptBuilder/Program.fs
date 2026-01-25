// For more information see https://aka.ms/fsharp-console-apps
open MyGame2
open Prime
open System.Numerics

let blinkScript : Script =
    { Name = "Main"
      SignalCondition = None
      Ops =
        FDeque.ofList
          [ FacialExpression ("Face", 13, 1.0f, 0.1f)

            // hold closed briefly
            Wait 0.05f
            
            // open eyelids
            FacialExpression ("Face", 13, 0.0f, 0.1f) ] }

let x = scstring blinkScript

x