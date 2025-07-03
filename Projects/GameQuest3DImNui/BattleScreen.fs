namespace GameQuest3DImNui

open Nu
open System.Numerics

type TurnNumber = int
type RoundNumber = int

type BattleState = 
    { Battle: Battle
      TurnNumber : TurnNumber
      RoundNumber : RoundNumber
      IsAttacking : bool }

module BattleState =
    let teamMemberPosition1 = v3 -63.710f -24.512f -34.443f
    let teamMemberPosition2 = v3 -65.705f -24.230f -34.445f
    let teamMemberPosition3 = v3 -67.701f -23.994f -34.450f
    let teamMemberPosition4 = v3 -69.695f -23.838f -34.456f
    let monsterPosition1 = v3 -63.710f -24.030f -30.520f

    let getTeamMemberPosition teamMemberIndex =
        match teamMemberIndex with
        | 0 -> teamMemberPosition1
        | 1 -> teamMemberPosition2
        | 2 -> teamMemberPosition3
        | 3 -> teamMemberPosition4
        | _ -> failwith "Invalid team member index"

    let getEnemyPosition monsterIndex =
        match monsterIndex with
        | 0 -> monsterPosition1
        | _ -> failwith "Invalid monster index"

    let getTeamMemberSimulant teamMemberIndex =
        match teamMemberIndex with
        | 0 -> Simulants.TeamMember1
        | 1 -> Simulants.TeamMember2
        | 2 -> Simulants.TeamMember3
        | 3 -> Simulants.TeamMember4
        | _ -> failwith "Invalid team member index"

    let getMonsterSimulant monsterIndex =
        match monsterIndex with
        | 0 -> Simulants.Horns
        | _ -> failwith "Invalid monster index"

    let empty =
        { Battle = Battle.empty
          TurnNumber = 0
          RoundNumber = 0
          IsAttacking = false }

    let init battle =
        { Battle = battle
          TurnNumber = 0
          RoundNumber = 0
          IsAttacking = false }

    let getBelligerent (state: BattleState): Entity =
        if state.TurnNumber < state.Battle.Companions.Length
        then
            getTeamMemberSimulant state.TurnNumber
        else
            let enemyIndex = state.TurnNumber - state.Battle.Companions.Length
            getMonsterSimulant enemyIndex

    let nextTurn (state: BattleState): BattleState =
        let nextTurnNumber = state.TurnNumber + 1
        if nextTurnNumber >= state.Battle.Companions.Length + state.Battle.Enemies.Length
        then { state with TurnNumber = 0; RoundNumber = state.RoundNumber + 1 }
        else { state with TurnNumber = nextTurnNumber }

type BattleMessage =
    | NextTurn
    | DoAttack
    interface Message

type BattleCommand = 
    | UpdateCamera
    | ScheduleNextTurn
    | WinBattle
    | Nil
    interface Command

type BelligerentDispatcher () =
    inherit Entity3dDispatcher<unit, Message, Command> (true, false, false, fun _ -> ())

    static member Facets =
        [ typeof<RigidBodyFacet>
          typeof<AnimatedModelFacet> ]

    static member Properties =
        [ define Entity.BodyType KinematicCharacter
          define Entity.MaterialProperties { MaterialProperties.defaultProperties with IsToon = true }
          define Entity.BodyShape
            (CapsuleShape
                { Height = 1.0f
                  Radius = 0.35f
                  TransformOpt = Some (Affine.makeTranslation (v3 0.0f 0.85f 0.0f))
                  PropertiesOpt = None }) ]

[<AutoOpen>]
module BattleScreenExtensions =
    type Screen with
        member this.GetBattleState world : BattleState = this.GetModelGeneric<BattleState> world
        member this.SetBattleState (value : BattleState) world = this.SetModelGeneric<BattleState> value world
        member this.BattleState = lens (nameof Screen.BattleState) this this.GetBattleState this.SetBattleState

type BattleScreenDispatcher () =
    inherit ScreenDispatcher<BattleState, BattleMessage, BattleCommand> (BattleState.empty)

    override this.Definitions (model, _) =
        [ Game.KeyboardKeyChangeEvent =|>
            (fun x -> 
                let isSpaceKey = x.Data.KeyboardKey = KeyboardKey.Space
                let notAttacking = not model.IsAttacking
                if  x.Data.Down && isSpaceKey && notAttacking
                then DoAttack 
                else Nil)
          Screen.UpdateEvent => UpdateCamera ]

    override this.Message (model, message, _, _) = 
        match message with
        | NextTurn ->
            let nextTurnState = BattleState.nextTurn model
            if nextTurnState.RoundNumber = 3
            then withSignal WinBattle { nextTurnState with IsAttacking = false }
            else just { nextTurnState with IsAttacking = false }

        | DoAttack ->
            withSignal ScheduleNextTurn { model with IsAttacking = true }

    override this.Command (model, command, screen, world) =
        match command with
        | Nil -> ()
        | UpdateCamera ->
            let simulant = BattleState.getBelligerent model
            let belligerentRotation = simulant.GetRotation world
            let belligerentRotation = 
                Quaternion.Concatenate
                    (belligerentRotation,
                    (Quaternion.CreateFromAxisAngle(v3Up, float32 Math.PI_MINUS_EPSILON)))
            let beligerentPosition = simulant.GetPosition world
            do World.setEye3dCenter
                   (beligerentPosition + v3Up * 1.75f + belligerentRotation.Left - belligerentRotation.Forward * 3.0f)
                   world
            do World.setEye3dRotation belligerentRotation world

        | ScheduleNextTurn ->
            do World.schedule 
                  (GameTime.ofSeconds 2f)
                  (World.signal NextTurn screen)
                  screen
                  world

        | WinBattle ->
            let gameState = Game.GetProgression world
            let nextState = Progression.doEvent gameState (Progression.BattleDone model.Battle.BattleTag)
            do Game.SetProgression nextState world

    override this.Content(model, _) =
        [ Content.group Simulants.BattleGroup.Name []
            [ Content.skyBox "skybox" []
              if (not model.IsAttacking) then
                 Content.text "pressSpace"
                    [ Entity.Text == "Press space to  attack"
                      Entity.Size == v3 200f 22f 0f ]
              Content.light3d "Sun"
                [ Entity.Position == v3 -48.0f -12.0f -16.5f
                  Entity.LightType == DirectionalLight
                  Entity.LightCutoff == 48.0f
                  Entity.DesireShadows == true ]
              Content.lightProbe3d "LightProbe"
                [ Entity.Position == v3 -12.0f 3.0f -18.0f
                  Entity.ProbeBounds == box3 (v3 -124.0f -48.0f -118.0f) (v3 160.0f 64.0f 168.0f)
                  Entity.AmbientBrightness == 0.1f ]
              for i, companion in Seq.indexed model.Battle.Companions do
                 let position = BattleState.getTeamMemberPosition i
                 let simulant = BattleState.getTeamMemberSimulant i
                 let current = BattleState.getBelligerent model
                 let isAttacking = model.IsAttacking && current.Name = simulant.Name
                 let animatedModel = Companion.animatedModel companion
                 let attackAnimation = Companion.attack companion
                 let idleAnimation = Companion.fightIdle companion
                 Content.entity<BelligerentDispatcher> simulant.Name
                   [ Entity.AnimatedModel == animatedModel
                     Entity.Animations := 
                        [| if isAttacking 
                           then attackAnimation
                           else idleAnimation |]
                     Entity.Position == position ]
              for i, monster in Seq.indexed model.Battle.Enemies do
                 let position = BattleState.getEnemyPosition i
                 let rotation = Quaternion.CreateFromAxisAngle(v3Up, float32 Math.PI_MINUS_EPSILON)
                 let simulant = BattleState.getMonsterSimulant i
                 let current = BattleState.getBelligerent model
                 let isAttacking = model.IsAttacking && current.Name = simulant.Name
                 let animatedModel = Enemy.animatedModel monster
                 let idleAnimation = Enemy.fightIdle monster
                 let attackAnimation = Enemy.attack monster
                 Content.entity<BelligerentDispatcher> simulant.Name
                   [ Entity.AnimatedModel == animatedModel
                     Entity.Animations :=
                        [| if isAttacking
                           then attackAnimation
                           else idleAnimation |]
                     Entity.Position == position
                     Entity.Rotation == rotation ]
              Content.rigidModelHierarchy "environment" [ Entity.StaticModel == Assets.Gameplay.SideZone3 ]]]
