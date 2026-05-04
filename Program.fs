// For more information see https://aka.ms/fsharp-console-apps

open System
open System.Threading



type ProgramState =
| Menu
| Running
| Terminated

type State = {
    ProgramState: ProgramState
    RedrawScreen: bool
    Tick: int
    Clock: int
    MonsterX: int
    MonsterY: int
    RockX: int
    RockY: int
    StartTick: int
    MenuIndex: int // se agreaga
}

let initialState = {
    ProgramState = Menu // se agrega estado incial menu
    RedrawScreen = true
    Tick = -1
    Clock = 0
    MonsterX = Console.BufferWidth/2
    MonsterY = Console.BufferHeight/2
    RockX = 5
    RockY = 0
    StartTick=0
    MenuIndex = 0 // se agrega
}



let displayMessage x y color (msg:string) =
    Console.SetCursorPosition(x,y)
    Console.ForegroundColor <- color
    msg |> Console.Write

let displayMessageRight y color (msg:string) =
    let x = Console.BufferWidth-msg.Length
    displayMessage x y color msg



//agregamos menú
let displayMenu state =
    let color0 = if state.MenuIndex = 0 then ConsoleColor.Yellow else ConsoleColor.White
    let color1 = if state.MenuIndex = 1 then ConsoleColor.Yellow else ConsoleColor.White
    let color2 = if state.MenuIndex = 2 then ConsoleColor.Yellow else ConsoleColor.White

    let prefix0 = if state.MenuIndex = 0 then "> " else "  "
    let prefix1 = if state.MenuIndex = 1 then "> " else "  "
    let prefix2 = if state.MenuIndex = 2 then "> " else "  "

    displayMessage 10 5 color0 (prefix0 + "New Game")
    displayMessage 10 6 color1 (prefix1 + "Load Game")
    displayMessage 10 7 color2 (prefix2 + "Exit")

    state



let updateTick state =
    {state with Tick = state.Tick+1}

let updateClock state =
    if state.Tick <> 0 && state.Tick % 40 = 0 then
        {state with Clock=state.Clock+1;RedrawScreen=true}
    else
        state

let displayClock state =
    displayMessageRight 0 ConsoleColor.Green $"{state.Clock}"
    state

let displayMonster state =
    displayMessage state.MonsterX state.MonsterY ConsoleColor.Red "👽"
    state

let displayRock state =
    displayMessage state.RockX state.RockY ConsoleColor.Red "🪨"
    state

let updateRock state =
    let t = float (state.Tick - state.StartTick)*0.025
    let y = 0.5*9.77*t**2.0
    let pixelY = min (Console.BufferHeight-1) (int (y*300.0/float Console.BufferHeight))
    if pixelY <> state.RockY then
        {state with RockY = pixelY;RedrawScreen=true}
    else
        state
let redrawScreen state =
    if state.RedrawScreen then
        Console.Clear() 
        // state
        // |> displayClock
        // |> displayMonster
        // |> displayRock
        (match state.ProgramState with // se agrego
        | Menu ->
            state |> displayMenu

        | Running ->
            state
            |> displayClock
            |> displayMonster
            |> displayRock

        | Terminated -> state ) // se agrego
        |> fun s -> {s with RedrawScreen=false}
    else
        state



// se crea teclado menu 

let updateMenuKeyboard key state =
    match key with
    | ConsoleKey.UpArrow ->
        {state with MenuIndex = max 0 (state.MenuIndex-1); RedrawScreen=true}

    | ConsoleKey.DownArrow ->
        {state with MenuIndex = min 2 (state.MenuIndex+1); RedrawScreen=true}

    | ConsoleKey.Enter ->
        match state.MenuIndex with
        | 0 -> { initialState with ProgramState = Running } // new game
        //| 1 -> state
        | 1 -> { initialState with ProgramState = Running } // Load Game
        | 2 -> {state with ProgramState = Terminated} // exit
        | _ -> state

    | _ -> state



let updateClockKeyboard key state =
    match key with 
    | ConsoleKey.Escape -> 
        {state with ProgramState = Terminated}
    | _ -> state

let updateMonsterKeyboard key state =
    match key with 
    | ConsoleKey.UpArrow -> {state with MonsterY = max 0 (state.MonsterY-1)}
    | ConsoleKey.DownArrow -> {state with MonsterY = min (Console.BufferHeight-1) (state.MonsterY+1)}
    | ConsoleKey.LeftArrow -> { state with MonsterX = max 0 (state.MonsterX-1)}
    | ConsoleKey.RightArrow -> {state with MonsterX = min (Console.BufferWidth-2) (state.MonsterX+1)}
    | _ -> state
    |> fun s ->
        if s <> state then 
            {s with RedrawScreen = true}
        else
            state

let updateRockKeyboard key state =
    match key with 
    | ConsoleKey.Enter -> {state with RockY = 0;StartTick=state.Tick; RedrawScreen= true}
    | _ -> state
let processKeyboard state =
    if Console.KeyAvailable then 
        let k = Console.ReadKey true
        // state 
        // |> updateClockKeyboard k.Key
        // |> updateMonsterKeyboard k.Key
        // |> updateRockKeyboard k.Key  // se reemplaza por : 
        match state.ProgramState with
        | Menu ->
            updateMenuKeyboard k.Key state

        | Running ->
            state 
            |> updateClockKeyboard k.Key
            |> updateMonsterKeyboard k.Key
            |> updateRockKeyboard k.Key

        | Terminated -> state
    else
        state
let rec mainLoop state =
    let newState =
        state
        |> updateTick
        |> updateClock
        |> processKeyboard
        |> updateRock
        |> redrawScreen
   // if newState.ProgramState = Running then
    if newState.ProgramState <> Terminated then 
        Thread.Sleep 25
        mainLoop newState

Console.Clear()
Console.CursorVisible <- false
let oldForeground = Console.ForegroundColor

initialState
|> mainLoop

Console.CursorVisible <- true
Console.ForegroundColor <- oldForeground
Console.Clear()

//
// Tarea, crear un menu interactivo estilo Nintendo
//
// * New Game
//   Load Game
//   Exit