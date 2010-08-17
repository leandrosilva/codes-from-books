#light
// --------------------------------------------------------------------------
// Functional Programming in .NET - Chapter 14
// --------------------------------------------------------------------------

// Reference functions from the 'Parallel.fs' module
open Functional

// --------------------------------------------------------------------------
// Section 14.3.1 Accessing shared objects safely

open System

// Random is not thread-safe, so this is a safe generator        
module SafeRandom = 
  let private rnd = new Random()
  let New() =
    // Thread-safe way to generate a new random generator
    // that should be used only by the calling thread
    lock rnd (fun () -> new Random(rnd.Next()))


// --------------------------------------------------------------------------
// Section 14.3.2 Representing the simulated world

// Value type representing location
[<Struct>]
type Vector(x:float, y:float) =
  member t.X = x
  member t.Y = y
  // Add X and Y coordinates
  static member (+) (l1:Vector, l2:Vector) = 
    Vector(l1.X + l2.X, l1.Y + l2.Y)
  // Multiply by coefficient of type float
  static member (*) (l:Vector, f) = 
    Vector(l.X * f, l.Y * f)
  // Implemented using + and *
  static member (-) (l1:Vector, l2:Vector) = l1 + (l2 * -1.0)

// Represents the simulation state    
type Simulation =
  { Animals : list<Vector>
    Predators : list<Vector> }

// --------------------------------------------------------------------------
// Implementing GUI (This code isn't discussed in the book)

// Basic GUI and drawing of the state
open System.Drawing
open System.Windows.Forms

let form = new Form(TopMost = true, Visible = true, ClientSize=Size(800,600))

// Draw the state to the form
let drawState(state) =
  if (not form.Visible) then false else
    // Fill the background with 'khaki' color
    let bmp = new Bitmap(form.ClientSize.Width, form.ClientSize.Height)
    let gr = Graphics.FromImage(bmp)
    gr.FillRectangle(Brushes.Khaki, form.ClientRectangle)

    // Draw all predators and animals
    for pred in state.Predators do
      gr.FillEllipse(Brushes.OrangeRed, Rectangle(int pred.X,int pred.Y,20,20))
    for anim in state.Animals do
      gr.FillEllipse(Brushes.Green, Rectangle(int anim.X,int anim.Y,10,10))

    // Draw the 'cached' image to the form
    let gr = form.CreateGraphics()
    gr.DrawImage(bmp, Point(0, 0))
    true
  
// --------------------------------------------------------------------------
// Section 14.3.4 Implementing smart animals and predators
  
// Returns the distance between two specified locations
let distance (l1:Vector) (l2:Vector) = 
    Math.Sqrt(Math.Pow(l1.X - l2.X, 2.0) + Math.Pow(l1.Y - l2.Y, 2.0))

// Generates a sequence with floats in range  f...t with step 'b'
let rec frange(f, b, t) =  
  seq { yield f
        if f + b < t then yield! frange(f+b, b, t) }

// Returns 10 check-points on the path between the specified locations
let getPathPoints(count, pfrom:Vector, pto:Vector) =
    seq { for q in frange(0.0, 1.0 / (float count), 1.0) do
            let pos = (pfrom + (pto - pfrom) * q)
            yield pos }

// Returns the specified number of randomly generated locations
let randomLocations(count)=
  let rnd = SafeRandom.New()
  seq { for i in 1..count 
          ->  Vector(rnd.NextDouble()*800.0, rnd.NextDouble()*600.0) } 

// --------------------------------------------------------------------------
// Moving animals and predators

// Listing 14.25 Implementing the animal behavior
let moveAnimal (state:Simulation) (animPos:Vector) =
  // Get the distance between 'pos' and the nearest predator
  let nearestPredatorDistanceFrom(pos) =
    state.Predators |> List.map (distance pos) |> Seq.min
 
  // Check safety of the path to the 'target'
  let nearestPredatorDistanceOnPath(target) =
    getPathPoints(10, animPos, target) |> Seq.map nearestPredatorDistanceFrom |> Seq.min
   
  // Choose the best of the generated locations
  let target = randomLocations(10) |> Seq.maxBy nearestPredatorDistanceOnPath
  // Move the animal by 20 points in that direction
  animPos + (target - animPos) 
    * (20.0 / (distance target animPos))

                
let movePredator (state:Simulation) (predPos:Vector) =
  // Calculate the number of locations (predators or animals) close to the given position
  let locationsClose an pos =
    an |> List.filter (fun an -> 50.0 > distance an pos) |> List.length
    
  // Return the number of locations close to the whole path to 'target'
  let locationsOnPath(an, target) =
    getPathPoints(10, predPos, target) |> Seq.map (locationsClose an) |> Seq.sum
  
  // Choose the best of the generated locations  
  let target = randomLocations(20) |> Seq.maxBy (fun pos ->
    // Prefer path with more animals and less predators
    locationsOnPath(state.Animals, pos) - 3 * locationsOnPath(state.Predators, pos))
  // Move the predator by 10 points in that direction
  predPos + (target - predPos) * (10.0 / (distance target predPos))

// --------------------------------------------------------------------------
// Section 14.3.5 Running the simulation in parallel

// Listing 14.15 Measuring the time
open System.Diagnostics

let measureTime(f) = 
  let stop = new Stopwatch()
  stop.Start()
  let res = f()
  (res, stop.ElapsedMilliseconds)

// Utility function that prints the time and returns the second value from the tuple
let printTimeAndReturn name (value, time) = 
  printfn "- %d (%s)" time name 
  value

// Listing 14.27 Generating random state and running a simulation step
// Note: The code in the book doesn't contain time measurement 
open System.Threading.Tasks

let simulationStep(state) = 
  // Process animals as a task
  let futureAnimals = Task.Factory.StartNew(fun () ->
    measureTime(fun () ->
      state.Animals 
        |> PSeq.ofSeq 
        |> PSeq.map (moveAnimal state) 
        |> List.ofSeq) |> printTimeAndReturn "moving animals")
  // Process predators immediately
  let predators = 
    measureTime(fun () ->
      state.Predators 
        |> PSeq.ofSeq 
        // Get new predator locations using PLINQ
        |> PSeq.map (movePredator state) 
        |> List.ofSeq) |> printTimeAndReturn "moving predators"
  { Animals = futureAnimals.Result
    Predators = predators }

// Sequential version of the function that can be used for comparison
let simulationStepSequential(state) = 
  let animals =
    measureTime(fun () ->
      state.Animals |> List.map (moveAnimal state)) 
      |> printTimeAndReturn "moving animals"
  let predators = 
    measureTime(fun () ->
      state.Predators |> List.map (movePredator state))
      |> printTimeAndReturn "moving predators"
  { Animals = animals
    Predators = predators }
  
// Add members to the 'Simulation' type
type Simulation with
  // Runs a single simulation step
  member x.Step() = simulationStep(x)
  // Generates random initial state
  static member CreateInitialState() = 
    { Animals = randomLocations(150) |> List.ofSeq
      Predators = randomLocations(15) |> List.ofSeq }


// --------------------------------------------------------------------------
// Section 14.3.3 Designing simulation operations

// Listing 14.24 Running simulation using recursion and 'async'        
let rec runSimulation(state:Simulation) =
  let running = form.Invoke(new Func<bool>(fun () -> 
    // Redraw the form
    drawState(state))) :?> bool
  // Tail-recursive call with a new state
  if (running) then runSimulation(state.Step())

// Start the computation asynchronously
Async.Start(async { 
    runSimulation(Simulation.CreateInitialState()) 
  })

[<STAThread>]
do Application.Run(form)