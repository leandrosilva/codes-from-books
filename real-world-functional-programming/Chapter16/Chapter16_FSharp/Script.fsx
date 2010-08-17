// --------------------------------------------------------------------------
// Functional Programming in .NET - Chapter 16
// --------------------------------------------------------------------------

// --------------------------------------------------------------------------
// Section 16.1 Reactive programming using events

// Listing 16.1 Monitoring file system events
// Initialize the watcher
open System.IO
let w1 = new FileSystemWatcher("C:\\Temp", EnableRaisingEvents = true)

// Test attributes of the file
let isNotHidden(fse:FileSystemEventArgs) = 
  let hidden = FileAttributes.Hidden
  (File.GetAttributes(fse.FullPath) &&& hidden) <> hidden
    
// Register the event handler
w1.Renamed.Add(fun fse ->
  // Report only visible files
  if isNotHidden(fse) then
    printfn "%s renamed to %s" fse.OldFullPath fse.FullPath)

// --------------------------------------------------------------------------
// 16.1.1 Higher order event functions

// Listing 16.2 Filtering events using Observable.filter function 
let w2 = new FileSystemWatcher("C:\\Temp", EnableRaisingEvents = true)

// Filter renames of hidden files
let renamedVisible = 
  w2.Renamed |> Observable.filter isNotHidden
// Print file name when event occurs
renamedVisible |> Observable.add (fun fse -> 
    printfn "%s renamed to %s" fse.OldFullPath fse.FullPath)

// --------------------------------------------------------------------------

// Function for formatting the information about event
// (needed below in the listing 16.3)
let formatFileEvent(fse:RenamedEventArgs) = 
  sprintf "%s renamed to %s" fse.OldFullPath fse.FullPath

// Create a new file system watcher..
let w3 = new FileSystemWatcher("C:\\Temp", EnableRaisingEvents = true)

// Listing 16.3 Declarative event handling (F#)
w3.Renamed 
  |> Observable.filter isNotHidden
  |> Observable.map formatFileEvent
  |> Observable.add (printfn "%s")

// --------------------------------------------------------------------------
// Section 16.1.2 Creating simple reactive application

// Create the user interface of the application
// (this part of the code ins't discussed in the book)
open System
open System.Drawing
open System.Windows.Forms
         
let fnt = new Font("Calibri", 32.0f)
let form = new Form(ClientSize=Size(300, 80))

let btnUp = new Button(Location=Point(10,10), Size=Size(100, 25), Text="Increment")
let btnDown = new Button(Location=Point(10,40), Size=Size(100, 25), Text="Decrement")
let lbl = new Label(Location=Point(120, 10), Size=Size(200, 60), Text="Count: 0", Font=fnt)
form.Controls.Add(lbl)
form.Controls.Add(btnUp)
form.Controls.Add(btnDown)
form.Show()

// Create a function that ignores the parameter and always returns the same value
let always x = (fun _ -> x)

// --------------------------------------------------------------------------
// Listing 16.4 Pipeline for handling events

// Create events that carry +1 or -1 values
let incEvt = (btnUp.Click |> Observable.map (always 1))
let decEvt = (btnDown.Click |> Observable.map (always -1))

// Merge the events
Observable.merge incEvt decEvt
  // Calculate summary of carried values
  |> Observable.scan (+) 0 
  |> Observable.add (fun sum -> 
    // Display the result
    lbl.Text <- sprintf "Count: %d" sum)

Application.Run(form)

// --------------------------------------------------------------------------
// Section 16.1.4 Declaring events in F#

type Counter() = 
  let mutable num = 0
  // Create a new event
  let ev = new Event<_>()
  // Publish the 'IEvent' value
  member x.SignChanged = ev.Publish
  member x.Add(n) =
    let original = num
    num <- num + n
    if (sign(original) <> sign(num)) then
      // Trigger the event using provided member
      ev.Trigger(num) 

let c = Counter()
c.SignChanged |> Observable.add (printfn "Number: %d")
c.Add(10)    // Sign changed from 0 to 1
c.Add(10)
c.Add(-30)   // Sign changed to -1

// --------------------------------------------------------------------------
// Section 16.2 Creating reactive animations

#load @"..\Animations.fs"

// Functionality that were already discussed in the previous chapter
open System.Drawing
open Demo.Animations

// Rotate animation in a specified distance using the given speed
let rotate (dist:float32) speed img = 
  let pos = wiggle * dist.forever
  img |> translate pos (wait 0.5f pos) |> faster speed

// Create animation form
let af = new AnimationForm(ClientSize = Size(400, 400), Visible=true)

// --------------------------------------------------------------------------
// Section 16.2 Creating reactive animations
// Note: The listings here are in reversed order

// Listing 16.8 Implementing the switch function (F#)
let switch init evt =  
  // Store the actual behavior in a ref cell
  let current = ref init
  evt |> Observable.add (fun arg -> 
    // Update the behavior
    current := arg)
  sample(fun ctx -> 
    // Get the current behavior and run it
    let (BH(f)) = !current
    f(ctx) 
  )


// Listing 16.7 Animation with changing speed

// Create rotating circle with a constant size
let greenCircle = circle (forever Brushes.OliveDrab) 100.0f.forever
let rotatingCircle = rotate 100.0f 1.0f greenCircle

// Event carrying behaviors as a value
let circleEvt = 
  af.Click 
  // Adds 0.1 to the initial speed 0.0 
  |> Observable.map (always 0.1f) 
  |> Observable.scan (+) 0.0f 
  // Create a new faster animation
  |> Observable.map (fun x -> faster x rotatingCircle)
  
// Initial animation is suspended
let init = faster 0.0f rotatingCircle
// Animation that speeds-up with clicks
af.Animation <- switch init circleEvt


// --------------------------------------------------------------------------
// Section 16.2.1 Waiting for events asynchronously
#load @"..\AsyncExtensions.fs"
open Demo.AsyncExtensions

// Listing 16.9 Counting clicks using asynchronous workflows (F#)
module Listing_16_9 =
  
  // Construct the user interface
  let form = new Form(ClientSize=Size(200, 100), Visible=true)
  let fnt = new Font("Calibri", 24.0f)
  let lbl = new Label(Dock = DockStyle.Fill, TextAlign = ContentAlignment.MiddleCenter, Font = fnt)
  form.Controls.Add(lbl)

  // 'Infinite' asynchronous loop
  let rec loop(count) = async {       
    // Wait for the next click
    let! _ = Async.AwaitObservable(lbl.MouseDown)
    lbl.Text <- sprintf "Clicks: %d" count
    
    // OPTIONAL:  Wait 1 second to limit the clicking rate
    do! Async.Sleep(1000)
    
    // Loop with incremented count
    return! loop(count + 1) }
    
  Async.StartImmediate(loop(1))
  form.Show()
  
  // NOTE: The following example is not discussed in the book!
  // Just for curiosity, this shows how the thing above
  // could be implemented using F# event combinators
  lbl.MouseDown 
    |> Observable.map (fun _ -> DateTime.Now)          // Create events carrying the current time
    |> Observable.scan (fun (_, dt:DateTime) ndt ->    // Remembers the last time click was accepted
        if ((ndt - dt).TotalSeconds > 1.0) then        // When the time is more than a second...
          (1, ndt) else (0, dt)) (0, DateTime.Now)     // .. we return 1 and the new current time
    |> Observable.map fst |> Observable.scan (+) 0     // Sum the yielded numbers 
    |> Observable.map (sprintf "Clicks: %d")           // Format the output as a string 
    |> Observable.add lbl.set_Text                     // Display the result...
  


// --------------------------------------------------------------------------
// Section 16.5 Message passing concurrency
open System

type Message = 
  | ModifyState of int
  | Block
  | Resume

// Listing 16.19 Mailbox processor using state machine

let mbox = MailboxProcessor.Start(fun mbox ->
    // Represents the blocked state
    let startTime = DateTime.Now
    let rec blocked(n) = 
      printfn "[%A] Blocking" (DateTime.Now - startTime)
      // Only process the 'Resume' message
      mbox.Scan(fun msg ->
        match msg with
        // Return workflow to continue with
        | Resume -> Some(async {
            printfn " [%A] Resuming" (DateTime.Now - startTime)
            return! processing(n) })
        // Other messages cannot be processed now
        | _ -> None)
        
    // Represents the active  state
    and processing(n) = async {
      printfn "[%A] Processing: %d" (DateTime.Now - startTime) n
      // Process any message
      let! msg = mbox.Receive()
      match msg with
      | ModifyState(by) -> return! processing(n + by)
      | Resume -> return! processing(n)
      | Block -> return! blocked(n) }
    processing(0)
  )
  
// Listing 16.20 Sending messages from multiple threads 

open System
open System.Threading
  
// Thread performing calculations
let modifyThread() =
  let rnd = new Random(Thread.CurrentThread.ManagedThreadId)  
  while true do
    Thread.Sleep(500)
    // Send an update to the mailbox
    mbox.Post(ModifyState(rnd.Next(11) - 5)) 

let blockThread() =
  while true do
    Thread.Sleep(2000)
    mbox.Post(Block)    
    // Block the processing for one and half seconds
    Thread.Sleep(1500)
    mbox.Post(Resume) 

for proc in [ blockThread; modifyThread; modifyThread ] do
  Async.Start(async { proc() })