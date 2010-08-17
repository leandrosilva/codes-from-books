// --------------------------------------------------------------------------
// Functional Programming in .NET - Chapter 15
// --------------------------------------------------------------------------

#load @"Animations.fs"

open Demo.Animations
open System.Drawing

// ----------------------------------------------------------------------------
// Section 15.4.2 Creating and composing drawings

// Open module with drawing functions
module Section_42 =
  open Drawings

  // Create a green and blue circle
  let greenCircle = circle Brushes.OliveDrab 100.0f
  let blueCircle = circle Brushes.SteelBlue 100.0f

  // Compose two translated circles
  let drawing = 
     compose (translate -35.0f 35.0f greenCircle)
             (translate 35.0f -35.0f blueCircle)

  // Create & display animation
  let animDrawing = forever drawing
  
  let af = new AnimationForm(ClientSize = Size(260, 260), Visible=true)
  af.Animation <- animDrawing
  

// ----------------------------------------------------------------------------
// Listing 15.17 Creating simple animation

do
  // Create the animation form
  let af = new AnimationForm(ClientSize = Size(600, 600), Visible=true)

  // Lift the 'translate' function to work with behaviors
  // All arguments are behaviors now
  let translate x y img = Behavior.lift3 Drawings.translate x y img

  // Multiply 'wiggle' to get a value in range -100 .. 100
  let wiggle100 = Behavior.lift2 (*) wiggle 100.0f.forever

  // Create and display the animation
  af.Animation <- translate wiggle100 0.0f.forever Section_42.animDrawing                
  
// --------------------------------------------------------------------------------
// Section 15.5.4 Creating a solar system animation

// Listing 15.21 Implementing rotation
let rotate (dist:float32) speed img = 
  // Oscillate between -dist and +dist
  let pos = wiggle * dist.forever
  // Delay the Y-coordinate animation by one half
  // .. & use the provided speed
  img |> translate pos (wait 0.5f pos) |> faster speed

// Listing 15.22 Creating solar system animation in F# and C#

let sun   = circle (forever Brushes.Goldenrod) 100.0f.forever
let earth = circle (forever Brushes.SteelBlue) 50.0f.forever
let mars  = circle (forever Brushes.Chocolate) 40.0f.forever
let moon  = circle (forever Brushes.DimGray)   10.0f.forever
 
let planets = 
   sun -- (earth -- (moon |> rotate 40.0f 12.0f) 
           |> rotate 160.0f 1.3f)
       -- (mars |> rotate 250.0f 0.7f)
   |> faster 0.2f
   
let af = new AnimationForm(ClientSize = Size(750, 750), Visible=true)
af.Animation <- planets
  
// --------------------------------------------------------------------------
// Additional: Code that draws some behaviors as functions

open System

let t = Behavior.lift2 (+) wiggle time;;
let squared = time |> Behavior.map (fun t -> t * t / 5.0f - 8.0f);;

let bf = 
  new BehaviorPlotForm
    (35, 10.0f, 
     [ t, "wiggle + time", Color.Indigo
       squared, "(time^2 / 5) - 8", Color.OliveDrab
       wiggle, "wiggle", Color.SteelBlue
     ], Text="Behaviors", ClientSize = Size(750, 450))
     
bf.Show()


let bf2 = 
  new BehaviorPlotForm
    (15, 2.0f, 
     [ 1.5f.forever, "constant", Color.Chocolate
       time, "current", Color.OliveDrab
       wiggle, "wiggle", Color.SteelBlue
     ], Text="Primitive behaviors", ClientSize = Size(750, 450))
bf2.Show()
