// --------------------------------------------------------------------------
// Functional Programming in .NET - Chapter 15
// --------------------------------------------------------------------------
module Demo.Animations

// --------------------------------------------------------------------------
// Section 15.2 Creating animated values

open System
open System.Drawing
open System.Drawing.Imaging // fancy stuff 
open System.Windows.Forms

// Listing 15.3 Representing behaviors using functions (F#)

// Arguments for evaluating time-varying values
type BehaviorContext =
  { Time : float32 }

// Single case discriminated union
type Behavior<'a> = 
  // Function evaluates the actual value
  | BH of (BehaviorContext -> 'a)

// --------------------------------------------------------------------------
// Section 15.2.4 Creating simple behaviors in F#

// Listing 15.7 Primitive behavior functions and values

/// Creates a 'Behavior' from a function that takes time
/// as an argument and returns a value
let sample(a) = BH(a)

/// Creates a constant function returning 'n' at any time
let forever n = 
  sample(fun _ -> n)

/// Return the current time
let time = 
  sample(fun t -> t.Time)

/// Time varying value from -1 to 0 (sinusoid)
let wiggle = 
  sample(fun t -> sin (t.Time * float32 Math.PI))

/// Extension property for float32 type
type System.Single with 
  member x.forever = forever(x)

// --------------------------------------------------------------------------
// Section 15.3 Writing computations with behaviors

// Listing 15.8 Reading values of behaviors at the specified time

let readValue(BH animFunc, time) = // Extract function value using pattern matching
  animFunc { Time = time }         // Run the function


// Listing 15.9 Implementing 'map' for behaviors 
module Behavior = 
  
  // Create behavior that applies 'f' to a returned value
  let map f (BH(fv)) = sample(fun t -> f (fv t))

  do
    // Behavior representing a square of the current time
    let squared = time |> map (fun n -> n * n)
    // Get the value after 9 seconds
    readValue(squared, 9.0f) |> ignore

  // Listing 15.10 Lifting functions of multiple arguments
  let lift1 = map
  let lift2 f (BH(fv1)) (BH(fv2)) = 
    sample(fun t -> f (fv1(t)) (fv2(t)))
  let lift3 f (BH(fv1)) (BH(fv2)) (BH(fv3)) = 
    sample(fun t -> f (fv1(t)) (fv2(t)) (fv3(t)))

// --------------------------------------------------------------------------

// Listing 15.19 Extension operators for calculating with behaviors 

// Type augmentation adding operators
type Behavior<'a> with
  static member (+) (a:Behavior<float32>, b) = 
    // Lift the standard operator
    Behavior.lift2 (+) a b
  
  // Multiplication for behaviors of 32bit floats
  static member (*) (a:Behavior<float32>, b) = 
    Behavior.lift2 (*) a b

// Listing 15.20 Speeding up and delaying behaviors 

let wait delay (BH(a)) = 
  sample(fun t -> a { t with Time = t.Time + delay })
let faster q (BH(a)) = 
  sample(fun t -> a { t with Time = t.Time * q })

// -------------------------------------------------------------------------
// Section 15.4.1 Representing drawings

// Listing 15.12 Representing drawings 

// Interface representing a drawing in F#
type Drawing =
  abstract Draw : Graphics -> unit

// Function for creating concrete drawings
let drawing f =
  { new Drawing with 
      member x.Draw(gr) = f(gr) }
  
// Listing 15.13 Creating circle 
// In F# we use module with functions
module Drawings = 
  let circle brush size =
    // Create drawing using higher order function
    drawing(fun g ->   
      g.FillEllipse(brush, -size/2.0f, -size/2.0f, size, size))

  // Return a new translated drawing 
  let translate x y (img:Drawing) =
    drawing (fun g -> 
      // All drawing will be translated
      g.TranslateTransform(x, y)
      // Run the original drawing
      img.Draw(g)
      // Reset the transform
      g.TranslateTransform(-x, -y) )

  // Listing 15.15 Creating composed drawing 

  let compose (img1:Drawing) (img2:Drawing) = 
    // Return a new drawing
    drawing(fun g ->
      // Draw both of the drawings
      img1.Draw(g)
      img2.Draw(g) )

// -------------------------------------------------------------------------
// Listing 15.16 Implementing form for showing animations  

/// A form that displays the specified animation, optionally using shadows
/// Note: this is an improved version that also supports shadows 
type AnimationForm() as x =
  inherit Form()
  let emptyAnim = forever(drawing(fun _ -> ()))
  let mutable startTime = DateTime.UtcNow
  let mutable anim = emptyAnim
  let mutable shadows = true
  
  do 
    x.SetStyle(ControlStyles.AllPaintingInWmPaint ||| ControlStyles.OptimizedDoubleBuffer, true)
    let tmr = new Timers.Timer(Interval = 25.0)
    tmr.Elapsed.Add(fun _ -> x.Invalidate() )
    tmr.Start()
  
  /// Gets or sets the currently displayed animation
  member x.Animation 
    with get() = anim
    and set(newAnim) = 
      anim <- newAnim
      startTime <- DateTime.UtcNow

  /// Gets or sets whether shadows are enabled
  member x.Shadows with get() = shadows and set(v) = shadows <- v
  
  // Redraw the form
  override x.OnPaint(e) =
    e.Graphics.FillRectangle(Brushes.White, Rectangle(Point(0,0), x.ClientSize))
    
    let start = if shadows then 5 else 0
    for t = start downto 0 do
        // Calculate time that we want to draw
        let time = float32 (DateTime.UtcNow - startTime).TotalSeconds
        // Take earlier time when drawing shadows
        let time = time - (float32 t) / 4.0f
        
        // Create a bitmap & draw the animation
        let bmp = new Bitmap(x.ClientSize.Width, x.ClientSize.Height)
        let gr = Graphics.FromImage(bmp)
        gr.TranslateTransform(float32 x.ClientSize.Width/2.f, float32 x.ClientSize.Height/2.f)
        let drawing = readValue(anim, time) 
        drawing.Draw(gr)     
        
        // Use transformation to add alpha-blending of shadows
        let op = if t = 0 then 1.0f else 0.6f - (float32 t) / 10.0f
        let ar = 
          [| 
            [|1.0f; 0.0f; 0.0f; 0.0f; 0.0f |]
            [|0.0f; 1.0f; 0.0f; 0.0f; 0.0f |]
            [|0.0f; 0.0f; 1.0f; 0.0f; 0.0f |]
            [|0.0f; 0.0f; 0.0f; op; 0.0f |]
            [|0.0f; 0.0f; 0.0f; 0.0f; 1.0f |]
          |]
        let clrMatrix = new ColorMatrix(ar);
        let imgAttributes = new ImageAttributes();
        imgAttributes.SetColorMatrix(clrMatrix, ColorMatrixFlag.Default, ColorAdjustType.Bitmap);    
        gr.Dispose()
        
        // Draw the created bitmap with the current layer
        e.Graphics.DrawImage
          (bmp, Rectangle(Point(0,0), x.ClientSize), 0, 0, bmp.Width, 
           bmp.Height, GraphicsUnit.Pixel, imgAttributes)
    
// -------------------------------------------------------------------------

/// A form that draws behavior as a function of time
type BehaviorPlotForm(offset, secs, anims:seq<Behavior<float32> * string * Color>) as x =
  inherit Form()
  do 
    x.SetStyle
      (ControlStyles.AllPaintingInWmPaint ||| 
       ControlStyles.OptimizedDoubleBuffer, true)
  
  // Invalidate the form on resize
  override x.OnResize(e) = x.Invalidate()
  
  // Draw the provided behaviors on the form...
  override x.OnPaint(e) =
    e.Graphics.FillRectangle(Brushes.White, Rectangle(Point(0,0), x.ClientSize))
    
    // Calculate 100 locations of the behaviors
    // We'll draw lines between these points
    let locations = 
      seq { for anim, name, clr in anims ->
              [| for t in 0 .. 100 do 
                   let time = float32 t / 100.0f * secs
                   yield t, readValue(anim, time) |], (name, clr) }

    // Concatenate all locations
    let allLocs = Seq.collect fst locations
    // Calculate minimal & maximal value on the form                       
    let (_, min), (_, max) = Seq.minBy snd allLocs, Seq.maxBy snd allLocs

    // Draw the lines... for each behavior    
    for anim, (name, color) in locations do
      use pen = new Pen(color, 3.0f)
      use fnt = new Font("Calibri", 16.0f, FontStyle.Bold)
      let br = new SolidBrush(color)
      let points =
        [| for idx, value in anim do
             yield 
              PointF
                (float32(x.ClientSize.Width * idx / 100),
                 float32(x.ClientSize.Height - 10) - 
                  (float32 x.ClientSize.Height - 20.0f) * (value - min) / (max - min)) |]
      e.Graphics.DrawLines(pen, points)
      e.Graphics.DrawString(name, fnt, br, points.[offset])

// -------------------------------------------------------------------------
// Section 15.5.3 Adding animation primitives

// Listing 15.18 Creating animation primitives using lifting 

// All arguments are behaviors
let circle br size = 
  Behavior.lift2 Drawings.circle br size

// Custom operator for composing animations
let ( -- ) img1 img2 = 
  Behavior.lift2 Drawings.compose img1 img2

// Lifted version of the translate primitive for drawings  
let translate x y img = 
  Behavior.lift3 Drawings.translate x y img

