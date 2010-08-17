open Functional

// --------------------------------------------------------------------------
// Functional Programming in .NET - Chapter 14
// --------------------------------------------------------------------------

// --------------------------------------------------------------------------
// Section 14.2.1 Calculating with colors in F#

// Listing 14.9 Implementing color type with operators

// Compile the type as value type
[<Struct>]
type SimpleColor(r:int, g:int, b:int) = 
  member x.R = r
  member x.G = g
  member x.B = b
  // Create color with components in range 0-255
  member x.ClipColor() = 
    let check c = min 255 (max 0 c)
    SimpleColor(check r, check g, check b)
  // Component-wise addition of two colors
  static member (+) (c1:SimpleColor, c2:SimpleColor) = 
    SimpleColor(c1.R + c2.R, c1.G + c2.G, c1.B + c2.B)
  // Divide color components by an integer
  static member DivideByInt (c1:SimpleColor, n) = 
    SimpleColor(c1.R / n, c1.G / n, c1.B / n)
  // Multiply color components by an integer
  static member (*) (c1:SimpleColor, n) = 
    SimpleColor(c1.R * n, c1.G * n, c1.B * n)
  // Color initialized with zeros
  static member Zero = 
    SimpleColor(0, 0, 0)

// --------------------------------------------------------------------------
// Section 14.2.2 Implementing and running color filters  

// Note: the following two functions implement conversion between 
// bitmap and array represenation and aren't discussed in the book.
// The implementation uses 'LockBits' method and you can find
// additional documentation online in the .NET Reference
module BitmapUtils = 
  open System
  open System.Drawing
  
  // Convert bitmap to an array
  let ToArray2D(bmp:Bitmap) : SimpleColor[,] =
      let rect = Rectangle(0, 0, bmp.Width, bmp.Height) 
      // Lock bitmap in the memory, so that we can access it directly
      let bmpData = bmp.LockBits(rect, Imaging.ImageLockMode.ReadWrite, Imaging.PixelFormat.Format32bppArgb)
      // Using pointer arithmethic to copy the bits
      let ptr0 = bmpData.Scan0 : IntPtr
      let stride = bmpData.Stride
      let res = Array2D.init bmp.Width bmp.Height (fun i j ->
        let offset = i*4 + stride*j
        let clr = Color.FromArgb(System.Runtime.InteropServices.Marshal.ReadInt32(ptr0,offset))
        SimpleColor(int clr.R, int clr.G, int clr.B))
      bmp.UnlockBits(bmpData)
      res 
      
  // Convert array to .NET Bitmap type
  let ToBitmap(arr:SimpleColor[,]) =
      let bmp = new Bitmap(arr.GetLength(0), arr.GetLength(1))
      let rect = new Rectangle(0, 0, bmp.Width, bmp.Height)
      // Lock bitmap in the memory, so that we can access it directly
      let bmpData = bmp.LockBits(rect, Imaging.ImageLockMode.ReadWrite, Imaging.PixelFormat.Format32bppArgb)
      // Using pointer arithmethic to copy the bits
      let ptr0 = bmpData.Scan0 : IntPtr
      let stride = bmpData.Stride
      for i = 0 to bmp.Width - 1 do
        for j = 0 to bmp.Height - 1 do
          let offset = i*4 + stride*j
          let clr = arr.[i,j]
          let clr = Color.FromArgb(clr.R, clr.G, clr.B).ToArgb()
          System.Runtime.InteropServices.Marshal.WriteInt32(ptr0, offset, clr)
      bmp.UnlockBits(bmpData)
      bmp

// --------------------------------------------------------------------------

// Listing 14.11 Applying filters and two simple filters (F#)

// Apply 'f' to all elements of the 2D array
let runFilter f arr = Array2D.map f arr

// Filters are encapsulated in a module
module ColorFilters =
  // Grayscale using weighted average
  let Grayscale(clr:SimpleColor) =
    let c = (clr.R + clr.G + clr.B) / 3
    SimpleColor(c, c, c)

  // Calculate lighter color
  let Lighten(clr:SimpleColor) =
    (clr*2).ClipColor()


// --------------------------------------------------------------------------
// Section 14.2.5 Parallelizing the application
// Note: The sections 14.2.5 and 14.2.4 have to be reordered to make the code compile

// Listing 14.18 Parallel 'map' function for 2D array
module Array2D =
  module Parallel =
    // Declare function in a module
    // Apply 'f' to all elements in parallel
    let map f (arr:_ [,]) =
      let wid, hgt = arr.GetLength(0), arr.GetLength(1)
      // Create new array as a result
      let res = Array2D.zeroCreate wid hgt
      // Parallelized outer loop
      pfor 0 (wid-1) (fun x ->
        for y = 0 to hgt - 1 do
          res.[x, y] <- f(arr.[x, y]) )
      res
    
// Expose function with a domain-specific alias
let runFilterParallel f arr  = Array2D.Parallel.map f arr

// --------------------------------------------------------------------------
// Section 14.2.4 Creating and running effects

// Listing 14.14 Creating effects using partial function application

// Create effect using partial application
let effect = runFilter ColorFilters.Grayscale


// Listing 14.15 Measuring the time
open System.Diagnostics

let measureTime(f) = 
  let stop = Stopwatch.StartNew()
  let res = f()
  (res, stop.ElapsedMilliseconds)

// --------------------------------------------------------------------------
// Section 14.2.6 Taking the example further

// Listing 14.19 Implementing 'Blur' effect (F#)
module ImageEffects = 
  let blur(arr:SimpleColor[,], x, y) =
    // Check that index is in range
    let hgt, wid = arr.GetLength(0) - 1, arr.GetLength(1) - 1
    let checkW x = max 0 (min wid x)
    let checkH y = max 0 (min hgt y)
    // Collect all close pixel colors
    seq { for dy in -2 .. 2 do
            for dx in -2 .. 2 do
              yield arr.[checkH(y + dy), checkW(x + dx)] }
    |> Seq.average // Calculate average color

  let runEffect isParallel effect (arr:SimpleColor[,]) =
    // Decide whether to use parallel or standard for
    let forLoop = 
      if isParallel then pfor 
      else (fun x y f -> 
        for i = x to y do f(i))

    // Create the resulting array
    let hgt, wid = arr.GetLength(0) - 1, arr.GetLength(1) - 1
    let res = Array2D.create (hgt + 1) (wid + 1) SimpleColor.Zero
    
    forLoop 0 hgt (fun y ->
      for x = 0 to wid do
        res.[y, x] <- effect(arr, x, y)
    )
    res

  let SequentialBlur(arr) = runEffect false blur arr
  let ParallelBlur(arr) = runEffect true blur arr

// --------------------------------------------------------------------------
// The rest of the code implements the GUI of the application and 
// it isn't discussed in detail in the book text.

// The GUI for the application is created using C# WinForms designer
// and you can find it in the 'FSharpEffects.Forms' project.
// This F# project references the GUI (which is just a library
// with the compiled form)

open System.Drawing
open System.Windows.Forms
open Chapter14_FSharpEffects.Forms

type EffectInfo = { Name : string; Effect : SimpleColor[,] -> SimpleColor[,] }

[<System.STAThread>]
do
  let form = new MainForm()
  let loadedBitmap = ref None
  
  // Initialize the collection of effects (created from filters)
  let effects =
    [| { Name = "Grayscale (sequential)"; Effect = runFilter ColorFilters.Grayscale }
       { Name = "Grayscale (parallel)"; Effect = runFilterParallel ColorFilters.Grayscale }
       { Name = "Lighten (sequential)"; Effect = runFilter ColorFilters.Lighten }
       { Name = "Lighten (parallel)"; Effect = runFilterParallel ColorFilters.Lighten }
       { Name = "Blur (sequential)"; Effect = ImageEffects.SequentialBlur }
       { Name = "Blur (parallel)"; Effect = ImageEffects.ParallelBlur } |]

  // Setup the databinding to display the list of effects
  form.listFilters.ComboBox.DataSource <- effects
  form.listFilters.ComboBox.DisplayMember <- "Name"
  form.listFilters.SelectedIndex <- 0

  // Handle click on the run button
  let handleClick() =
    match !loadedBitmap with
    | Some(bmp) ->
        // Get the selected filter
        let filter = (form.listFilters.SelectedItem :?> EffectInfo).Effect;
        let arr = bmp |> BitmapUtils.ToArray2D
        
        // Run filter and measure performance
        let res, time = measureTime (fun () -> filter(arr))

        // Display result and time taken
        form.pictProcessed.Image <- res |> BitmapUtils.ToBitmap
        form.lblTime.Text <- sprintf "Time: %d ms" time
    | _ -> ()
  
  // Loading of a bitmap
  form.btnOpen.Click.Add(fun _ ->
    if (form.openImageDlg.ShowDialog() = DialogResult.OK) then
      let b = new Bitmap(form.openImageDlg.FileName)
      form.pictOriginal.Image <- b
      loadedBitmap := Some(b) ) 
  
  // Use the current output as an input
  form.btnUseProcessed.Click.Add(fun _ ->
    if (form.pictProcessed.Image <> null) then
      form.pictOriginal.Image <- form.pictProcessed.Image
      loadedBitmap := Some(form.pictProcessed.Image :?> Bitmap) )
      
  form.btnApply.Click.Add(fun _ -> handleClick())
  
  Application.EnableVisualStyles()
  Application.Run(form)