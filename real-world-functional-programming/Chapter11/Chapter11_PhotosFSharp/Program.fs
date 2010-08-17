#light
// --------------------------------------------------------------------------
// Functional Programming in .NET - Chapter 11
// --------------------------------------------------------------------------
// Sample application that demonstrates how to use lazy values for
// caching of resized photo previews.
//
// Note: In project properties, we have to select 'Windows Application'
// because by default F# projects are 'Console' based
// --------------------------------------------------------------------------

// --------------------------------------------------------------------------
// Section 11.4.2 Caching values in a photo browser

// Listing 11.20 Creating collection of photo information

open System.IO
open System.Drawing

// Stores photo name and lazily created preview
type ImageInfo = 
  { Name : string
    Preview : Lazy<Bitmap> }

// Specify directory with your photos
let dir = @"C:\Users\Public\Pictures\Sample Pictures"

let createLazyResized(file) = 
  lazy( let bmp = Bitmap.FromFile(file)
        let resized = new Bitmap(400, 300)
        // 'use' will dispose the object automatically
        use gr = Graphics.FromImage(resized)
        let dst = Rectangle(0, 0, 400, 300)
        let src = Rectangle(0, 0, bmp.Width, bmp.Height)
        gr.InterpolationMode <- Drawing2D.InterpolationMode.High
        gr.DrawImage(bmp, dst, src, GraphicsUnit.Pixel)
        // Draw resized bitmap to the target
        resized)

// Array of ImageInfo value for each photo
let files = Directory.GetFiles(dir, "*.jpg") |> Array.map (fun file ->
  // Return record with name and preview
  { Name = Path.GetFileName(file)
    Preview = createLazyResized(file) })

// --------------------------------------------------------------------------
// Listing 11.21 Adding user interface for the photo browser

open System
open System.Windows.Forms

let main = new Form(Text="Photos", ClientSize=Size(600,300))
let pict = new PictureBox(Dock=DockStyle.Fill)

// Configure to display 'Name' property from 'files' array
let list = new ListBox(Dock=DockStyle.Left, Width=200, 
                       DataSource=files, DisplayMember = "Name")

// Called when the selection changes
list.SelectedIndexChanged.Add(fun _ -> 
  // Evaluate the lazy value 
  pict.Image <- files.[list.SelectedIndex].Preview.Value) 

main.Controls.Add(pict)
main.Controls.Add(list)

// This displays the first image (this line isn't included in the book)
main.Load.Add(fun _ -> 
  if (files.Length > 0) then 
    pict.Image <- files.[0].Preview.Value )

[<STAThread>]
do
  // Runs the application
  Application.Run(main)
