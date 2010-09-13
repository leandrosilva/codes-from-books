//===============================================================================
// Microsoft patterns & practices
// Parallel Programming Guide
//===============================================================================
// Copyright © Microsoft Corporation.  All rights reserved.
// This code released under the terms of the 
// Microsoft patterns & practices license (http://parallelpatterns.codeplex.com/license).
//===============================================================================

module Microsoft.Practices.ParallelGuideSamples.ImageBlender.Program

open System
open System.IO
open System.Drawing
open System.Windows.Forms
open System.Threading.Tasks
open System.Drawing.Drawing2D

open Microsoft.Practices.ParallelGuideSamples.Utilities
open Microsoft.Practices.ParallelGuideSamples.ImageBlender

// ------------------------------------------------------------------------------
// Main method of the sample
// ------------------------------------------------------------------------------

// Command line arguments are:
//
//  image source directory, default: current directory
//  first source image file, default: flowers.jpg 
//  second source image file, default: dog.jpg 
//  blended image destination directory, default: current directory  
//
// If any of the directories or files do not exist, the program exits without results
//
[<STAThread; EntryPoint>]
let main (args:string[]) =
    Console.WriteLine("Image Blender Sample\n")
    #if DEBUG
    Console.WriteLine("For most accurate timing results, use Release build.\n")
    #endif
        
    let sourceDir = if args.Length > 0 then args.[0] else Directory.GetCurrentDirectory()
    let file1     = if args.Length > 1 then args.[1] else "flowers.jpg" // don't rotate
    let file2     = if args.Length > 2 then args.[2] else "dog.jpg"     // don't set to gray
    let destDir   = if args.Length > 3 then args.[3] else Directory.GetCurrentDirectory()

    let path1 = Path.Combine(sourceDir, file1)
    let path2 = Path.Combine(sourceDir, file2)

    SampleUtilities.CheckDirectoryExists(sourceDir)
    SampleUtilities.CheckDirectoryExists(destDir)
    SampleUtilities.CheckFileExists(path1)
    SampleUtilities.CheckFileExists(path2)

    // Load source images
    let source1 = new Bitmap(path1)
    let source2 = new Bitmap(path2)
    use result = new Bitmap(source1.Width, source1.Height)
    let blender = Graphics.FromImage(result)

    blender.CompositingMode <- CompositingMode.SourceOver // NOT SourceCopy mode

    // --------------------------------------------------------------------------
    // Sequential
    // --------------------------------------------------------------------------

    // Prepare for result image
    let layer1 = new Bitmap(source1.Width, source1.Height) 
    let layer2 = new Bitmap(source2.Width, source2.Height) 
    
    SampleUtilities.TimedRun "       Sequential" (fun () ->
      Example.SeqentialImageProcessing source1 source2 layer1 layer2 blender ) |> ignore

    // --------------------------------------------------------------------------
    // Parallel tasks
    // --------------------------------------------------------------------------

    // restore layers to iniital condition; layer2 must be unrotated
    let layer1 = new Bitmap(source1.Width, source1.Height)
    let layer2 = new Bitmap(source2.Width, source2.Height)

    SampleUtilities.TimedRun "   Parallel tasks" (fun () ->
      Example.ParallelTaskImageProcessing source1 source2 layer1 layer2 blender ) |> ignore

    // --------------------------------------------------------------------------
    // Parallel invoke
    // --------------------------------------------------------------------------
                
    // restore layers to initial condition; layer2 must be unrotated
    let layer1 = new Bitmap(source1.Width, source1.Height)
    let layer2 = new Bitmap(source2.Width, source2.Height)

    SampleUtilities.TimedRun "  Parallel invoke" (fun () ->        
      Example.ParallelInvokeImageProcessing source1 source2 layer1 layer2 blender) |> ignore



    // Save blended image in file
    result.Save(Path.Combine(destDir, "blended.jpg"))
    
    // Show blended image on screen, pause until user closes window
    Console.WriteLine("Close image window to exit program.")
    use form = new Form(ClientSize = result.Size) 
    use pb = new PictureBox(SizeMode = PictureBoxSizeMode.AutoSize, Image = result)
    form.Controls.Add(pb)
    form.ShowDialog() |> ignore
    0