//===============================================================================
// Microsoft patterns & practices
// Parallel Programming Guide
//===============================================================================
// Copyright © Microsoft Corporation.  All rights reserved.
// This code released under the terms of the 
// Microsoft patterns & practices license (http://parallelpatterns.codeplex.com/license).
//===============================================================================

#r @"..\..\Utilities\bin\Release\Utilities.dll"

#load "BitmapExtensions.fs"
#load "Processing.fs"

open System
open System.IO
open System.Drawing
open System.Windows.Forms
open System.Threading.Tasks
open System.Drawing.Drawing2D
open Microsoft.Practices.ParallelGuideSamples.Utilities
open Microsoft.Practices.ParallelGuideSamples.ImageBlender

// --------------------------------------------------------------------------
// Load images & initialize
// --------------------------------------------------------------------------

// A full path to the directory with images (the one below should exist on Windows 7)
let sourceDir = @"C:\Users\Public\Pictures\Sample Pictures"
let file1 = "chrysanthemum.jpg"             // don't rotate
let file2 = "koala.jpg"     // don't set to gray

// Combine directories with image names
let path1 = Path.Combine(sourceDir, file1)
let path2 = Path.Combine(sourceDir, file2)

// Verify that files & directory exist
SampleUtilities.CheckDirectoryExists(sourceDir)
SampleUtilities.CheckFileExists(path1)
SampleUtilities.CheckFileExists(path2)

// Load source images
let source1 = new Bitmap(path1)
let source2 = new Bitmap(path2)
let result = new Bitmap(source1.Width, source1.Height)
let blender = Graphics.FromImage(result)

blender.CompositingMode <- CompositingMode.SourceOver // NOT SourceCopy mode

// --------------------------------------------------------------------------
// Run processing of images
// --------------------------------------------------------------------------

// Prepare for result image
let layer1 = new Bitmap(source1.Width, source1.Height) 
let layer2 = new Bitmap(source2.Width, source2.Height) 
    
#time "on"

// *** TODO ***:
// Select one version of the processing to run
// (Note: You need to recreate 'layer1' and 'layer2' after running the processing)

// Sequential
Example.SeqentialImageProcessing source1 source2 layer1 layer2 blender 

// Parallel (using tasks)
Example.ParallelTaskImageProcessing source1 source2 layer1 layer2 blender

// Parallel (usint Parallel.Invoke)
Example.ParallelInvokeImageProcessing source1 source2 layer1 layer2 blender

#time "off"

// Show a window with the blended image 
let form = new Form(ClientSize = result.Size) 
let pb = new PictureBox(SizeMode = PictureBoxSizeMode.AutoSize, Image = result)
form.Controls.Add(pb)
form.Show()
