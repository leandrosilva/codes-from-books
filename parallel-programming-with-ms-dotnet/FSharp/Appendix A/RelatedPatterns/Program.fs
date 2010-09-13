//===============================================================================
// Microsoft patterns & practices
// Parallel Programming Guide
//===============================================================================
// Copyright © Microsoft Corporation.  All rights reserved.
// This code released under the terms of the 
// Microsoft patterns & practices license (http://parallelpatterns.codeplex.com/license).
//===============================================================================

//===============================================================================
//
// NOTE: Only some of the patterns discussed in Appendix B have samples 
//       associated with them.
//
//===============================================================================

module Microsoft.Practices.ParallelGuideSamples.RelatedPatterns.Main

open System
open System.Collections.Generic
open System.Drawing
open System.IO
open Microsoft.Practices.ParallelGuideSamples.Utilities

let maxImages = 500

/// Load the specified number of images (from the current app directory)
let loadImages maxImages =
    let seq = SampleUtilities.GetImageFilenames (Directory.GetCurrentDirectory()) maxImages
    let res = new ResizeArray<_>()
    let en = seq.GetEnumerator()
    while res.Count < maxImages do
        en.MoveNext() |> ignore
        res.Add(new Bitmap(en.Current))
    res

/// Run the decorator example (sequential & parallel version)
let decoratorExample() =
    Console.WriteLine("Loading images...")
    let images = loadImages maxImages

    let serialEd = new SerialEditor() :> IImageEditor
    SampleUtilities.TimedAction "Rotate, sequential" (fun () -> 
        serialEd.Rotate RotateFlipType.RotateNoneFlipX images ) |> ignore

    let parallelEd = new ParallelEditor(new SerialEditor()) :> IImageEditor
    SampleUtilities.TimedAction "Rotate, parallel" (fun () -> 
        parallelEd.Rotate RotateFlipType.RotateNoneFlipX images )


/// Entry point - run the decorator example
do
    Console.WriteLine("Supporting Patterns Samples\n")
#if DEBUG
    Console.WriteLine("For most accurate timing results, use Release build.\n")
#endif
    decoratorExample()
    Console.WriteLine("\nRun complete... press enter to finish.")
    Console.ReadLine() |> ignore
