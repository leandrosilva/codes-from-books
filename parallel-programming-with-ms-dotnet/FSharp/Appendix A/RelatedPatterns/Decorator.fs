//===============================================================================
// Microsoft patterns & practices
// Parallel Programming Guide
//===============================================================================
// Copyright © Microsoft Corporation.  All rights reserved.
// This code released under the terms of the 
// Microsoft patterns & practices license (http://parallelpatterns.codeplex.com/license).
//===============================================================================

namespace Microsoft.Practices.ParallelGuideSamples.RelatedPatterns

open System.Collections.Generic
open System.Drawing
open System.Threading.Tasks

// ------------------------------------------------------------------------------
// Sequential and parallel editor for images
// ------------------------------------------------------------------------------

/// Interface that defines the editor component
type IImageEditor =
    abstract Rotate : RotateFlipType -> seq<Bitmap> -> unit

/// Sequential implementation of the component
type SerialEditor() =
    interface IImageEditor with
        member x.Rotate rotation images =
            for b in images do b.RotateFlip(rotation)

/// Decorated, parallel implementation of the component
type ParallelEditor(decorated : IImageEditor) =
    interface IImageEditor with
        // Modified (decorated) behavior
        member x.Rotate rotation images =
            Parallel.ForEach(images, fun (b:Bitmap) ->
                b.RotateFlip(rotation) ) |> ignore
