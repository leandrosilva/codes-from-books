//===============================================================================
// Microsoft patterns & practices
// Parallel Programming Guide
//===============================================================================
// Copyright © Microsoft Corporation.  All rights reserved.
// This code released under the terms of the 
// Microsoft patterns & practices license (http://parallelpatterns.codeplex.com/license).
//===============================================================================

module Microsoft.Practices.ParallelGuideSamples.ImagePipeline.Extensions

open System
open System.Drawing
open System.Diagnostics.CodeAnalysis
open Microsoft.Practices.ParallelGuideSamples.Utilities

/// Find luma value for filterTasks corresponding to pixel color
/// by formula recommended by the standard, ITU-R Rec.601
/// see http://en.wikipedia.org/wiki/HSL_and_HSV
let grayLuma (pixel:Color) =
    (int pixel.R * 30 + int pixel.G * 59 + int pixel.B * 11) / 100

let addPixelNoise (generator:GaussianRandom) (pixel:Color) =
    let newR = int pixel.R + generator.NextInteger()
    let newG = int pixel.G + generator.NextInteger()
    let newB = int pixel.B + generator.NextInteger()
    let r = max 0 (min 255 newR)
    let g = max 0 (min 255 newG)
    let b = max 0 (min 255 newB)
    Color.FromArgb(r, g, b)

/// Extension methods for System.Drawing.Bitmap
type System.Drawing.Bitmap with 

    /// <summary>
    /// Creates a grayscale image from a color image
    /// </summary>
    /// <remarks>
    /// This code uses Bitmap.GetPixel and SetPixel methods for clarity. An implementation using Bitmap.LockBits
    /// and then directly modifying the image data may be faster, espectially for large images.
    /// </remarks>
    member source.ToGray() =
        if source = null then nullArg "source"
        let bitmap = new Bitmap(source.Width, source.Height)
        for y in 0 .. bitmap.Height - 1 do
            for x in 0 .. bitmap.Width - 1 do
                let luma = source.GetPixel(x, y) |> grayLuma
                bitmap.SetPixel(x, y, Color.FromArgb(luma, luma, luma))
        bitmap

    /// <summary>
    /// Creates an image with a border from this image
    /// </summary>
    /// <remarks>
    /// This code uses Bitmap.GetPixel and SetPixel methods for clarity. An implementation using Bitmap.LockBits
    /// and then directly modifying the image data may be faster, espectially for large images.
    /// </remarks>
    member source.AddBorder(borderWidth) =
        if source = null then nullArg "source"
        let bitmap = new Bitmap(source.Width, source.Height)
        for y in 0 .. source.Height - 1 do 
            let yFlag = (y < borderWidth || (source.Height - y) < borderWidth)
            for x in 0 .. source.Width - 1 do
                let xFlag = (x < borderWidth || (source.Width - x) < borderWidth)
                if xFlag || yFlag then
                    let distance = min y (min (source.Height - y) (min x (source.Width - x)))
                    let percent = float distance / float borderWidth
                    let percent2 = percent * percent
                    let pixel = source.GetPixel(x, y)
                    let color = Color.FromArgb(int(float pixel.R * percent2), int(float pixel.G * percent2), int(float pixel.B * percent2))
                    bitmap.SetPixel(x, y, color)
                else
                    bitmap.SetPixel(x, y, source.GetPixel(x, y))
        bitmap

    /// <summary>
    /// Inserts Gaussian noise into a bitmap.
    /// </summary>
    /// <remarks>
    /// This code uses Bitmap.GetPixel and SetPixel methods for clarity. An implementation using Bitmap.LockBits
    /// and then directly modifying the image data may be faster, espectially for large images.
    /// </remarks>
    member source.AddNoise(amount) =
        if source = null then nullArg "source"
        let generator = new GaussianRandom(0.0, amount, SampleUtilities.MakeRandomSeed())
        let bitmap = new Bitmap(source.Width, source.Height)
        for y in 0 .. bitmap.Height - 1 do
            for x in 0 .. bitmap.Width - 1 do
                let newPixel = source.GetPixel(x, y) |> addPixelNoise generator
                bitmap.SetPixel(x, y, newPixel)
        bitmap
