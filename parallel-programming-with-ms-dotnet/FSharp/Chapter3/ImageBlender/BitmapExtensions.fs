//===============================================================================
// Microsoft patterns & practices
// Parallel Programming Guide
//===============================================================================
// Copyright © Microsoft Corporation.  All rights reserved.
// This code released under the terms of the 
// Microsoft patterns & practices license (http://parallelpatterns.codeplex.com/license).
//===============================================================================

module Microsoft.Practices.ParallelGuideSamples.ImageBlender.Extensions

open System
open System.Drawing

/// Extension methods for System.Drawing.Bitmap
type System.Drawing.Bitmap with

    /// <summary>
    /// Copy all source pixels to destination.
    /// Destination must already exist, must not be smaller than source in either dimension.
    /// </summary>
    /// <param name="source">source Bitmap, possibly without alpha layer</param>
    /// <param name="destination">destination Bitmap, possibly with alpha layer</param>
    /// <remarks>
    /// This code uses Bitmap.GetPixel and SetPixel methods for clarity. An implementation using Bitmap.LockBits
    /// and then directly modifying the image data may be faster, espectially for large images.
    /// </remarks>
    member source.CopyPixels(destination:Bitmap) =
        if source = null then nullArg "source"
        if destination = null then nullArg "destination"
        for y in 0 .. source.Height - 1 do
            for x in 0 .. source.Width - 1 do
                let p = source.GetPixel(x, y)
                destination.SetPixel(x, y, Color.FromArgb(int p.R, int p.G, int p.B))


    /// <summary>
    /// Assign same alpha (transparency) to all pixels in image
    /// </summary>
    /// <param name="bitmap">existing image where alpha is assigned</param>
    /// <param name="alpha">transparency to assign: 256 opaque, 0 invisible</param>
    /// <remarks>
    /// This code uses Bitmap.GetPixel and SetPixel methods for clarity. An implementation using Bitmap.LockBits
    /// and then directly modifying the image data may be faster, espectially for large images.
    /// </remarks>
    member bitmap.SetAlpha(alpha) =
        if bitmap = null then nullArg "bitmap"
        for y in 0 .. bitmap.Height - 1 do
            for x in 0 .. bitmap.Width - 1 do
                let p = bitmap.GetPixel(x, y)
                let p = Color.FromArgb(alpha, int p.R, int p.G, int p.B)
                bitmap.SetPixel(x, y, p)


    /// <summary>
    /// Set a color image to gray
    /// </summary>
    /// <param name="bitmap">existing color image to set gray</param>
    /// <remarks>
    /// This code uses Bitmap.GetPixel and SetPixel methods for clarity. An implementation using Bitmap.LockBits
    /// and then directly modifying the image data may be faster, espectially for large images.
    /// </remarks>
    member bitmap.SetGray() =
        if bitmap = null then
            raise (new ArgumentNullException("bitmap"))
        for y in 0 .. bitmap.Height - 1 do
            for x in 0 .. bitmap.Width - 1 do
                let p = bitmap.GetPixel(x, y);
                let luma = (int p.R * 30 + int p.G * 59 + int p.B * 11) / 100
                bitmap.SetPixel(x, y, Color.FromArgb(luma, luma, luma))