//===============================================================================
// Microsoft patterns & practices
// Parallel Programming Guide
//===============================================================================
// Copyright © Microsoft Corporation.  All rights reserved.
// This code released under the terms of the 
// Microsoft patterns & practices license (http://parallelpatterns.codeplex.com/license).
//===============================================================================

using System;
using System.Drawing;

namespace Microsoft.Practices.ParallelGuideSamples.ImageBlender
{
    /// <summary>
    /// Extension methods for System.Drawing.Bitmap
    /// </summary>
    public static class BitmapExtensions
    {
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
        public static void CopyPixels(this Bitmap source, Bitmap destination)
        {
            if (source == null)
                throw new ArgumentNullException("source");
            if (destination == null)
                throw new ArgumentNullException("destination");
            for (int y = 0; y < source.Height; y++)
            {
                for (int x = 0; x < source.Width; x++)
                {
                    var p = source.GetPixel(x, y);
                    destination.SetPixel(x, y, Color.FromArgb(p.R, p.G, p.B)); // apparently preserves alpha
                }
            }
        }

        /// <summary>
        /// Assign same alpha (transparency) to all pixels in image
        /// </summary>
        /// <param name="bitmap">existing image where alpha is assigned</param>
        /// <param name="alpha">transparency to assign: 256 opaque, 0 invisible</param>
        /// <remarks>
        /// This code uses Bitmap.GetPixel and SetPixel methods for clarity. An implementation using Bitmap.LockBits
        /// and then directly modifying the image data may be faster, espectially for large images.
        /// </remarks>
        public static void SetAlpha(this Bitmap bitmap, int alpha)
        {
            if (bitmap == null)
                throw new ArgumentNullException("bitmap");
            for (int x = 0; x < bitmap.Width; x++)
            {
                for (int y = 0; y < bitmap.Height; y++)
                {
                    var p = bitmap.GetPixel(x, y);
                    p = Color.FromArgb(alpha, p.R, p.G, p.B);
                    bitmap.SetPixel(x, y, p);
                }
            }
        }

        /// <summary>
        /// Set a color image to gray
        /// </summary>
        /// <param name="bitmap">existing color image to set gray</param>
        /// <remarks>
        /// This code uses Bitmap.GetPixel and SetPixel methods for clarity. An implementation using Bitmap.LockBits
        /// and then directly modifying the image data may be faster, espectially for large images.
        /// </remarks>
        public static void SetGray(this Bitmap bitmap)
        {
            if (bitmap == null)
                throw new ArgumentNullException("bitmap");
            for (int y = 0; y < bitmap.Height; y++)
            {
                for (int x = 0; x < bitmap.Width; x++)
                {
                    var pixel = bitmap.GetPixel(x, y);
                    int luma = (int)(pixel.R * 0.3 + pixel.G * 0.59 + pixel.B * 0.11);
                    bitmap.SetPixel(x, y, Color.FromArgb(luma, luma, luma));
                }
            }
        }
    }
}
