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
using Microsoft.Practices.ParallelGuideSamples.Utilities;

namespace Microsoft.Practices.ParallelGuideSamples.ImagePipeline
{
    /// <summary>
    /// Extension methods for System.Drawing.Bitmap
    /// </summary>
    public static class BitmapExtensions
    {
        /// <summary>
        /// Find luma value for filterTasks corresponding to pixel color
        /// by formula recommended by the standard, ITU-R Rec.601
        /// see http://en.wikipedia.org/wiki/HSL_and_HSV
        /// </summary>
        /// <param name="pixel">color of pixel to convert to filterTasks</param>
        /// <returns></returns>
        [System.Diagnostics.CodeAnalysis.SuppressMessage("Microsoft.Naming", "CA1704:IdentifiersShouldBeSpelledCorrectly", MessageId = "Luma")]
        public static int GrayLuma(Color pixel)
        {
            return (int)(pixel.R * 0.3 + pixel.G * 0.59 + pixel.B * 0.11);
        }

        /// <summary>
        /// Creates a grayscale image from a color image.
        /// </summary>
        /// <param name="source">Color image (Bitmap)</param>
        /// <returns>Grayscale image</returns>
        /// <remarks>
        /// This code uses Bitmap.GetPixel and SetPixel methods for clarity. An implementation using Bitmap.LockBits
        /// and then directly modifying the image data may be faster, espectially for large images.
        /// </remarks>
        public static Bitmap ToGray(this Bitmap source)
        {
            if (source == null)
                throw new ArgumentNullException("source");
            Bitmap bitmap = null;
            Bitmap tempBitmap = null;
            try // ensure disposal, prevent warning CA2000
            {
                tempBitmap = new Bitmap(source.Width, source.Height);
                for (int y = 0; y < tempBitmap.Height; y++)
                    {
                        for (int x = 0; x < tempBitmap.Width; x++)
                        {
                            var pixel = source.GetPixel(x, y);
                            int luma = GrayLuma(pixel);
                            tempBitmap.SetPixel(x, y, Color.FromArgb(luma, luma, luma));
                        }
                    }
                bitmap = tempBitmap;
                tempBitmap = null;
            }
            finally
            {
                if (tempBitmap != null) tempBitmap.Dispose();
            }
            return bitmap;
        }

        /// <summary>
        /// Creates an image with a border from this image.
        /// </summary>
        /// <param name="source">Color image (Bitmap)</param>
        /// <param name="borderWidth">Width of border</param>
        /// <returns>Image with border</returns>
        /// <remarks>
        /// This code uses Bitmap.GetPixel and SetPixel methods for clarity. An implementation using Bitmap.LockBits
        /// and then directly modifying the image data may be faster, espectially for large images.
        /// </remarks>
        public static Bitmap AddBorder(this Bitmap source, int borderWidth)
        {
            if (source == null)
                throw new ArgumentNullException("source");
            Bitmap bitmap = null;
            Bitmap tempBitmap = null;
            try 
            {
                int width = source.Width;
                int height = source.Height;
                tempBitmap = new Bitmap(width, height);
                for (int y = 0; y < height; y++)
                {
                    bool yFlag = (y < borderWidth || (height - y) < borderWidth);
                    for (int x = 0; x < width; x++)
                    {
                        bool xFlag = (x < borderWidth || (width - x) < borderWidth);
                        if (xFlag || yFlag)
                        {
                            var distance = Math.Min(y, Math.Min(height - y, Math.Min(x, width - x)));
                            var percent = distance / (double)borderWidth;
                            var percent2 = percent * percent;
                            var pixel = source.GetPixel(x, y);
                            var color = Color.FromArgb((int)(pixel.R * percent2), (int)(pixel.G * percent2), (int)(pixel.B * percent2));
                            tempBitmap.SetPixel(x, y, color);
                        }
                        else
                        {
                            tempBitmap.SetPixel(x, y, source.GetPixel(x, y));
                        }
                    }
                }
                bitmap = tempBitmap;
                tempBitmap = null;
            }
            finally
            {
                if (tempBitmap != null) tempBitmap.Dispose();
            }
            return bitmap;
        }

        /// <summary>
        /// Inserts Gaussian noise into a bitmap.
        /// </summary>
        /// <param name="source">Bitmap to be processed</param>
        /// <param name="amount">Standard deviation of perturbation for each color channel.</param>
        /// <returns>New, speckled bitmap</returns>
        /// <remarks>
        /// This code uses Bitmap.GetPixel and SetPixel methods for clarity. An implementation using Bitmap.LockBits
        /// and then directly modifying the image data may be faster, espectially for large images.
        /// </remarks>
        public static Bitmap AddNoise(this Bitmap source, double amount)
        {
            if (source == null)
                throw new ArgumentNullException("source");
            Bitmap bitmap = null;
            Bitmap tempBitmap = null;
            try
            {
                var generator = new GaussianRandom(0.0, amount, SampleUtilities.MakeRandomSeed());
                tempBitmap = new Bitmap(source.Width, source.Height);
                for (int y = 0; y < tempBitmap.Height; y++)
                {
                    for (int x = 0; x < tempBitmap.Width; x++)
                    {
                        var pixel = source.GetPixel(x, y);
                        Color newPixel = AddPixelNoise(pixel, generator);
                        tempBitmap.SetPixel(x, y, newPixel);
                    }
                }
                bitmap = tempBitmap;
                tempBitmap = null;
            }
            finally
            {
                if (tempBitmap != null) tempBitmap.Dispose();
            }
            return bitmap;
        }

        static Color AddPixelNoise(Color pixel, GaussianRandom generator)
        {
            int newR = (int)pixel.R + generator.NextInteger();
            int newG = (int)pixel.G + generator.NextInteger();
            int newB = (int)pixel.B + generator.NextInteger();
            int r = Math.Max(0, Math.Min(newR, 255));
            int g = Math.Max(0, Math.Min(newG, 255));
            int b = Math.Max(0, Math.Min(newB, 255));
            return Color.FromArgb(r, g, b);
        }
    }
}

