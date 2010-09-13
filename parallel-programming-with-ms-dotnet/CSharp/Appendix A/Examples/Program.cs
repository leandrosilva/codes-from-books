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

using System;
using System.Collections.Generic;
using System.Drawing;
using System.IO;
using Microsoft.Practices.ParallelGuideSamples.Utilities;

namespace Microsoft.Practices.ParallelGuideSamples.RelatedPatterns
{
    class Program
    {
        static void Main()
        {
            Console.WriteLine("Supporting Patterns Samples\n");
#if DEBUG
            Console.WriteLine("For most accurate timing results, use Release build.\n");
#endif
            DecoratorExample();

            Console.WriteLine("\nRun complete... press enter to finish.");
            Console.ReadLine();
        }

        private static void DecoratorExample()
        {
            const int maxImages = 1000;

            Console.WriteLine("Loading images...");
            IList<Bitmap> images = LoadImages(maxImages);

            IImageEditor serial = new SerialEditor();
            SampleUtilities.TimedAction(() => { serial.Rotate(RotateFlipType.RotateNoneFlipX, images); }, "Rotate, sequential");

            IImageEditor parallel = new ParallelEditor(new SerialEditor());
            SampleUtilities.TimedAction(() => { parallel.Rotate(RotateFlipType.RotateNoneFlipX, images); }, "Rotate, parallel");
        }

        private static IList<Bitmap> LoadImages(int maxImages)
        {
            IEnumerable<string> paths = SampleUtilities.GetImageFilenames(Directory.GetCurrentDirectory(), maxImages);
            IList<Bitmap> images = new List<Bitmap>();
            int i = 0;
            foreach (var img in paths)
            {
                images.Add(new Bitmap(Path.Combine(img)));
                if (i++ > maxImages)
                    break;
            }
            return images;
        }
    }
}
