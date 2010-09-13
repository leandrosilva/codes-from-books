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
using System.Drawing.Drawing2D;
using System.Threading.Tasks;
using System.Windows.Forms;
using Microsoft.Practices.ParallelGuideSamples.Utilities;
using System.IO;

namespace Microsoft.Practices.ParallelGuideSamples.ImageBlender
{
    class Program
    {
        static void SetToGray(Bitmap source, Bitmap layer)
        {
            source.CopyPixels(layer);
            layer.SetGray(); 
            layer.SetAlpha(128);
        }

        static void Rotate(Bitmap source, Bitmap layer)
        {
            source.CopyPixels(layer);
            layer.RotateFlip(RotateFlipType.Rotate90FlipNone);
            layer.SetAlpha(128);
        }

        // Alpha blend: call DrawImage on each layer in turn
        static void Blend(Bitmap layer1, Bitmap layer2, Graphics blender)
        {
            blender.DrawImage(layer1, 0, 0);
            blender.DrawImage(layer2, 0, 0);
        }

        static int SeqentialImageProcessing(Bitmap source1, Bitmap source2, 
                                            Bitmap layer1, Bitmap layer2, Graphics blender)
        {
            SetToGray(source1, layer1);
            Rotate(source2, layer2);
            Blend(layer1, layer2, blender);
            return source1.Width;
        }

        static int ParallelTaskImageProcessing(Bitmap source1, Bitmap source2,
                                            Bitmap layer1, Bitmap layer2, Graphics blender)
        {
            Task toGray = Task.Factory.StartNew(() => SetToGray(source1, layer1));
            Task rotate = Task.Factory.StartNew(() => Rotate(source2, layer2));
            Task.WaitAll(toGray, rotate);
            Blend(layer1, layer2, blender);
            return source1.Width;
        }

        static int ParallelInvokeImageProcessing(Bitmap source1, Bitmap source2, 
                                            Bitmap layer1, Bitmap layer2, Graphics blender)
        {
            Parallel.Invoke(
                () => SetToGray(source1, layer1),
                () => Rotate(source2, layer2));
            Blend(layer1, layer2, blender);
            return source1.Width;
        }

        /// <summary>
        /// Parallel tasks sample
        /// Command line arguments are:
        ///  image source directory, default: current directory
        ///  first source image file, default: flowers.jpg 
        ///  second source image file, default: dog.jpg 
        ///  blended image destination directory, default: current directory  
        /// If any of the directories or files do not exist, the program exits without results.
        /// </summary>
        [STAThread]
        static void Main(string[] args)
        {
            Console.WriteLine("Image Blender Sample\n");
#if DEBUG
            Console.WriteLine("For most accurate timing results, use Release build.\n");
#endif
            string sourceDir = Directory.GetCurrentDirectory();
            string file1 = "flowers.jpg"; // don't rotate
            string file2 = "dog.jpg";     // don't set to gray
            string destDir = Directory.GetCurrentDirectory();

            if (args.Length > 0) sourceDir = args[0];
            if (args.Length > 1) file1 = args[1];
            if (args.Length > 2) file2 = args[2];
            if (args.Length > 3) destDir = args[3];

            string path1 = Path.Combine(sourceDir, file1);
            string path2 = Path.Combine(sourceDir, file2);

            SampleUtilities.CheckDirectoryExists(sourceDir);
            SampleUtilities.CheckFileExists(path1);
            SampleUtilities.CheckFileExists(path2);
            SampleUtilities.CheckDirectoryExists(destDir);

            // Load source images
            var source1 = new Bitmap(path1);
            var source2 = new Bitmap(path2);

            // Prepare for result image
            var layer1 = new Bitmap(source1.Width, source1.Height); // new layer apparently includes alpha layer...
            var layer2 = new Bitmap(source2.Width, source2.Height); // ... even when source does not.

            using (var result = new Bitmap(source1.Width, source1.Height))
            {
                var blender = Graphics.FromImage(result);
                blender.CompositingMode = CompositingMode.SourceOver; // NOT SourceCopy mode

                // Sequential
                SampleUtilities.TimedRun(() =>
                    SeqentialImageProcessing(source1, source2, layer1, layer2, blender),
                    "       Sequential");

                // restore layers to iniital condition; layer2 must be unrotated
                layer1 = new Bitmap(source1.Width, source1.Height);
                layer2 = new Bitmap(source2.Width, source2.Height);

                // Parallel tasks
                SampleUtilities.TimedRun(() =>
                    ParallelTaskImageProcessing(source1, source2, layer1, layer2, blender),
                    "   Parallel tasks");
                
                // restore layers to initial condition; layer2 must be unrotated
                layer1 = new Bitmap(source1.Width, source1.Height);
                layer2 = new Bitmap(source2.Width, source2.Height);

                // Parallel invoke
                SampleUtilities.TimedRun(() =>
                    ParallelInvokeImageProcessing(source1, source2, layer1, layer2, blender),
                    "  Parallel invoke");

                // Save blended image in file
                result.Save(Path.Combine(destDir, "blended.jpg"));

                // Show blended image on screen, pause until user closes window
                Console.WriteLine("Close image window to exit program.");
                using (var form = new Form())  // ensure disposal, prevent warning CA2000
                {
                    using (var pb = new PictureBox())
                    {
                        pb.SizeMode = PictureBoxSizeMode.AutoSize; // fit to image - but form is initially smaller
                        pb.Image = result;
                        form.Controls.Add(pb);
                        form.ShowDialog();
                    }
                }
            } // using result
        }
    }
}
