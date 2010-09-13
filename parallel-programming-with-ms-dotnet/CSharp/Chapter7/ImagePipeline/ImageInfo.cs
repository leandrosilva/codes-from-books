//===============================================================================
// Microsoft patterns & practices
// Parallel Programming Guide
//===============================================================================
// Copyright © Microsoft Corporation.  All rights reserved.
// This code released under the terms of the 
// Microsoft patterns & practices license (http://parallelpatterns.codeplex.com/license).
//===============================================================================

using System;
using System.Diagnostics.CodeAnalysis;
using System.Drawing;

namespace Microsoft.Practices.ParallelGuideSamples.ImagePipeline
{
    public class ImageInfo : IDisposable
    {
        // Image data

        public int SequenceNumber { get; private set; }
        public string FileName { get; private set; }
        public Bitmap OriginalImage { get; set; }
        public Bitmap ThumbnailImage { get; set; }
        public Bitmap FilteredImage { get; set; }

        // Image pipeline performance data

        public int ClockOffset { get; private set; }
        [SuppressMessage("Microsoft.Performance", "CA1819:PropertiesShouldNotReturnArrays")]
        public int[] PhaseStartTick { get; private set; }
        [SuppressMessage("Microsoft.Performance", "CA1819:PropertiesShouldNotReturnArrays")]
        public int[] PhaseEndTick {get; private set;}

        public int QueueCount1 { get; set; }
        public int QueueCount2 { get; set; }
        public int QueueCount3 { get; set; }

        public int ImageCount { get;  set; }
        public double FramesPerSecond { get; set; }

        public ImageInfo(int sequenceNumber, string fileName, Bitmap originalImage, int clockOffset)
        {
            SequenceNumber = sequenceNumber;
            FileName = fileName;
            OriginalImage = originalImage;
            ClockOffset = clockOffset;

            PhaseStartTick = (int[])Array.CreateInstance(typeof(int), 4);
            PhaseEndTick = (int[])Array.CreateInstance(typeof(int), 4);
        }

        #region IDisposable Members

        public void Dispose()
        {
            Dispose(true);
            GC.SuppressFinalize(this);
        }

        protected virtual void Dispose(bool disposing)
        {
            if (disposing)
            {
                if (OriginalImage != null)
                {
                    OriginalImage.Dispose();
                    OriginalImage = null;
                }
                if (ThumbnailImage != null)
                {
                    ThumbnailImage.Dispose();
                    ThumbnailImage = null;
                }
                if (FilteredImage != null)
                {
                    FilteredImage.Dispose();
                    FilteredImage = null;
                }
            }
        }

        #endregion
    }
}
