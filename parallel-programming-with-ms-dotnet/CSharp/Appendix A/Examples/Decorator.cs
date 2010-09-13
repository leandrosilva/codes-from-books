//===============================================================================
// Microsoft patterns & practices
// Parallel Programming Guide
//===============================================================================
// Copyright © Microsoft Corporation.  All rights reserved.
// This code released under the terms of the 
// Microsoft patterns & practices license (http://parallelpatterns.codeplex.com/license).
//===============================================================================

using System.Collections.Generic;
using System.Drawing;
using System.Threading.Tasks;

namespace Microsoft.Practices.ParallelGuideSamples.RelatedPatterns
{
    #region Calculate example

    public interface IImageEditor
    {
        void Rotate(RotateFlipType rotation, IEnumerable<Bitmap> images);
    }

    public class SerialEditor : IImageEditor
    {
        public void Rotate(RotateFlipType rotation, IEnumerable<Bitmap> images)
        {
            foreach (Bitmap b in images)
                b.RotateFlip(rotation);
        }
    }

    public class ParallelEditor : IImageEditor
    {
        private IImageEditor decorated;

        public ParallelEditor(IImageEditor decorated)
        {
            this.decorated = decorated;
        }

        // Modified behavior
        public void Rotate(RotateFlipType rotation, IEnumerable<Bitmap> images)
        {
            if (decorated == null)
                return;
            Parallel.ForEach(images, b =>
                {
                    b.RotateFlip(rotation);
                });
        }

        // Additional behavior...
    }

    #endregion
}
