//===============================================================================
// Microsoft patterns & practices
// Parallel Programming Guide
//===============================================================================
// Copyright © Microsoft Corporation.  All rights reserved.
// This code released under the terms of the 
// Microsoft patterns & practices license (http://parallelpatterns.codeplex.com/license).
//===============================================================================

using System;
using System.Collections.Concurrent;
using System.Collections.Generic;
using System.Diagnostics.CodeAnalysis;
using System.Drawing;
using System.IO;
using System.Linq;
using System.Threading;
using System.Threading.Tasks;
using Microsoft.Practices.ParallelGuideSamples.Utilities;

namespace Microsoft.Practices.ParallelGuideSamples.ImagePipeline
{  
    static class ImagePipeline
    {
        const int QueueBoundedCapacity = 4;
        const int LoadBalancingDegreeOfConcurrency = 2;
        const int MaxNumberOfImages = 500;
        const double GaussianNoiseAmount = 50.0; 
        
        #region Image Pipeline Top Level Loop

        /// <summary>
        /// Runs the image pipeline example. The program goes through the jpg images located in the SourceDir
        /// directory and performs a series of steps: it resizes each image and adds a black border and then applies
        /// a Gaussian noise filter operation to give the image a grainy effect. Finally, the program invokes 
        /// a user-provided delegate to the image (for example, to display the image on the user interface).
        /// 
        /// Images are processed in sequential order. That is, the display delegate will be invoked in exactly the same
        /// order as the images appear in the file system.
        /// </summary>
        /// <param name="displayFn">A delegate that is invoked for each image at the end of the pipeline, for example, to 
        /// display the image in the user interface.</param>
        /// <param name="token">A token that can signal an external cancellation request.</param>
        /// <param name="algorithmChoice">The method of calculation. 0=sequential, 1=pipeline, 2=load balanced pipeline</param>
        /// <param name="errorFn">A delegate that will be invoked if this method or any of its parallel subtasks observe an exception during their execution.</param>
        [SuppressMessage("Microsoft.Design", "CA1031:DoNotCatchGeneralExceptionTypes")]
        public static void ImagePipelineMainLoop(Action<ImageInfo> displayFn, CancellationToken token,
            int algorithmChoice, Action<Exception> errorFn)
        {
            try
            {
                string sourceDir = Directory.GetCurrentDirectory();

                // Ensure that frames are presented in sequence before invoking the user-provided display function.
                int imagesSoFar = 0;
                Action<ImageInfo> safeDisplayFn = info =>
                    {
                        if (info.SequenceNumber != imagesSoFar)
                            throw new InvalidOperationException("Images processed out of order. Saw " +
                                info.SequenceNumber.ToString() + " , expected " +
                                imagesSoFar);

                        displayFn(info);
                        imagesSoFar += 1;
                    };

                // Create a cancellation handle for inter-task signaling of exceptions. This cancellation
                // handle is also triggered by the incoming token that indicates user-requested
                // cancellation.
                using (CancellationTokenSource cts = CancellationTokenSource.CreateLinkedTokenSource(token))
                {
                    IEnumerable<string> fileNames = SampleUtilities.GetImageFilenames(sourceDir, MaxNumberOfImages);
                    switch (algorithmChoice)
                    {
                        case 0: 
                            RunSequential(fileNames, sourceDir, safeDisplayFn, cts);
                            break;
                        case 1: 
                            RunPipelined(fileNames, sourceDir, QueueBoundedCapacity, safeDisplayFn, cts);
                            break;
                        case 2: 
                            RunLoadBalancedPipeline(fileNames, sourceDir, QueueBoundedCapacity, safeDisplayFn, cts,
                            LoadBalancingDegreeOfConcurrency);
                            break;
                        default:
							throw new InvalidOperationException("Invalid algorithm choice.");
                    }
                }
            }
            catch (AggregateException ae)
            {
                errorFn((ae.InnerExceptions.Count == 1) ? ae.InnerExceptions[0] : ae);
            }
            catch (Exception e)
            {
                errorFn(e);
            }
        } 

        #endregion
        
        #region Variations (Sequential and Pipelined)
        
        /// <summary>
        /// Run the image processing pipeline.
        /// </summary>
        /// <param name="fileNames">List of image file names in source directory</param>
        /// <param name="sourceDir">Name of directory of source images</param>
        /// <param name="displayFn">Display action</param>
        /// <param name="cts">Cancellation token</param>
        static void RunSequential(IEnumerable<string> fileNames, string sourceDir, 
                                  Action<ImageInfo> displayFn, CancellationTokenSource cts)
        {
            int count = 0;
            int clockOffset = Environment.TickCount;
            int duration = 0;
            var token = cts.Token;
            ImageInfo info = null;
            try
            {
                foreach (var fileName in fileNames)
                {
                    if (token.IsCancellationRequested)
                        break;

                    info = LoadImage(fileName, sourceDir, count, clockOffset);
                    ScaleImage(info);
                    FilterImage(info);
                    int displayStart = Environment.TickCount;
                    DisplayImage(info, count + 1, displayFn, duration);
                    duration = Environment.TickCount - displayStart;

                    count += 1;
                    info = null;
                }
            }
            finally
            {
                if (info != null) info.Dispose();
            }
        }

        /// <summary>
        /// Run the image processing pipeline.
        /// </summary>
        /// <param name="fileNames">List of image file names in source directory</param>
        /// <param name="sourceDir">Name of directory of source images</param>
        /// <param name="queueLength">Length of image queue</param>
        /// <param name="displayFn">Display action</param>
        /// <param name="cts">Cancellation token</param>
        static void RunPipelined(IEnumerable<string> fileNames, string sourceDir, int queueLength, Action<ImageInfo> displayFn,
            CancellationTokenSource cts)
        {
            // Data pipes 
            var originalImages = new BlockingCollection<ImageInfo>(queueLength);
            var thumbnailImages = new BlockingCollection<ImageInfo>(queueLength);
            var filteredImages = new BlockingCollection<ImageInfo>(queueLength);
            try
            {
                var f = new TaskFactory(TaskCreationOptions.LongRunning, TaskContinuationOptions.None);
                Action<ImageInfo> updateStatisticsFn = info =>
                {
                    info.QueueCount1 = originalImages.Count();
                    info.QueueCount2 = thumbnailImages.Count();
                    info.QueueCount3 = filteredImages.Count();
                };

                // Start pipelined tasks
                var loadTask = f.StartNew(() =>
                      LoadPipelinedImages(fileNames, sourceDir, originalImages, cts));

                var scaleTask = f.StartNew(() =>
                      ScalePipelinedImages(originalImages, thumbnailImages, cts));

                var filterTask = f.StartNew(() =>
                      FilterPipelinedImages(thumbnailImages, filteredImages, cts));

                var displayTask = f.StartNew(() =>
                      DisplayPipelinedImages(filteredImages.GetConsumingEnumerable(), 
                           displayFn, updateStatisticsFn, cts));

                Task.WaitAll(loadTask, scaleTask, filterTask, displayTask);
            }
            finally
            {
                // in case of exception or cancellation, there might be bitmaps
                // that need to be disposed.
                DisposeImagesInQueue(originalImages);
                DisposeImagesInQueue(thumbnailImages);
                DisposeImagesInQueue(filteredImages);                
            }
        }

        /// <summary>
        /// Run a variation of the pipeline that uses a user-specified number of tasks for the filter stage.
        /// </summary>
        /// <param name="fileNames">List of image file names in source directory</param>
        /// <param name="sourceDir">Name of directory of source images</param>
        /// <param name="queueLength">Length of image queue</param>
        /// <param name="displayFn">Display action</param>
        /// <param name="cts">Cancellation token</param>
        /// <param name="filterTaskCount">Number of filter tasks</param>
        static void RunLoadBalancedPipeline(IEnumerable<string> fileNames, string sourceDir, int queueLength, Action<ImageInfo> displayFn,
            CancellationTokenSource cts, int filterTaskCount)
        {
            // Create data pipes 
            var originalImages = new BlockingCollection<ImageInfo>(queueLength);
            var thumbnailImages = new BlockingCollection<ImageInfo>(queueLength);
            var filteredImageMultiplexer = new BlockingMultiplexer<ImageInfo>(info => info.SequenceNumber, 0, queueLength);
            var filteredImagesCollections = (BlockingCollection<ImageInfo>[])Array.CreateInstance(
                                   typeof(BlockingCollection<ImageInfo>), filterTaskCount);

            try
            {
                // Start pipelined tasks
                Action<ImageInfo> updateStatisticsFn = info =>
                {
                    info.QueueCount1 = originalImages.Count();
                    info.QueueCount2 = thumbnailImages.Count();
                    info.QueueCount3 = filteredImageMultiplexer.Count;
                };
                const TaskCreationOptions options = TaskCreationOptions.LongRunning;
                var f = new TaskFactory(CancellationToken.None, options, TaskContinuationOptions.None, TaskScheduler.Default);
                Task[] tasks = (Task[])Array.CreateInstance(typeof(Task), filterTaskCount + 3);
                int taskId = 0;

                tasks[taskId++] = f.StartNew(() =>
                      LoadPipelinedImages(fileNames, sourceDir, originalImages, cts));

                tasks[taskId++] = f.StartNew(() =>
                      ScalePipelinedImages(originalImages, thumbnailImages, cts));

                for (int i = 0; i < filterTaskCount; i++)
                {
                    var tmp = i;
                    filteredImagesCollections[tmp] = filteredImageMultiplexer.GetProducerQueue();
                    tasks[taskId++] = f.StartNew(() => FilterPipelinedImages(thumbnailImages, filteredImagesCollections[tmp], cts));
                }

                tasks[taskId++] = f.StartNew(() =>
                      DisplayPipelinedImages(filteredImageMultiplexer.GetConsumingEnumerable(), displayFn, 
                                             updateStatisticsFn, cts));

                Task.WaitAll(tasks);
            }
            finally
            {
                // there might be cleanup in the case of cancellation or an exception.
                DisposeImagesInQueue(originalImages);
                DisposeImagesInQueue(thumbnailImages);
                foreach (var filteredImages in filteredImagesCollections)
                    DisposeImagesInQueue(filteredImages);
                foreach (var info in filteredImageMultiplexer.GetCleanupEnumerable())
                    info.Dispose();
            }
        }
    
        #endregion
        
        #region The Pipeline Phases

        /// <summary>
        /// Image pipeline phase 1: Load images from disk and put them a queue.
        /// </summary>
        static void LoadPipelinedImages(IEnumerable<string> fileNames, string sourceDir, 
            BlockingCollection<ImageInfo> original, CancellationTokenSource cts)
        {
            int count = 0;
            int clockOffset = Environment.TickCount;
            var token = cts.Token;
            ImageInfo info = null;
            try
            {
                foreach (var fileName in fileNames)
                {
                    if (token.IsCancellationRequested)
                        break;
                    info = LoadImage(fileName, sourceDir, count, clockOffset);
                    original.Add(info, token);
                    count += 1;
                    info = null;
                }                
            }
            catch (Exception e)
            {
                // in case of exception, signal shutdown to other pipeline tasks
                cts.Cancel();
                if (!(e is OperationCanceledException))
                    throw;
            }
            finally
            {
                original.CompleteAdding();
                if (info != null) info.Dispose();
            }
        }

        /// <summary>
        /// Image pipeline phase 2: Scale to thumbnail size and render picture frame.
        /// </summary>
        static void ScalePipelinedImages(
            BlockingCollection<ImageInfo> originalImages, 
            BlockingCollection<ImageInfo> thumbnailImages,
            CancellationTokenSource cts)
        {
            var token = cts.Token;
            ImageInfo info = null;
            try
            {
                foreach (var infoTmp in originalImages.GetConsumingEnumerable())
                {
                    info = infoTmp;
                    if (token.IsCancellationRequested)
                        break;
                    ScaleImage(info);
                    thumbnailImages.Add(info, token);
                    info = null;
                }
            }
            catch (Exception e)
            {
                cts.Cancel(); 
                if (!(e is OperationCanceledException))
                    throw;
            }
            finally
            {
                thumbnailImages.CompleteAdding();
                if (info != null) info.Dispose();
            }
        }

        /// <summary>
        /// Image pipeline phase 3: Filter images (give them a speckled appearance by adding Gaussian noise)
        /// </summary>
        static void FilterPipelinedImages(
            BlockingCollection<ImageInfo> thumbnailImages, 
            BlockingCollection<ImageInfo> filteredImages,
            CancellationTokenSource cts)
        {
            ImageInfo info = null;
            try
            {
                var token = cts.Token;
                foreach (ImageInfo infoTmp in
                    thumbnailImages.GetConsumingEnumerable())
                {
                    info = infoTmp;
                    if (token.IsCancellationRequested)
                        break;
                    FilterImage(info);
                    filteredImages.Add(info, token);
                    info = null;
                }
            }
            catch (Exception e)
            {
                cts.Cancel();
                if (!(e is OperationCanceledException))
                    throw;
            }
            finally
            {
                filteredImages.CompleteAdding();
                if (info != null) info.Dispose();
            }
        }

        /// <summary>
        /// Image pipeline phase 4: Invoke the user-provided delegate (for example, to display the result in a UI)
        /// </summary>
        static void DisplayPipelinedImages(IEnumerable<ImageInfo> filteredImages, 
                                           Action<ImageInfo> displayFn,
                                           Action<ImageInfo> updateStatisticsFn, 
                                           CancellationTokenSource cts) 
        {
            int count = 1;
            int duration = 0;
            var token = cts.Token;
            ImageInfo info = null;
            try
            {
                foreach (ImageInfo infoTmp in filteredImages)
                {
                    info = infoTmp;
                    if (token.IsCancellationRequested)
                        break;
                    int displayStart = Environment.TickCount;
                    updateStatisticsFn(info);
                    DisplayImage(info, count, displayFn, duration);
                    duration = Environment.TickCount - displayStart;

                    count = count + 1;
                    info = null;
                }
            }
            catch (Exception e)
            {
                cts.Cancel();
                if (!(e is OperationCanceledException))
                    throw;
            }
            finally
            {
                if (info != null) info.Dispose();
            }
        }
        
        #endregion     
 
        #region Operations for Individual Images

        [SuppressMessage("Microsoft.Reliability", "CA2000:DisposeObjectsBeforeLosingScope")]
        static ImageInfo LoadImage(string fname, string sourceDir, int count, int clockOffset)
        {
            int startTick = Environment.TickCount;
            ImageInfo info = null;
            Bitmap bitmap = new Bitmap(Path.Combine(sourceDir, fname));
            try
            {
                bitmap.Tag = fname;

                info = new ImageInfo(count, fname, bitmap, clockOffset);
                info.PhaseStartTick[0] = startTick - clockOffset;
                bitmap = null;
            }
            finally
            {
                if (bitmap != null) bitmap.Dispose();
            }

            if (info != null) info.PhaseEndTick[0] = Environment.TickCount - clockOffset;
            return info;
        }

        static void ScaleImage(ImageInfo info)
        {
            int startTick = Environment.TickCount;
            var orig = info.OriginalImage;
            info.OriginalImage = null;
            const int scale = 200;
            var isLandscape = (orig.Width > orig.Height);
            var newWidth = isLandscape ? scale : scale * orig.Width / orig.Height;
            var newHeight = !isLandscape ? scale : scale * orig.Height / orig.Width;
            Bitmap bitmap = new Bitmap(orig, newWidth, newHeight);
            try
            {
                Bitmap bitmap2 = bitmap.AddBorder(15);
                try
                {
                    bitmap2.Tag = orig.Tag;
                    info.ThumbnailImage = bitmap2;
                    info.PhaseStartTick[1] = startTick - info.ClockOffset;
                    bitmap2 = null;
                }
                finally
                {
                    if (bitmap2 != null) bitmap2.Dispose();
                }
            }
            finally
            {
                bitmap.Dispose();
                orig.Dispose();
            }
            info.PhaseEndTick[1] = Environment.TickCount - info.ClockOffset;
        }

        static void FilterImage(ImageInfo info)
        {
            int startTick = Environment.TickCount;
            var sc = info.ThumbnailImage;
            info.ThumbnailImage = null;
            Bitmap bitmap = sc.AddNoise(GaussianNoiseAmount);

            try
            {
                bitmap.Tag = sc.Tag;
                info.FilteredImage = bitmap;
                info.PhaseStartTick[2] = startTick - info.ClockOffset;

                bitmap = null;
            }
            finally
            {
                if (bitmap != null) bitmap.Dispose();
                sc.Dispose();
            }
            info.PhaseEndTick[2] = Environment.TickCount - info.ClockOffset;
        }

        static void DisplayImage(ImageInfo info, int count, Action<ImageInfo> displayFn, int duration)
        {
            int startTick = Environment.TickCount;
            info.ImageCount = count;
            info.PhaseStartTick[3] = startTick - info.ClockOffset;
            info.PhaseEndTick[3] = (duration > 0) ? startTick - info.ClockOffset + duration :
                                                     Environment.TickCount - info.ClockOffset;
            displayFn(info);
        } 

        #endregion

        #region Cleanup methods used by error handling

        // Ensure that the queue contents is disposed. You could also implement this by 
        // subclassing BlockingCollection<> and providing an IDisposable implmentation.
        static void DisposeImagesInQueue(BlockingCollection<ImageInfo> queue)
        {
            if (queue != null)
            {
                queue.CompleteAdding();
                foreach (var info in queue)
                {
                    info.Dispose();
                }
            }
        }

        #endregion
    }
}
