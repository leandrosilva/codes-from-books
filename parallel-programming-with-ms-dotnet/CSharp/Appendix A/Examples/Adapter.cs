//===============================================================================
// Microsoft patterns & practices
// Parallel Programming Guide
//===============================================================================
// Copyright © Microsoft Corporation.  All rights reserved.
// This code released under the terms of the 
// Microsoft patterns & practices license (http://parallelpatterns.codeplex.com/license).
//===============================================================================

using System;
using System.Threading.Tasks;
using Microsoft.Practices.ParallelGuideSamples.Utilities;

namespace Microsoft.Practices.ParallelGuideSamples.RelatedPatterns
{
    public interface IWithFutures
    {
        Task<int> Start();
    }

    public class FuturesBased : IWithFutures
    {
        public Task<int> Start()
        {
            return Task<int>.Factory.StartNew(() =>
                {
                    SampleUtilities.DoCpuIntensiveOperation(2.0);
                    return 42;
                });
        }
    }

    public interface IWithEvents
    {
        void Start();
        event EventHandler<CompletedEventArgs> Completed;
    }
 
    public class EventBased : IWithEvents
    {
        readonly IWithFutures instance = new FuturesBased();

        public void Start()
        {
            Task<int> task = instance.Start();
            task.ContinueWith((t) =>
            {
                var evt = Completed;
                if (evt != null)
                    evt(this, new CompletedEventArgs(t.Result));
            });
        }

        public event EventHandler<CompletedEventArgs> Completed;
    }

    public class CompletedEventArgs : EventArgs
    {
        public int Result { get; private set; }

        public CompletedEventArgs(int result)
        {
            Result = result;
        }
    }
}
