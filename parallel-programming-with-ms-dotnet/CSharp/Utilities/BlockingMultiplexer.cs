//===============================================================================
// Microsoft patterns & practices
// Parallel Programming Guide
//===============================================================================
// Copyright © Microsoft Corporation.  All rights reserved.
// This code released under the terms of the 
// Microsoft patterns & practices license (http://parallelpatterns.codeplex.com/license).
//===============================================================================

using System;
using System.Collections.Generic;
using System.Linq;
using System.Collections.Concurrent;
using System.Threading;

namespace Microsoft.Practices.ParallelGuideSamples.Utilities
{
    /// <summary>
    /// Multiplexer that serializes inputs from multiple producers into a single consumer enumeration in a 
    /// user-specified order. 
    /// </summary>
    /// <typeparam name="T">The type of input element</typeparam>
    /// <remarks>The use case for this class is a producer/consumer scenario with multiple producers and 
    /// a single consumer. The producers each have their private blocking collections for enqueuing the elements
    /// that they produce. The consumer of the producer queues is the multiplexer, which is responsible 
    /// combining the inputs from all of the producers according to user-provided "lock order." The multiplexer 
    /// provides an enumeration that a consumer can use to observe the multiplexed values in the chosen order. 
    /// 
    /// The multiplexer does not perform sorting. Instead, it relies on the fact the the producer queues are
    /// locally ordered and looks for the next value by simultaneously monitoring the heads of 
    /// all of the producer queues.
    /// 
    /// The order of elements in the producer queues is given by a user-provided lockOrderFn delegate. This is called
    /// lock order and is represented by an integer. The initial lock id is specified in the multiplexer's constructor. 
    /// Producer queues must be consistent. This means that they are locally ordered with respect to lock ids. When
    /// multiplexed together into a single enumeration, the producer queues must produce a sequence of values whose 
    /// lock ids are consecutive. (The lock ids in the individual producer queues must be in order but not necessarily 
    /// consecutive.)
    /// 
    /// It is not required that all elements in the producer queues have a lock order. The position of such elements (denoted
    /// by a lock id that is less than zero) is constrained by preceding and succeeding elements in the producer's queue
    /// that do include a lock order. This results in a partial order. The unit tests for this class for an example of 
    /// partial ordering constraints.
    /// 
    /// See Campbell et al, "Multiplexing of Partially Ordered Events," in TestCom 2005, Springer Verlag, June 2005,  
    /// available online at http://research.microsoft.com/apps/pubs/default.aspx?id=77808. 
    /// </remarks>
    public class BlockingMultiplexer<T> 
    {
        #region Local Types

        private enum Mode { Starting, Running, Finished, CleanupStarted }

        // Internal state for each producer
        private struct ProducerInfo
        {
            public int Index;                         // producer id 0, 1, 2, ...
            public BlockingCollection<T> Collection;  // producer's queue
            public bool IsCompleted;                  // true if producer's IsCompleted property was observed to be true 
            public bool HasPendingValue;              // does lookahead value exist?
            public T PendingValue;                    //   if yes, lookahead value
            public int PendingLockId;                 //   if yes, lookahead lock id 
            public int LastLockId;                    // last lock id read (for error checking only)
        }

        #endregion

        #region Fields

        readonly int boundedCapacity = -1;
        readonly object producersLock = new object();
        readonly Func<T, int> lockOrderFn;

        ProducerInfo[] producers = new ProducerInfo[] { };
        Mode mode = Mode.Starting;
        int nextLockId = 0;

        #endregion

        #region Constructors

        /// <summary>
        /// Creates a multiplexer that serializes inputs from multiple producer queues.
        /// </summary>
        /// <param name="lockOrderFn">Delegate that returns an integer sequence number for elements of the 
        /// producer queues. It returns a negative number if order is not important for a given element.</param>
        /// <param name="initialLockId">The first lock id of the sequence</param>
        public BlockingMultiplexer(Func<T, int> lockOrderFn, int initialLockId)
            : this(lockOrderFn, initialLockId, -1)
        {
        }

        /// <summary>
        /// Creates a multiplexer that serializes inputs from multiple producer queues.
        /// </summary>
        /// <param name="lockOrderFn">Delegate that returns an integer sequence number for elements of the 
        /// producer queues. It returns a negative number if order is not important for a given element.</param>
        /// <param name="initialLockId">The first lock id of the sequence</param>
        /// <param name="boundedCapacity">The maximum number of elements that a producer queue
        /// may contain before it blocks the producer.</param>
        public BlockingMultiplexer(Func<T, int> lockOrderFn, int initialLockId, int boundedCapacity)
        {
            if (lockOrderFn == null) throw new ArgumentNullException("lockOrderFn");
            if (initialLockId < 0) throw new ArgumentOutOfRangeException("initialLockId");
            if (boundedCapacity < -1) throw new ArgumentOutOfRangeException("boundedCapacity");
            this.boundedCapacity = boundedCapacity;
            nextLockId = initialLockId;
            this.lockOrderFn = lockOrderFn;
        }

        #endregion

        #region Public Methods

        /// <summary>
        /// Creates a new input source to the multiplexer.
        /// </summary>
        /// <returns>A blocking collection that will be used as one of the multiplexer's inputs.
        /// </returns>
        /// <remarks>This blocking collection for the use of the producer only. Its only consumer of the 
        /// is the multiplexer instance that created it.
        /// 
        /// The producer should invoke Add to insert elements as needed. After the last element, the producer 
        /// invokes CompleteAdding.
        /// 
        /// If the boundedCapacity was specified in the multiplexer's constructor, this value will be used as the
        /// boundedCapacity of the blocking collections used by the producers. This will cause the producer to block
        /// when trying to add elements to the blocking collection above this limit.
        /// 
        /// There is a partial order constraint on the values added by the producer to this blocking collection. The 
        /// lockOrderFn that was provided to the constructor of the multiplexer will be applied to each element in 
        /// the queue by the multiplexer. If the lockOrderFn returns a non-negative value for the enqueued 
        /// object, this value must be strictly greater than the lock order of all previous objects that were added 
        /// to this blocking collection.
        /// 
        /// All producer queues must be created before getting the consumer's enumerable object.</remarks>
        public BlockingCollection<T> GetProducerQueue()
        {
            BlockingCollection<T> result =
                   boundedCapacity > 0 ? new BlockingCollection<T>(boundedCapacity) :
                                          new BlockingCollection<T>();
            lock (producersLock)
            {
                if (mode != Mode.Starting)
                    throw new InvalidOperationException("Cannot get new producer queue for running multiplexer");

                var index = producers.Length;
                Array.Resize(ref producers, index + 1);
                producers[index].Index = index;
                producers[index].Collection = result;
                producers[index].IsCompleted = false;
                producers[index].HasPendingValue = false;
                producers[index].PendingValue = default(T);
                producers[index].PendingLockId = -1;
                producers[index].LastLockId = -1;
            }
            return result;
        }

        /// <summary>
        /// Creates an enumerable object for use by the consumer.
        /// </summary>
        /// <returns>An enumeration of values. The order of the values will respect the lock order of the
        /// producer queues. This method may be called only one time for this object.</returns>
        public IEnumerable<T> GetConsumingEnumerable()
        {
            return GetConsumingEnumerable(CancellationToken.None);
        }

        /// <summary>
        /// Creates an enumerable object for use by the consumer.
        /// </summary>
        /// <param name="token">The cancellation token</param>
        /// <returns>An enumeration of values. The order of the values will respect the lock order of the
        /// producer queues. This method may be called only one time for this object.</returns>
        public IEnumerable<T> GetConsumingEnumerable(CancellationToken token)
        {
            lock (producersLock)
            {
                if (producers.Length == 0)
                    throw new InvalidOperationException("Multiplexer requires at least one producer before getting consuming enumerable");
                if (mode != Mode.Starting)
                    throw new InvalidOperationException("Cannot get enumerator of multiplexer that has already been started");
                mode = Mode.Running;
            }
            bool complete = false;
            while (!complete || producers.Any(info => info.HasPendingValue))
            {
                // Yield case 1: Value with the next lock id is in a lookahead buffer
                if (producers.Any(info => info.HasPendingValue &&
                                           info.PendingLockId == nextLockId))
                {
                    int index = producers.Single(info => info.HasPendingValue &&
                                                  info.PendingLockId == nextLockId).Index;
                    var item = producers[index].PendingValue;

                    // clear lookahead buffer
                    producers[index].HasPendingValue = false;
                    producers[index].PendingValue = default(T);
                    producers[index].PendingLockId = -1;
                    producers[index].LastLockId = nextLockId;

                    // consume value
                    nextLockId += 1;
                    yield return item;
                }

                // Look ahead values exist but we didn't find the next lock id and there are no more
                // values to read from the producer queues. This means that producer blocking collections 
                // violated the contract by failing to give all lock ids between the lowest to the highest observed.
                else if (complete)
                {
                    // Error occurs only for normal termination, not cancellation
                    if (!token.IsCancellationRequested)
                        throw new InvalidOperationException(
                            "Producer blocking collections completed before giving required lock id "
                            + nextLockId.ToString()
                            + ". All values up to "
                            + producers.Where(info => info.HasPendingValue)
                                    .Select(info => info.PendingLockId)
                                    .Max()
                            + " are required."
                            );
                }
                else
                {
                    while (!complete)
                    {
                        // Select producers without lookahead values.
                        var waitList = producers.Where(info => !info.HasPendingValue && !info.IsCompleted)
                                                 .Select(info => info.Collection)
                                                 .ToArray();

                        if (waitList.Length == 0)
                        {
                            if (token.IsCancellationRequested)
                                yield break;
                            else
                                throw new InvalidOperationException("Producer blocking collections omitted required value " +
                                    nextLockId.ToString());
                        }

                        T item = default(T);
                        int waitListIndex = -1;
                        try
                        {
                            waitListIndex = BlockingCollection<T>.TakeFromAny(waitList, out item);
                        }
                        catch (ArgumentException)
                        {
                            // handle occurrence of AddingComplete on another thread.
                            waitListIndex = -2;
                        }
                        if (waitListIndex < 0)
                        {
                            for (int i = 0; i < producers.Length; i++)
                            {
                                if (!producers[i].IsCompleted && producers[i].Collection.IsCompleted)
                                    producers[i].IsCompleted = true;
                            }
                            complete = producers.All(info => info.IsCompleted);
                            continue;
                        }

                        var index = producers.Where(info => info.Collection == waitList[waitListIndex])
                                              .Select(info => info.Index)
                                              .Single();
                        var lockId = lockOrderFn(item);

                        // Yield case 2: Item with no ordering constraint. Consume it immediately.
                        if (lockId < 0)
                        {
                            yield return item;
                        }
                        // Yield case 3: Item read is the one we are looking for. Consume it immediately.
                        else if (lockId == nextLockId)
                        {
                            producers[index].LastLockId = lockId;
                            nextLockId += 1;
                            yield return item;
                            break;
                        }
                        else if (lockId < nextLockId)
                        {
                            throw new InvalidOperationException("Blocking queue delivered duplicate sequence number to multiplexer (1). The duplicate value is "
                                + lockId.ToString());
                        }
                        else if (lockId <= producers[index].LastLockId)
                        {
                            throw new InvalidOperationException("Blocking queue delivered out-of-order item (2)");
                        }
                        else if (producers.Where(info => info.HasPendingValue)
                                           .Any(info => info.PendingLockId == lockId))
                        {
                            throw new InvalidOperationException("Blocking queue delivered duplicate sequence number to multiplexer (2)");
                        }
                        else
                        {
#if DEBUG
                            if (producers[index].HasPendingValue)
                                throw new InvalidOperationException("Internal error-- double read from blocking collection");
#endif
                            producers[index].HasPendingValue = true;
                            producers[index].PendingValue = item;
                            producers[index].PendingLockId = lockId;
                        }
                    }
                }
            }
            lock (producersLock)
            {
                if (mode == Mode.Running)
                    mode = Mode.Finished;
            }
        }

        /// <summary>
        /// Returns an enumeration of all values that have been read by the multiplexer but not yet consumed.
        /// </summary>
        /// <returns>The enumerable object</returns>
        public IEnumerable<T> GetCleanupEnumerable()
        {
            lock (producersLock)
            {
                if (mode == Mode.Finished || mode == Mode.Running)
                    mode = Mode.CleanupStarted;
                else
                    yield break;
            }
            foreach (var p in producers)
            {
                if (p.HasPendingValue)
                {            
                    yield return p.PendingValue;
                }
            }
        }
        
        /// <summary>
        /// Returns the number of items in all of the producer queues and in the multiplexer's buffers
        /// </summary>
        public int Count
        {
            get
            {
                int result = 0;
                foreach (var p in producers)
                {
                    result += p.Collection.Count;
                    result += p.HasPendingValue ? 1 : 0;
                }
                return result;
            }
        }

        #endregion
    }
}

