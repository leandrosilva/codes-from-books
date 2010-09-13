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

namespace Microsoft.Practices.ParallelGuideSamples.RelatedPatterns
{
	public sealed class UnsafeSingleton
	{
        private UnsafeSingleton () { }

        // BAD Code, do not use!
		private static UnsafeSingleton instance = null;

		public static UnsafeSingleton Instance
		{
			get
			{
                // If this is executed on multiple threads more than one
                // instance of the singleton may be created.
				if (instance == null)
					instance = new UnsafeSingleton();
				return instance;
			}
		}
	}

	public sealed class LazyDoubleLockedSingleton
	{
		private static volatile LazyDoubleLockedSingleton instance = null;
		private static object sync = new object();

		private LazyDoubleLockedSingleton() { }

		public static LazyDoubleLockedSingleton Instance
		{
			get
			{
				if (instance == null)
				{
					lock (sync)
					{
						if (instance == null)
							instance = new LazyDoubleLockedSingleton();
					}
				}
				return instance;
			}
		}
	}

    public sealed class NestedSingleton
    {
        private NestedSingleton() { }

        public static NestedSingleton Instance
        {
            get { return SingletonCreator.PrivateInstance; }
        }

        private static class SingletonCreator
        {
			[SuppressMessage("Microsoft.Performance", "CA1810:InitializeReferenceTypeStaticFieldsInline")]
			static SingletonCreator() { }

            internal static readonly NestedSingleton PrivateInstance = new NestedSingleton();
        }
    }

    public sealed class LazySingleton
    {
        private readonly static Lazy<LazySingleton> instance = 
            new Lazy<LazySingleton>(() => new LazySingleton() );

        private LazySingleton() { }

        public static LazySingleton Instance
        {
            get { return instance.Value; }
        }
    }
}
