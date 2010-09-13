//===============================================================================
// Microsoft patterns & practices
// Parallel Programming Guide
//===============================================================================
// Copyright © Microsoft Corporation.  All rights reserved.
// This code released under the terms of the 
// Microsoft patterns & practices license (http://parallelpatterns.codeplex.com/license).
//===============================================================================

namespace Microsoft.Practices.ParallelGuideSamples.RelatedPatterns

open System

/// Naive implementation that is buggy (may create multiple instances)
[<AllowNullLiteral>]
type UnsafeSingleton private() =

    // BAD Code, do not use!
    static let mutable instance : UnsafeSingleton = null

    static member Instance =
        // If this is executed on multiple threads more than one
        // instance of the singleton may be created.
        if instance = null then 
            instance <- new UnsafeSingleton()
        instance


/// Solution based on double locking (correct with volatile attribute)
[<AllowNullLiteral>]
type LazyDoubleLockedSingleton private() =
    [<VolatileField>]
    static let mutable instance : LazyDoubleLockedSingleton = null
    static let sync = new obj()

    static member Instance =
        if instance = null then
            lock sync (fun () ->
                if instance = null then
                    instance <- new LazyDoubleLockedSingleton() )
        instance


/// Solution that is originally based on nested classes (in C#), but in F#, 
/// we use two mutually recursive classes (one internal)
type internal SingletonCreator private () =
    //static new() = ()
    static let privateInstance = new NestedSingleton()
    static member PrivateInstance = privateInstance

and NestedSingleton internal () =
    static member Instance =
        SingletonCreator.PrivateInstance


/// The best option - delegate the lazy initialization to the .NET 4.0 Lazy type
/// (in F#, this corresponds to the 'lazy' language construct)
type LazySingleton private () =
    static let instance = lazy new LazySingleton()
    member x.Instance = instance.Value
