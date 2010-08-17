// --------------------------------------------------------------------------
// Functional Programming in .NET - Chapter 3
// --------------------------------------------------------------------------
// NOTE: This library contains several useful classes for functional 
// programming in C# that we implemented in chapter 3 and that we'll 
// extend and use later in the book. Each secion is marked with a reference
// to a code listing or section in the book where it was discussed.
// --------------------------------------------------------------------------

using System;
using System.Collections.Generic;
using System.Linq;
using System.Text;

// NOTE: In .NET 4.0, the Tuple type is already available (System.Tuple),
// so we place it into 'Demo' namespace only for a reference.
#if NET4
namespace FunctionalCSharp.TupleDemo
#else
namespace FunctionalCSharp
#endif
{
  // --------------------------------------------------------------------------
  // Section 3.2.2: Implementing tuple in C#

  // Listing 3.7: Implementing the tuple type in C#

  /// <summary>
  /// Represents a functional tuple that can be used to store 
  /// two values of different types inside one object.
  /// </summary>
  /// <typeparam name="T1">The type of the first element</typeparam>
  /// <typeparam name="T2">The type of the second element</typeparam>
  public sealed class Tuple<T1, T2>
  {
    private readonly T1 item1;
    private readonly T2 item2;

    /// <summary>
    /// Retyurns the first element of the tuple
    /// </summary>
    public T1 Item1
    {
      get { return item1; }
    }

    /// <summary>
    /// Returns the second element of the tuple
    /// </summary>
    public T2 Item2
    {
      get { return item2; }
    }

    /// <summary>
    /// Create a new tuple value
    /// </summary>
    /// <param name="item1">First element of the tuple</param>
    /// <param name="second">Second element of the tuple</param>
    public Tuple(T1 item1, T2 item2)
    {
      this.item1 = item1;
      this.item2 = item2;
    }
  }

  // Listing 3.8: Improved type inference for tuples in C#

  /// <summary>
  /// Utility class that simplifies cration of tuples by using 
  /// method calls instead of constructor calls
  /// </summary>
  public static class Tuple
  {
    /// <summary>
    /// Creates a new tuple value with the specified elements. The method 
    /// can be used without specifying the generic parameters, because C#
    /// compiler can usually infer the actual types.
    /// </summary>
    /// <param name="item1">First element of the tuple</param>
    /// <param name="second">Second element of the tuple</param>
    /// <returns>A newly created tuple</returns>
    public static Tuple<T1, T2> Create<T1, T2>(T1 item1, T2 second)
    {
      return new Tuple<T1, T2>(item1, second);
    }
  }
}

// Extension methods for Tuples (on both .NET 4 and earlier)
namespace FunctionalCSharp
{
  public static class TupleExtensions
  {
    // Listing 3.9: Incrementing population of a city in C#

    /// <summary>
    /// Returns a tuple with the first element set to the specified value.
    /// </summary>
    /// <param name="nsnd">A new value for the first element</param>
    /// <returns>A new tuple with modified first element</returns>
    public static Tuple<T1, T2> WithItem1<T1, T2>(this Tuple<T1, T2> tuple, T1 newItem1)
    {
      return Tuple.Create(newItem1, tuple.Item2);
    }

    /// <summary>
    /// Returns a tuple with the second element set to the specified value.
    /// </summary>
    /// <param name="nsnd">A new value for the second element</param>
    /// <returns>A new tuple with modified second element</returns>
    public static Tuple<T1, T2> WithItem2<T1, T2>(this Tuple<T1, T2> tuple, T2 newItem2)
    {
      return Tuple.Create(tuple.Item1, newItem2);
    }
  }
}