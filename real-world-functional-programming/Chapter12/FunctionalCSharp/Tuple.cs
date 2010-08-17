// --------------------------------------------------------------------------
// Functional Programming in .NET - Chapter 3, 5 and 6
// --------------------------------------------------------------------------
// NOTE: This library contains several useful classes for functional
// programming in C# that we implemented in chapter 3, 5 and 6 and that we'll
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
	/// <typeparam name="TFirst">The type of the first element</typeparam>
	/// <typeparam name="TSecond">The type of the second element</typeparam>
	public class Tuple<TFirst, TSecond>
	{
		private readonly TFirst first;
		private readonly TSecond second;

		/// <summary>
		/// Retyurns the first element of the tuple
		/// </summary>
		public TFirst First
		{
			get { return first; }
		}

		/// <summary>
		/// Returns the second element of the tuple
		/// </summary>
		public TSecond Second
		{
			get { return second; }
		}

		/// <summary>
		/// Create a new tuple value
		/// </summary>
		/// <param name="first">First element of the tuple</param>
		/// <param name="second">Second element of the tuple</param>
		public Tuple(TFirst first, TSecond second)
		{
			this.first = first;
			this.second = second;
		}

		// Listing 3.9: Incrementing population of a city in C#

		/// <summary>
		/// Returns a tuple with the first element set to the specified value.
		/// </summary>
		/// <param name="nsnd">A new value for the first element</param>
		/// <returns>A new tuple with modified first element</returns>
		public Tuple<TFirst, TSecond> SetFirst(TFirst nfst)
		{
			return Tuple.New(nfst, this.second);
		}

		/// <summary>
		/// Returns a tuple with the second element set to the specified value.
		/// </summary>
		/// <param name="nsnd">A new value for the second element</param>
		/// <returns>A new tuple with modified second element</returns>
		public Tuple<TFirst, TSecond> SetSecond(TSecond nsnd)
		{
			return Tuple.New(this.first, nsnd);
		}


		public override bool Equals(object obj)
		{
			var tup = obj as Tuple<TFirst, TSecond>;
			if (tup != null)
				return first.Equals(tup.first) && (second.Equals(tup.second));

			return base.Equals(obj);
		}

    public override int GetHashCode()
    {
      return first.GetHashCode() + second.GetHashCode();
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
		/// <param name="first">First element of the tuple</param>
		/// <param name="second">Second element of the tuple</param>
		/// <returns>A newly created tuple</returns>
		public static Tuple<TFirst, TSecond> New<TFirst, TSecond>(TFirst first, TSecond second)
		{
			return new Tuple<TFirst, TSecond>(first, second);
		}
	}

	// --------------------------------------------------------------------------
	// Section 6.2.1: Methods for working with tuples in C#

	// Listing 6.6 Extension methods for working with tuples

	/// <summary>
	/// Contains extension methods from chapter 6 for working with tuples
	/// </summary>
	public static class TupleUtils
	{
		/// <summary>
		/// Applies given function to the first element of the tuple 
		/// and returns a tuple containing new value as the first element and 
		/// the unchanged second element
		/// </summary>
		public static Tuple<B, C> MapFirst<A, B, C>(this Tuple<A, C> t, Func<A, B> f)
		{
			return Tuple.New(f(t.First), t.Second);
		}


		/// <summary>
		/// Applies given function to the second element of the tuple 
		/// and returns a tuple containing new value as the second element and 
		/// the unchanged first element
		/// </summary>
		public static Tuple<C, B> MapSecond<A, B, C>(this Tuple<C, A> t, Func<A, B> f)
		{
			return Tuple.New(t.First, f(t.Second));
		}
	}
}
