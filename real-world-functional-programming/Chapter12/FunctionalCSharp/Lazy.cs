using System;
using System.Collections.Generic;
using System.Linq;
using System.Text;

namespace FunctionalCSharp
{
	// --------------------------------------------------------------------------
	// Functional Programming in .NET - Chapter 11
	// --------------------------------------------------------------------------
	// NOTE: This library contains several useful classes for functional
	// programming in C# that we implemented in various chapters of the book.
	// Each secion is marked with a reference to a code listing or section in 
	// the book where it was discussed.
	// --------------------------------------------------------------------------

	// --------------------------------------------------------------------------
	// 11.3.3 Implementing lazy values for C#

	// Listing 11.13: Implementing lazy values (C#)

	public class Lazy<T>
	{
		Func<T> func;
		Option<T> value = Option.None<T>(); 

		public Lazy(Func<T> func) {
			this.func = func;
		}
		public T Force()
		{
			T result;
			if (!value.MatchSome(out result)) {
				result = func();
				value = Option.Some(result);
			}
			return result;
		}
	}

	public class Lazy
	{
		public static Lazy<T> Create<T>(Func<T> func)
		{
			return new Lazy<T>(func);
		}
	}
}
