using System;
using System.Collections.Generic;
using System.Linq;
using System.Text;

// NOTE: In .NET 4.0, the Lazy type is already available (System.Lazy),
// so we place it into 'Demo' namespace only for a reference.
#if NET4
namespace FunctionalCSharp.LazyDemo
#else
namespace FunctionalCSharp
#endif
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
  // Section 11.3.4 Implementing lazy values for C#

	// Listing 11.17: Implementing lazy values (C#)

	public class Lazy<T> {
		readonly Func<T> func;
    // Represents state of the cache
    bool evaluated = false;
    T value;

		// Creates lazy value from a function
    public Lazy(Func<T> func) {
			this.func = func;
		}

    // Evaluate the lazy value
    public T Value {
      get {
        if (!evaluated) {
          // Compute value and modify the cache
          value = func();
          evaluated = true;
        }
        return value;
      }
    }
	}
}

namespace FunctionalCSharp
{
  // Helper class that enables type inference when creating values
  public class Lazy {
		public static Lazy<T> Create<T>(Func<T> func) {
			return new Lazy<T>(func);
		}
	}
}
