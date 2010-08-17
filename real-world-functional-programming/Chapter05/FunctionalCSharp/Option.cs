using System;
using System.Collections.Generic;
using System.Linq;
using System.Text;

namespace FunctionalCSharp
{
	// --------------------------------------------------------------------------
	// Functional Programming in .NET - Chapter 3 and 5
	// --------------------------------------------------------------------------
	// NOTE: This library contains several useful classes for functional
	// programming in C# that we implemented in chapter 3 and 5 and that we'll
	// extend and use later in the book. Each secion is marked with a reference
	// to a code listing or section in the book where it was discussed.
	// --------------------------------------------------------------------------

	// --------------------------------------------------------------------------
	// Section 5.3.1: Implementing option type in C#

	// Listing 5.9 Generic option type using classes (C#)

	/// <summary>
	/// Enumeration with possible alternatives of the Option type
	/// </summary>
	public enum OptionType { Some, None };

  /// <summary>
  /// Represents option value that can be either 'None' or 'Some(v)'
  /// </summary>
  public abstract class Option<T>
	{
    private OptionType tag;

    /// <summary>
    /// Creates the option type and takes the type of the 
    /// created option as an argument.
    /// </summary>
    protected Option(OptionType tag) { 
      this.tag = tag;
    }
		
    /// <summary>
		/// Specifies alternative represented by this instance
		/// </summary>
    public OptionType Tag { get { return tag; } }

		// Listing 5.10 "Pattern matching" methods for Option class (C#)

		/// <summary>
		/// Matches 'None' alternative
		/// </summary>
		/// <returns>Returns true when succeeds</returns>
		public bool MatchNone()
		{
			return Tag == OptionType.None;
		}

		/// <summary>
		/// Matches 'Some' alternative
		/// </summary>
		/// <param name="value">When succeeds sets this parameter to the carried value</param>
		/// <returns>Returns true when succeeds</returns>
		public bool MatchSome(out T value)
		{
			if (Tag == OptionType.Some) value = ((Some<T>)this).Value;
			else value = default(T);
			return Tag == OptionType.Some;
		}
	}

	/// <summary>
	/// Inherited class representing empty option
	/// </summary>
	public class None<T> : Option<T>
	{
		public None() : base(OptionType.None) {
		}
	}

	/// <summary>
	/// Inherited class representing option with value
	/// </summary>
	public class Some<T> : Option<T>
	{
		public Some(T value) : base(OptionType.Some)
		{
			Value = value;
		}

		/// <summary>
		/// Returns value carried by the option
		/// </summary>
		public T Value { get; private set; }
	}

	/// <summary>
	///  Utility class for creating options
	/// </summary>
	public static class Option
	{
		/// <summary>
		/// Creates an empty option
		/// </summary>
		public static Option<T> None<T>()
		{
			return new None<T>();
		}

		/// <summary>
		/// Creates option with a value. This method can be
		/// used without type parameters thanks to C# type inference
		/// </summary>
		public static Option<T> Some<T>(T value)
		{
			return new Some<T>(value);
		}
	}
}
