using System;
using System.Collections.Generic;
using System.Linq;
using System.Text;

namespace FunctionalCSharp
{
	// --------------------------------------------------------------------------
	// Functional Programming in .NET - Chapter 3, 5 and 6
	// --------------------------------------------------------------------------
	// NOTE: This library contains several useful classes for functional 
	// programming in C# that we implemented in chapters 3, 5 and 6 and that we'll 
	// extend and use later in the book. Each secion is marked with a reference
	// to a code listing or section in the book where it was discussed.
	// --------------------------------------------------------------------------

	// --------------------------------------------------------------------------
	// Section 3.3.3: Functional lists in C#

	// Listing 3.14 Functional list in C#

	/// <summary>
	/// Represents a functional list that can be eihter 
	/// 'empty list' or a 'cons cell'
	/// </summary>
	public class FuncList<T>
	{
		/// <summary>
		/// Constructor that creates an empty list
		/// </summary>
		public FuncList()
		{
			IsEmpty = true;
		}

		/// <summary>
		/// Constructor that creates a cons cell with an element 
		/// (head) and a reference to the rest of the list (tail)
		/// </summary>
		/// <param name="head">The elemnet stored by the cons cell</param>
		/// <param name="tail">Reference to the rest of the list</param>
		public FuncList(T head, FuncList<T> tail)
		{
			IsEmpty = false;
			Head = head;
			Tail = tail;
		}

		/// <summary>
		/// Is the list empty list or a cons cell?
		/// </summary>
		public bool IsEmpty { get; private set; }

		/// <summary>
		/// Returns the element stored in the cons cell
		/// </summary>
		public T Head { get; private set; }

		/// <summary>
		/// Returns reference to the rest of the list when the list is a cons cell
		/// </summary>
		public FuncList<T> Tail { get; private set; }
	}

	/// <summary>
	/// Utility class for constructing lists
	/// </summary>
	public static class FuncList
	{
		/// <summary>
		/// Creates an empty list
		/// </summary>
		public static FuncList<T> Empty<T>()
		{
			return new FuncList<T>();
		}

		/// <summary>
		/// Creates a cons cell storing an element of the list.
		/// This method can be used without specifying generic 
		/// type parameters thanks to the C# type inference.
		/// </summary>
		public static FuncList<T> Cons<T>(T head, FuncList<T> tail)
		{
			return new FuncList<T>(head, tail);
		}
	}
}
