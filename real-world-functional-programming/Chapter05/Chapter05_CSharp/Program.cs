using System;
using System.Collections.Generic;
using System.Text;

// Required namespace containing the 'Where' method
using System.Linq;

using FunctionalCSharp;

namespace Chapter05_CSharp
{
	// --------------------------------------------------------------------------
	// Functional Programming in .NET - Chapter 5
	// --------------------------------------------------------------------------
	// NOTE: In this chapter we implement several classes that we'll need later
	// in the book, so classes like Option etc. can be found in the 
	// 'FunctionalCSharp' library.
	// --------------------------------------------------------------------------

	class Program
	{
		#region Program entry-point

		static void Main(string[] args)
		{
			Program.MainTuples();
			Program.MainFunctions();
		}

		#endregion

		// --------------------------------------------------------------------------
		// Section 5.1: Multiple values

		// Listing 5.1: Division with a remainder
		static int DivRem(int a, int b, out int rem)
		{
			// Set remainder and return the result
			rem = a % b;
			return a / b;
		}

		static void MainTuples()
		{
			// Calling the method
			int rem;
			int res = DivRem(10, 3, out rem);
		}

		// ----------------------------------------------------------------------------
		// Section 5.2: Alternative values

		#region Class hierarchy for representing schedule

		enum ScheduleType { Never, Repeatedly, Once };

		abstract class Schedule
		{
			public ScheduleType Tag { get; set; }
		}

		class Never : Schedule
		{ }

		class Repeatedly : Schedule
		{
			public Repeatedly(DateTime first, TimeSpan periodicity)
			{
				StartDate = first;
				Interval = periodicity;
			}
			public DateTime StartDate { get; private set; }
			public TimeSpan Interval { get; private set; }
		}

		class Once : Schedule
		{
			public Once(DateTime when) { EventDate = when; }
			public DateTime EventDate { get; private set; }
		}

		#endregion

		// Listing 5.6 Does the event occur next week? (C#)
		bool IsNextWeekDate(DateTime dt)
		{
			return dt > DateTime.Now && dt < DateTime.Now.AddDays(7.0);
		}

    DateTime GetNextOccurrence(Schedule schedule)
    {
      // Switch using the schedule type
      switch (schedule.Tag)
      {
        case ScheduleType.Never:
          return DateTime.MinValue;

        case ScheduleType.Once:
          // Accessing property of the 'Once' class
          var once = (Once)schedule;
          return once.EventDate > DateTime.Now ? 
            once.EventDate : DateTime.MinValue;

        case ScheduleType.Repeatedly:
          // Extract properties of the 'Repeatedly' class
          var rp = (Repeatedly)schedule;
          var secondsFromFirst = (DateTime.Now - rp.StartDate).TotalSeconds;
          double q1 = secondsFromFirst / rp.Interval.TotalSeconds;
          double q2 = Math.Max(q1, 0.0);
          return rp.StartDate.AddSeconds
              (rp.Interval.TotalSeconds * (Math.Floor(q2) + 1.0));
      }
    	throw new InvalidOperationException();
    }

		// --------------------------------------------------------------------------
		// Section 5.3.1: Implementing option type in C#

		static Option<int> ReadInput()
		{
			string s = Console.ReadLine();
			int num;
			if (Int32.TryParse(s, out num))
				return Option.Some(num);
			else
				return Option.None<int>();
		}

		static void TestInput()
		{
			Option<int> inp = ReadInput();
			int num;
			if (inp.MatchSome(out num))
				// Pattern matching for Some
				Console.WriteLine("You entered: {0}", num);
			else if (inp.MatchNone())
				// Pattern matching for None
				Console.WriteLine("Incorrect input!");
		}

		// --------------------------------------------------------------------------
		// Section: 5.3.4 Writing generic functions

		// Generic method with type parameter T
		static T ReadValue<T>(Option<T> opt) 
		{
			T v;
			if (opt.MatchSome(out v)) return v; 
			else throw new InvalidOperationException();
		}

		// ------------------------------------------------------------------------
		// Section 5.4 Function values

		static void MainFunctions() 
		{
			// Listing 5.14 Filtering using predicate
			
			var nums = new[] {4,9,1,8,6};
			// Filtering using predicate
			var evens = nums.Where(n => n % 2 == 0);


			// Section 5.4.1: Lambda functions

			// Square as a delegate using lambda function
			Func<int, int> square =
				a => a * a;

			// Lambda function with multiple parameters
			Func<int, int, int> add =
				(a, b) => a + b;

			// Explicitly specified type of the parameter
			Func<int, string> toStr1 = num => num.ToString();
			Func<int, string> toStr2 = (int num) => num.ToString();


			// Listing 5.16 Advanced lambda functions 

			// Function declared as an Action
			Action<string> sayHello =
				// Lambda function written as a statement block
				s => {               
					var msg = string.Format("Hello {0}!", s);
					Console.WriteLine(msg);
				};

			// ----------------------------------------------------------------------------
			// Section 5.4.2: Function type    

			// Listing 5.17 Function as an argument in C# 

			// Calling using lambda function
			var r1 = Twice(2, n => n * n);
			// Result: r1 == 16


			// Listing 5.18 Function as a return value in C# 

			// Calling method that returns a function
			Func<int, int> add10 = Adder(10);
			// Invoke the returned function
			var r2 = add10(15);
			// Result: r2 == 25	


			// Section 5.4.3: Functions of multiple arguments
			
			// Nested lambda functions
			Func<int, Func<int, int>> addNest =  
				a => b => a + b;
			
      // Adding numbers
			int m = addNest(39)(44);
		}

		// Listing 5.17 Function as an argument in C# and F
		// C# method taking function as an argument
		static int Twice(int n, Func<int, int> f)
		{
			return f(f(n));
		}

		// Listing 5.18 Function as a return value in C# 
		static Func<int, int> Adder(int n)
		{
			// Create return value using lambda function
			return (a) => a + n;
		}
	}
}
