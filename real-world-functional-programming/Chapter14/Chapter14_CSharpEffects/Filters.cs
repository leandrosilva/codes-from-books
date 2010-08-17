using System;
using System.Collections.Generic;
using System.Linq;
using System.Text;
using System.Threading.Tasks;
using FunctionalCSharp;

// --------------------------------------------------------------------------
// Functional Programming in .NET - Chapter 14
// --------------------------------------------------------------------------

namespace GraphicalFilters_CSharp
{
  // --------------------------------------------------------------------------

  /// <summary>
  /// Structure similar to the 'SimpleColor' type in F# discussed in section 14.2.1
  /// </summary>
  struct SimpleColor {
		public int R { get; private set; }
		public int G { get; private set; }
		public int B { get; private set; }

		public SimpleColor(int r, int g, int b) : this() {
			R = r; G = g; B = b;
		}

    // Create color with components in range 0-255
    public SimpleColor ClipColor() {
			return new SimpleColor(Math.Max(0, Math.Min(255, R)),
				Math.Max(0, Math.Min(255, G)), Math.Max(0, Math.Min(255, B)));
		}
    // Component-wise addition of two colors
		public static SimpleColor operator + (SimpleColor c1, SimpleColor c2) {
			return new SimpleColor(c1.R + c2.R, c1.G + c2.G, c1.B + c2.B);
		}
    // Multiply color components by an integer
		public static SimpleColor operator *(SimpleColor c1, int n) {
			return new SimpleColor(c1.R * n, c1.G * n, c1.B * n);
		}
    // Divide color components by an integer
		public static SimpleColor operator /(SimpleColor c1, int n) {
			return new SimpleColor(c1.R / n, c1.G / n, c1.B / n);
		}
    // Color initialized with zeros
		public static SimpleColor Zero {
			get { return new SimpleColor(0, 0, 0); }
		}
	}

  // --------------------------------------------------------------------------
  // Section 14.2.2 Implementing and running color filters  

  // Listing 14.10 Grayscale and lighten filter
	static class Filters 
	{
		public static SimpleColor Grayscale(SimpleColor clr) {
      // Calculates weighted average from the color components
      var c = (clr.R*11 + clr.G*59 + clr.B*30) / 100;
      return new SimpleColor(c, c, c);
		}

		public static SimpleColor Lighten(SimpleColor clr) {
      // Lighten the color an make sure it is valid
      return (clr * 2).ClipColor();
		}

    // Listing 14.11 Sequential method for applying filters (C#)

		public static SimpleColor[,] RunFilter
			(this SimpleColor[,] arr, Func<SimpleColor, SimpleColor> f) {
		  int hgt = arr.GetLength(0), wid = arr.GetLength(1);
      
      // Create a new array as the result
		  var res = new SimpleColor[hgt, wid];

      // Calculate new color for every pixel
      for (int y = 0; y < hgt; y++)
				for(int x = 0; x < wid; x++)
					res[y, x] = f(arr[y, x]);
			return res;
		}


    // --------------------------------------------------------------------------
    // Section 14.2.5 Parallelizing the application

    // Listing 14.17 Applying color filter in parallel

		public static SimpleColor[,] RunFilterParallel
			(this SimpleColor[,] arr, Func<SimpleColor, SimpleColor> f) {
		  int hgt = arr.GetLength(0), wid = arr.GetLength(1);
		  var res = new SimpleColor[hgt, wid];

      // Parallelize the outer loop
			Parallel.For(0, hgt, y => {
        // Leave inner loop sequential
        for(int x = 0; x < wid; x++)
					res[y, x] = f(arr[y, x]);
			});
			return res;
		}


    // --------------------------------------------------------------------------
    // Listing 14.13 Creating graphical effect from a color filter 

    // Takes color filter as an argument
    public static Func<SimpleColor[,], SimpleColor[,]> MakeEffect(Func<SimpleColor, SimpleColor> filter) {
      // Return effect that applies the filter
      return arr => RunFilter(arr, filter);
    }

    // Takes color filter as an argument
    public static Func<SimpleColor[,], SimpleColor[,]> MakeParallelEffect(Func<SimpleColor, SimpleColor> filter) {
      return arr => RunFilterParallel(arr, filter);
    }
  }

  static class Effects {
    // --------------------------------------------------------------------------
    // BONUS: Sample graphical effect
    // Implements 'Blur' effect, which cannot be created using color filters

    // Read color at the specified location, but check bounds of the bitmap
		private static SimpleColor CheckedRead(SimpleColor[,] arr, int y, int x, int hgt, int wid) {
			return arr[Math.Max(0, Math.Min(hgt - 1, y)), Math.Max(0, Math.Min(wid - 1, x))];
		}
  
    // Trick that allows us to write the code below in a simple way
    static void RunForLoop(bool parallel, int from, int to, Action<int> f) {
      if (parallel) Parallel.For(from, to, f);
      else for (int i = from; i < to; i++) f(i);
    }

    // Get pixel color after applying effect to the specified input
		public static SimpleColor[,] Blur(SimpleColor[,] arr, bool parallel) {
      int hgt = arr.GetLength(0), wid = arr.GetLength(1);
      var res = new SimpleColor[hgt, wid];

      // The outer for loop can be parallelized, depending on the argument
      RunForLoop(parallel, 0, hgt, y => {
        // Leave inner loop sequential
        for (int x = 0; x < wid; x++) {
          // Sum colors close to the specified location
          var sum = SimpleColor.Zero;
          for (int dy = -2; dy <= 2; dy++)
            for (int dx = -2; dx <= 2; dx++)
              sum += CheckedRead(arr, y + dy, x + dx, hgt, wid);
          res[y, x] = sum / 25;
        }
      });
      return res;
		}

    // Parallelized version of the blur effect
    public static SimpleColor[,] ParallelBlur(SimpleColor[,] arr) {
      return Blur(arr, true);
    }

    // Sequential version of the blur effect
    public static SimpleColor[,] SequentialBlur(SimpleColor[,] arr) {
      return Blur(arr, false);
    }
  }
}
