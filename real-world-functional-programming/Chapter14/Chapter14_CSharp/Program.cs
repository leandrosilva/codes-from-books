using System;
using System.Collections.Generic;
using System.Linq;
using System.Text;

// Reference the Parallel Extensions library
using System.Threading;
using System.Threading.Tasks;

using FunctionalCSharp;
using System.Diagnostics;

// --------------------------------------------------------------------------
// Functional Programming in .NET - Chapter 14
// --------------------------------------------------------------------------
// This file demonstrates techniques that are used in sample applications
// --------------------------------------------------------------------------

namespace Chapter14_CSharp
{
  class Program
  {
    // --------------------------------------------------------------------------
    // 14.1.1 Parallelizing islands of imperative code

    static void MainImperative()
    {
      int[] inp = new int[] { 1, 6, 0, 2, 4, 2, 8, 5, 1, 2 };
      int[] res = new int[10];

      // Listing 14.1 For loop for calculating blurred array
      for (int i = 1; i < inp.Length - 1; i++) {
        var sum = inp[i - 1] + inp[i] + inp[i + 1];
        res[i] = sum / 3;
      }

      foreach (int n in res) Console.Write("{0}, ", n);
      Console.WriteLine();

      // Listing 14.2 Parallelized for loop (C# and F#)
      Parallel.For(1, inp.Length - 1, i => {
        var sum = inp[i - 1] + inp[i] + inp[i + 1];
        res[i] = sum / 3;
      });

      foreach (int n in res) Console.Write("{0}, ", n);
      Console.WriteLine();
    }

    // Listing 14.3 Counting the number of primes (C# and F#)
    static bool IsPrime(int n) {
      int max = (int)Math.Sqrt(n);
      for (int i = 2; i <= max; i++)
        if (n % i == 0) return false;
      return true;
    }

    static void MainDeclarative() {
      var nums = Enumerable.Range(1000000, 2000000);

      // Count the primes
      var sw1 = new Stopwatch();
      sw1.Start();
      var primeCount1 = 
        nums.Where(IsPrime)
            .Count();
      sw1.Stop();
      
      // Lising 14.4 Counting primes in parallel (C# and F#)
      var sw2 = new Stopwatch();
      sw2.Start();
      var primeCount2 = 
        nums.AsParallel()
            .Where(IsPrime)
            .Count();
      sw2.Stop();

      Console.WriteLine("Sequential: {0} ({1}ms)\nParallel: {2} ({3}ms)",
        primeCount1, sw1.ElapsedMilliseconds, primeCount2, sw2.ElapsedMilliseconds);
    }

    // --------------------------------------------------------------------------
    // Section 14.1.2 Task-based parallelism

    #region Class hierarchy implementing IntTree 

    // Abstract class that represents the tree
    abstract class IntTree {
      public virtual bool TryGetLeaf(out int value) {
        value = 0;
        return false;
      }
      public virtual bool TryGetNode(out IntTree left, out IntTree right) {
        left = null;
        right = null;
        return false;
      }
    }

    // Represents a leaf of the tree that stores an int value
    class IntTreeLeaf : IntTree {
      private readonly int value;
      public IntTreeLeaf(int value) {
        this.value = value;
      }
      public override bool TryGetLeaf(out int value) {
        value = this.value;
        return true;
      }
    }

    // Represents a node with two sub-nodes
    class IntTreeNode : IntTree {
      private readonly IntTree left, right;
      public IntTreeNode(IntTree left, IntTree right) {
        this.left = left; this.right = right;
      }
      public override bool TryGetNode(out IntTree left, out IntTree right) {
        left = this.left; right = this.right;
        return true;
      }
    }

    #endregion

    // Count the number of primes in the tree
    static int Count(IntTree t)
    {
      int val;
      IntTree l, r;
      if (t.TryGetNode(out l, out r))
        // For a node, recursively process sub-trees
        return Count(l) + Count(r);
      else if (t.TryGetLeaf(out val))
        // For a leaf, test whether it contains a prime
        return IsPrime(val) ? 1 : 0;
      
      // This is unreachable code
      throw new InvalidOperationException();
    }

    // Parallel implementation 
    // The parameter 'd' specifies current depth
    static int ParallelCount(IntTree t, int d)
    {
      int value;
      IntTree l, r;
      // For small sub-tasks, use the non-parallel version
      if (d > 4) return Count(t);

      if (t.TryGetNode(out l, out r)) {
        // Calculate one sub-tree using 'Future'
        var cl = Task.Factory.StartNew(() => ParallelCount(l, d + 1));
        var cr = ParallelCount(r, d + 1);
        // Wait for both of the results and return
        return cl.Result + cr;
      }
      else if (t.TryGetLeaf(out value))
        // Test whether number is a prime
        return IsPrime(value) ? 1 : 0;

      // This is unreachable code
      throw new InvalidOperationException();
    }

    // Generates a tree
    static Random rnd = new Random();
    static IntTree Tree(int depth) {
      if (depth == 0) return new IntTreeLeaf(rnd.Next());
      else return new IntTreeNode(Tree(depth - 1), Tree(depth - 1));
    }
    
    // Test the performance of sequential and parallel version...
    static void MainTasks()
    {
      var t = Tree(16);

      var sw1 = new Stopwatch();
      sw1.Start();
      int c1 = Count(t);
      sw1.Stop();

      // Run once to initialize Parallel Extensions
      ParallelCount(t, 0);

      var sw2 = new Stopwatch();
      sw2.Start();
      int c2 = ParallelCount(t, 0);
      sw2.Stop();
      
      Console.WriteLine("Sequential: {0} ({1}ms)\nParallel: {2} ({3}ms)", 
        c1, sw1.ElapsedMilliseconds, c2, sw2.ElapsedMilliseconds);
    }

    // --------------------------------------------------------------------------
    // Entry point for the whole application

		static void Main(string[] args)
		{
      MainImperative();
      MainDeclarative();
      MainTasks();
    }
	}
}
