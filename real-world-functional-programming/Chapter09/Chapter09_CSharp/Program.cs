using System;
using System.Collections.Generic;

// --------------------------------------------------------------------------
// Functional Programming in .NET - Chapter 9
// --------------------------------------------------------------------------

// Note: The following using is for F# <-> C# interoperability examples
using Chapter09.FSharpExport;

class Program {

  class Client
  {
    public string Name { get; set; }
    public int Income { get; set; }
    public int YearsInJob { get; set; }
    public bool UsesCreditCard { get; set; }
    public bool CriminalRecord { get; set; }
  }

  // --------------------------------------------------------------------------
  // Section 9.4.21 Implementing interfaces and casting 

  interface IClientTest {
    bool Test(Client client);
    void Report(Client client);
  }

  // Listing 9.16 Client test using explicit interface implementation

  // Class implementing 'IClientTest'
  class CoefficientTest : IClientTest {
    double qIncome, qYrs, qMin;
    
    public CoefficientTest(double income, double years, double min) {
      // Store arguments in a private field
      qIncome = income; qYrs = years; qMin = min;
    }
    public void PrintInfo() {
      Console.WriteLine("income*{0}+years*{1}>{2}", qIncome, qYrs, qMin);
    }

    // Private implementations of interface methods
    bool IClientTest.Test(Client cl)
    {
      return cl.Income*qIncome + cl.YearsInJob*qYrs < qMin;
    }
    void IClientTest.Report(Client cl) {
      Console.WriteLine("Coefficient {0} is less than {1}.",
        cl.Income*qIncome + cl.YearsInJob*qYrs, qMin);
    }
  }

  static void MainInterfaces()
  {
    var john = new Client {
        Name = "John Doe", Income = 40000, YearsInJob = 1,
        UsesCreditCard = true, CriminalRecord = false 
      };
    
    // Create instance & call method of the class
    var test = new CoefficientTest(0.001, 5.0, 50.0);
    test.PrintInfo();

    // Cast to the interface type & call interface method
    var cltest = (IClientTest)test;
    if (cltest.Test(john)) cltest.Report(john);
  }

  // --------------------------------------------------------------------------
  // Section 9.5 Using F# libraries from C#

  // Note: You have to add reference using 'Add Reference' first.
  // The 'using' directive is above.

  static void MainInterop() 
  {
    // Create an instance of the class
    var rc1 = new Rect(0.0f, 100.0f, 0.0f, 50.0f);

    // Invoke a functional member of the class
    var rc2 = rc1.Deflate(20.0f, 10.0f);

    // Prints '(10, 20) - (60, 30)'
    Console.WriteLine("({0}, {1}) - ({2}, {3})",
        rc2.Left, rc2.Top, rc2.Width, rc2.Height);
  }

  // --------------------------------------------------------------------------
  // Section 9.5.1 Working with values and delegates 

  static void MainFunctions()
  {
    // Use a value from 'Tests' module
    var client = Tests.John;

    // We can use lambda function!
    client = Tests.WithIncome(income => income + 5000, client);

    Console.WriteLine("{0} - {1}", client.Name, client.Income);
  }

  // --------------------------------------------------------------------------
  // Sample entrypoint - select the part you're interested in below

	static void Main(string[] args) {
    //MainInterfaces();
    //MainInterop();
    MainFunctions();
	}
}
