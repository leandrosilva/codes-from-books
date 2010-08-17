using System;
using System.Collections.Generic;
using System.Linq;
using System.Text;

// --------------------------------------------------------------------------
// Functional Programming in .NET - Chapter 8
// --------------------------------------------------------------------------

namespace Chapter08_CSharp
{
  class Program
  {
    // --------------------------------------------------------------------------
    // Section 8.1 Using collections of behaviors

    class Client  {
		  public string Name { get; set; }
		  public int Income { get; set; }
		  public int YearsInJob { get; set; }
		  public bool UsesCreditCard { get; set; }
		  public bool CriminalRecord { get; set; }
	  }

    // Listing 8.1 Loan suitability tests using object oriented style
    interface IClientTest {
      // Method that tests the client
      bool Test(Client client);
    }
    
    // Each test is represented by a single class
    class TestYearsInJob : IClientTest {
      public bool Test(Client client) {
        // Body of the concrete test
        return client.YearsInJob < 2;
      }
    }

    // Listing 8.2 Loan suitability tests using a list of functions
    static List<Func<Client, bool>> GetTests() {
      // Returns a list of tests
      // Create new list using collection initializer
      return new List<Func<Client, bool>>() {
          // Several test checking loan suitability
          (cl => cl.CriminalRecord == true),
          (cl => cl.Income < 30000),
          (cl => cl.UsesCreditCard == false),
          (cl => cl.YearsInJob < 2)
        };
    }


    // --------------------------------------------------------------------------
    // Section 8.1.2 Executing behaviors in C#

    // Listing 8.3 Executing tests
    static void TestClient(List<Func<Client, bool>> tests, Client client) {
      // How many tests the client fails?
      int issuesCount = tests.Count(f => f(client));

      // Print the results of testing
      bool suitable   = issuesCount <= 1;
      Console.WriteLine("Client: {0}\nOffer a loan: {1}",
          client.Name, suitable ? "YES" : "NO");
    }

    static void MainCollections() 
    {
      // Create client using object initializer
      var john = new Client {
          Name = "John Doe", Income = 40000, YearsInJob = 1,
          UsesCreditCard = true, CriminalRecord = false 
        };

      // Offer a loan to the client?
      TestClient(GetTests(), john);
    }


    // --------------------------------------------------------------------------
    // Section 8.2.1 Command design pattern    

    // Listing 8.6 Income test using Command pattern 

    // Corresponds to the 'Receiver' class
    class IncomeTest {
      // Encapsulated mutable state of the receiver
      int minimalIncome;
      public IncomeTest() {
        minimalIncome = 30000;
      }
      // Modifies the mutable state
      public void SetMinimalIncome(int income) {
        minimalIncome = income;
      }
      // Operation used by the 'Command'
      public bool TestIncome(Client client)  {
        return client.Income < minimalIncome;
      }
    }

    static void MainCommand()
    {
      // List of tests & sample client
      var tests = GetTests();
      var john = new Client {
          Name = "John Doe", Income = 40000, YearsInJob = 1,
          UsesCreditCard = true, CriminalRecord = false 
        };
      
      // Create 'Receiver' with the state
      IncomeTest incomeTst = new IncomeTest();

      // Create the 'Command' as a lambda function
      Func<Client, bool> command1 =
          (cl) => incomeTst.TestIncome(cl);

      // Add command to the list of tests ('Invoker')
      // tests.Add(command)

      // We don't have to create lambda function explicitly:
      tests.Add(incomeTst.TestIncome);

      // Run some tests..
      TestClient(tests, john);
      incomeTst.SetMinimalIncome(45000);
      TestClient(tests, john);
    }

    // --------------------------------------------------------------------------
    // Section 8.4.2 Decision trees in C#

    // Listing 8.15 Object oriented decision tree (C#)
    
    abstract class Decision {
      // Tests the given client 
      public abstract void Evaluate(Client client);
    }

    class DecisionResult : Decision {
      public bool Result { get; set; }
      public override void Evaluate(Client client) {
        // Print the final result
        Console.WriteLine("OFFER A LOAN: {0}", Result ? "YES" : "NO");
      }
    }


    // Listing 8.16 Simplified implementation of Template method
    class DecisionQuery : Decision {
      public string Title { get; set; }
      public Decision Positive { get; set; }
      public Decision Negative { get; set; }
      // Primitive operation to be provided by the user
      public Func<Client, bool> Test { get; set; }

      public override void Evaluate(Client client) {
        // Test a client using the primitive operation
        bool res = Test(client);
        Console.WriteLine("  - {0}? {1}", Title, res ? "yes" : "no");
        // Select a branch to follow
        if (res) Positive.Evaluate(client);
        else Negative.Evaluate(client);
      }
    }

    static void MainDecisionTrees()
    {
      // The tree is constructed from a query
      var tree =
          new DecisionQuery
          {
            Title = "More than $40k",
            // Test is specified using a lambda function
            Test = (client) => client.Income > 40000,
            // Sub-trees can be 'DecisionResult' or 'DecisionQuery'
            Positive = new DecisionResult { Result = true },
            Negative = new DecisionResult { Result = false }
          };
    
      // Test a client using this tree
      // Create client using object initializer
      var john = new Client {
          Name = "John Doe", Income = 40000, YearsInJob = 1,
          UsesCreditCard = true, CriminalRecord = false 
        };
      tree.Evaluate(john);
    }
    // --------------------------------------------------------------------------
    // Entry-point: Select the example you're interested in

    private static void Main(string[] args)
		{
      //MainCollections();
      //MainCommand();
      MainDecisionTrees();
		}
	}
}
