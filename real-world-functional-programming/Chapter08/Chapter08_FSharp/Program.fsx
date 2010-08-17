// --------------------------------------------------------------------------
// Functional Programming in .NET - Chapter 8
// --------------------------------------------------------------------------

// --------------------------------------------------------------------------
// Section 8.1.2 Using lists of functions in F#
// Listing 8.4 Client record type and sample value

// Declare 'Client' as an F# record type 
type Client =
  { Name : string
    Income : int
    YearsInJob : int
    UsesCreditCard : bool
    CriminalRecord : bool }

// Create a value of the 'Client' type
let john = 
  { Name = "John Doe"  
    Income = 40000
    YearsInJob = 1
    UsesCreditCard = true
    CriminalRecord = false }

// Listing 8.5 Executing tests (F# interactive)
module Listing_8_5 =

  // Create a list of tests
  let tests =
    [ (fun cl -> cl.CriminalRecord = true);
      (fun cl -> cl.Income < 30000);
      (fun cl -> cl.UsesCreditCard = false);
      (fun cl -> cl.YearsInJob < 2) ]

  let testClient(client) = 
    // Filter tests and get a list of issues
    let issues =  tests |> List.filter (fun f -> f client)
    // Count the issues and print the result
    let suitable = issues.Length <= 1
    printfn "Client: %s\nOffer a loan: %s (issues = %d)" client.Name
            (if (suitable) then "YES" else "NO") issues.Length
  
  testClient(john)

// --------------------------------------------------------------------------
// Section 8.2.2 Capturing state using closures in F#

module Section_8_2_2 =

  // Listing 8.7 Configurable income test using closures

  let createIncomeTest() =
    // Declare local mutable value
    let minimalIncome = ref 30000
    (fun (newMinimal) -> 
        // Set new minimal income
        minimalIncome := newMinimal),
    (fun (cl) -> 
        // Test client using the current minimal income
        cl.Income < (!minimalIncome))

  // Create functions for setting and testing income 
  let setMinimalIncome, testIncome = createIncomeTest()

  // Store testing function in a list
  let tests = 
    [ (fun client -> client.CriminalRecord = true);
      testIncome;
      (fun client -> client.UsesCreditCard = false);
      (fun client -> client.YearsInJob < 2) ]

  let testClient(client) = 
    let issues =  tests |> List.filter (fun f -> f client)
    printfn "Client: %s\nOffer a loan: %s (issues = %d)" client.Name
            (if (issues.Length <= 1) then "YES" else "NO") issues.Length
  
  // Listing 8.8 Changing minimal income during testing
  testClient(john)
  setMinimalIncome(45000)   
  testClient(john)
  
// --------------------------------------------------------------------------
// Section 8.3 Working with composed behaviors

// Section 8.3.1 Records of functions
type ClientTests = 
  { Check   : Client -> bool
    Report : Client -> unit }

// Listing 8.9 Creating tests with reporting

// Testing and reporting for criminal record
let testCriminal(client) = client.CriminalRecord = true
let reportCriminal(client) =
    printfn "'%s' has a criminal record!" client.Name

// Tests required minimal income
let testIncome(client) = client.Income < 30000
let reportIncome(client) =
    printfn "Income of '%s' is less than 30000!" client.Name

// Tests required years in the current job
let testJobYears(client) = client.YearsInJob < 2
let reportJobYears(client) =
    printfn "Years in the job of '%s' is less than 2!" client.Name

// Create a list of records
let testsWithReports =
  [ { Check = testCriminal; Report = reportCriminal };
    { Check = testIncome;   Report = reportIncome };
    { Check = testJobYears; Report = reportJobYears };
    (* more tests... *) ]


// Listing 8.10 Testing a client with reporting (F# interactive)
let testClientWithReports(client) =  
  // Get a list of tests that failed
  let issues =
    testsWithReports
    |> List.filter (fun tr -> tr.Check(client))
  
  // Calculate overall result
  let suitable = issues.Length <= 1

  // Report all found issues  
  for i in issues do i.Report(client)
  printfn "Offer loan: %s" (if (suitable) then "YES" else "NO")

// Modify the client, so that we get interesting error reports
testClientWithReports({ john with CriminalRecord = true })


// --------------------------------------------------------------------------
// Section 8.3.1 Building composed behaviors

// Listing 8.11 Creating similar tests using a single function
module Listing_8_11 =
  let lessThanTest f min property =
    // Nested reporting function 
    let report(cl) =
      printfn "%s of '%s' is only %d; less than %d." 
              property cl.Name (f(cl)) min
    { Check = 
        // Compare actual value with the minimal value
        (fun cl -> f(cl) < min)
      Report = report }

  // Creates two similar tests with reporting 
  let tests = 
    [ (lessThanTest (fun cl -> cl.Income) 30000 "Income")
      (lessThanTest (fun cl -> cl.YearsInJob) 2 "Years in the job")
      (* more tests... *) ]

  // Note: To test the code, you can scroll above, to re-execute
  // the 'testClientWithReports' function using the new tests..
  
// --------------------------------------------------------------------------
// Section 8.4 Combining data and behaviors

// Listing 8.12 Mutually recursive types describing decision tree

// Declare first type using 'type' keyword
type QueryInfo =
  { Title : string
    // Member representing the behavior
    Test : Client -> bool
    // References to the second type
    Positive : Decision
    Negative : Decision }
    
// Make the declaration recursive using 'and' keyword 
and Decision = 
  | Result of string  
  | Query of QueryInfo // Reference to the first type


// Listing 8.13 Decision tree for testing clients

// Root node on level 1
let rec tree = 
    Query({ Title = "More than $40k" 
            Test = (fun cl -> cl.Income > 40000)
            Positive = moreThan40; Negative = lessThan40 })
// First option on the level 2
and moreThan40 = 
    Query({ Title = "Has criminal record"
            Test = (fun cl -> cl.CriminalRecord)
            Positive = Result("NO"); Negative = Result("YES") })
// Second option on the level 2
and lessThan40 = 
    Query({ Title = "Years in job"
            Test = (fun cl -> cl.YearsInJob > 1)
            Positive = Result("YES"); Negative = usesCredit })
// Additional question on level 3
and usesCredit = 
    Query({ Title = "Uses credit card"
            Test = (fun cl -> cl.UsesCreditCard)
            Positive = Result("YES"); Negative = Result("NO") })


// Listing 8.14 Recursive processing of the decision tree

// Recursive function declaration
let rec testClientTree(client, tree) =
  match tree with
  | Result(msg) ->
      // The case with the final result
      printfn "  OFFER A LOAN: %s" msg
  | Query(qi) ->
      // The case containing a query
      let s, case = if (qi.Test(client)) then "yes", qi.Positive
                    else "no", qi.Negative
      printfn "  - %s? %s" qi.Title s
      // Recursive call on the selected sub-tree
      testClientTree(client, case)

// Test the code interactively
testClientTree(john, tree)
