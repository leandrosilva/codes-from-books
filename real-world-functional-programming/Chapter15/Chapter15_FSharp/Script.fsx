// --------------------------------------------------------------------------
// Functional Programming in .NET - Chapter 15
// --------------------------------------------------------------------------

// Listing 15.1 Specifying animations using discriminated unions (F#)
open System.Drawing

// Specifies movement of an object
type AnimatedLocation =
   | Static of PointF 
   | Rotating of PointF * float32 * float32
   | Linear of PointF * PointF

// Type of values that represent animations
type Animation =
   | Disc of Brush * int * AnimatedLocation
   | Compose of Animation * Animation 

// Sample animation value
let animation =
   Compose(
      Disc(Brushes.Green, 50, Rotating(PointF(0.f,0.f), 100.f,2.f)), 
      Disc(Brushes.Red, 20, Linear(PointF(-100.f,0.f), PointF(100.f,0.f)))
   )

// --------------------------------------------------------------------------
// Section 15.6 Developing financial modeling language

open System

// Listing 15.23 Type representing financial contracts

// Contract can calculate it's trades
type Contract = 
  | ContractFunc of (DateTime -> seq<string * int>)

// Gets a list of trades at particular date
let eval (ContractFunc f) dt = f(dt) |> List.ofSeq


// --------------------------------------------------------------------------
// Listing 15.24 Combinators for creating and composing contracts

// Single trade of specified number of stocks
let trade what amount = ContractFunc(fun _ -> 
  seq { yield what, amount })

// Concatenate trades of two contracts
let combine (ContractFunc a) (ContractFunc b) = ContractFunc(fun now ->
  Seq.concat [ a(now); b(now) ])
  
// Limit the date when contract is active
let after dt (ContractFunc f) = ContractFunc(fun now -> 
  seq { if now >= dt then 
          yield! f(now) })
let until dt (ContractFunc f) = ContractFunc(fun now ->
  seq { if now <= dt then
          yield! f(now) })

// Change sale to purchase and conversely
let sell (ContractFunc f) = ContractFunc(fun now ->
  seq { for itm, am in f(now) -> itm, -am })
  
  
// Listing 15.25 Implementing derived financial contract functions
  
let between dateFrom dateTo contract = 
  after dateFrom (until dateTo contract)
  
let tradeAt date what amount = 
  after date (until date (trade what amount ))

// Listing 15.26 Creating and evaluating sample contract
  
let msft = (trade "MSFT" 1000)
let itstocks =   
  combine (sell (tradeAt (DateTime(2009, 4, 15)) "GOOG" 500))
          (between (DateTime(2009, 4, 10)) (DateTime(2009, 4, 20)) msft)

eval itstocks (DateTime(2009, 4, 14))
eval itstocks (DateTime(2009, 4, 15))

// --------------------------------------------------------------------------
// Sidebar: Representing contracts as abstract values

type Contract1 = 
  | Exchange of int * string
  | After of DateTime * Contract1
  | Until of DateTime * Contract1
  | Combine of Contract1 * Contract1

let tradeAt1 date amount what = 
  After(date, Until(date, Exchange(amount, what)))
