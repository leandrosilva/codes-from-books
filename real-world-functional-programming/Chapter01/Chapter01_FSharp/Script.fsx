#light
// --------------------------------------------------------------------------
// Functional Programming in .NET - Chapter 1
// --------------------------------------------------------------------------
// This file is a script and contains code that can be executed using
// F# interactive. To run a code, highlight it and hit Alt+Enter.
// The last listing from the chapter is in 'Program.fs' which is a 
// main file of standard F# project.
// --------------------------------------------------------------------------

// ------------------------------------------------------------------------
// Section 1.5.1 Hello world in F#

// Listing 1.10 Printing hello world (F#)

// Value binding for 'message'
let message = "Hello world!"
// Call to the 'printfn' function
printfn "%s" message


// ------------------------------------------------------------------------
// Section 1.5.2 From simplicity to the real world

// Figure 1.3 Using F# interactive we can first test the code and then wrap it into a function.

// Experiment with string concatenation
let name = "Tomas"
let str = "Hello " + name + "!"

// Wrap the code in a function
let sayHello(name) = 
   let str = "Hello " + name + "!"
   printfn "%s" str
