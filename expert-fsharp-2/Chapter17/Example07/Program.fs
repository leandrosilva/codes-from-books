// Expert F# 2.0
// Chapter 17 Example 07

namespace HelloWorld

open System
open System.Runtime.InteropServices

[<ProgId("Hwfs.FSComponent")>]
type FSCOMComponent() =
    member x.HelloWorld() = "Hello world from F#!"
