// Expert F# 2.0
// Chapter 17 Example 09

open System.Runtime.InteropServices

module CInterop =
    [<DllImport("CInteropDLL", CallingConvention=CallingConvention.Cdecl)>]
    extern void HelloWorld()

CInterop.HelloWorld()
