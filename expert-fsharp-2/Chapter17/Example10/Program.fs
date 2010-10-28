// Expert F# 2.0
// Chapter 17 Example 10

open System.Runtime.InteropServices

module CInterop =
    [<Struct; StructLayout(LayoutKind.Sequential)>]
    type Complex =
        val mutable re:double
        val mutable im:double
        new(r,i) = { re = r; im = i; }

    [<DllImport("CInteropDLL", CallingConvention=CallingConvention.Cdecl)>]
    extern Complex SumC(Complex c1, Complex c2)

    [<DllImport("CInteropDLL", CallingConvention=CallingConvention.Cdecl)>]
    extern void ZeroC(Complex* c)

let c1 = CInterop.Complex(1.0, 0.0)
let c2 = CInterop.Complex(0.0, 1.0)

let mutable c3 = CInterop.SumC(c1, c2)
printf "c3 = SumC(c1, c2) = %f + %fi\n" c3.re c3.im;

let mutable c4 = CInterop.SumC(c1, c2)
printf "c4 = SumC(c1, c2) = %f + %fi\n" c4.re c4.im

CInterop.ZeroC(&&c4)
printf "c4 = %f + %fi\n" c4.re c4.im
