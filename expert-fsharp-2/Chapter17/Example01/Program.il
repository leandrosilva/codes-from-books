
//  Microsoft (R) .NET Framework IL Disassembler.  Version 4.0.30319.1
//  Copyright (c) Microsoft Corporation.  All rights reserved.



// Metadata version: v4.0.30319
.assembly extern mscorlib
{
  .publickeytoken = (B7 7A 5C 56 19 34 E0 89 )                         // .z\V.4..
  .ver 4:0:0:0
}
.assembly extern FSharp.Core
{
  .publickeytoken = (B0 3F 5F 7F 11 D5 0A 3A )                         // .?_....:
  .ver 4:0:0:0
}
.assembly Example1701
{
  .custom instance void [FSharp.Core]Microsoft.FSharp.Core.FSharpInterfaceDataVersionAttribute::.ctor(int32,
                                                                                                      int32,
                                                                                                      int32) = ( 01 00 02 00 00 00 00 00 00 00 00 00 00 00 00 00 ) 
  .custom instance void [mscorlib]System.Runtime.Versioning.TargetFrameworkAttribute::.ctor(string) = ( 01 00 29 2E 4E 45 54 46 72 61 6D 65 77 6F 72 6B   // ..).NETFramework
                                                                                                        2C 56 65 72 73 69 6F 6E 3D 76 34 2E 30 2C 50 72   // ,Version=v4.0,Pr
                                                                                                        6F 66 69 6C 65 3D 43 6C 69 65 6E 74 01 00 54 0E   // ofile=Client..T.
                                                                                                        14 46 72 61 6D 65 77 6F 72 6B 44 69 73 70 6C 61   // .FrameworkDispla
                                                                                                        79 4E 61 6D 65 1F 2E 4E 45 54 20 46 72 61 6D 65   // yName..NET Frame
                                                                                                        77 6F 72 6B 20 34 20 43 6C 69 65 6E 74 20 50 72   // work 4 Client Pr
                                                                                                        6F 66 69 6C 65 )                                  // ofile

  // --- The following custom attribute is added automatically, do not uncomment -------
  //  .custom instance void [mscorlib]System.Diagnostics.DebuggableAttribute::.ctor(valuetype [mscorlib]System.Diagnostics.DebuggableAttribute/DebuggingModes) = ( 01 00 02 00 00 00 00 00 ) 

  .hash algorithm 0x00008004
  .ver 0:0:0:0
}
.mresource public FSharpSignatureData.Example1701
{
  // Offset: 0x00000000 Length: 0x000002E1
  // WARNING: managed resource file FSharpSignatureData.Example1701 created
}
.mresource public FSharpOptimizationData.Example1701
{
  // Offset: 0x000002E8 Length: 0x000000F5
  // WARNING: managed resource file FSharpOptimizationData.Example1701 created
}
.module Example1701.exe
// MVID: {4C14898F-B4C7-D41E-A745-03838F89144C}
.imagebase 0x00400000
.file alignment 0x00000200
.stackreserve 0x00100000
.subsystem 0x0002       // WINDOWS_GUI
.corflags 0x00000003    //  ILONLY 32BITREQUIRED
// Image base: 0x001F0000


// =============== CLASS MEMBERS DECLARATION ===================

.class public abstract auto ansi sealed Program
       extends [mscorlib]System.Object
{
  .custom instance void [FSharp.Core]Microsoft.FSharp.Core.CompilationMappingAttribute::.ctor(valuetype [FSharp.Core]Microsoft.FSharp.Core.SourceConstructFlags) = ( 01 00 07 00 00 00 00 00 ) 
  .method public specialname static int32 
          get_i() cil managed
  {
    .custom instance void [mscorlib]System.Runtime.CompilerServices.CompilerGeneratedAttribute::.ctor() = ( 01 00 00 00 ) 
    .custom instance void [mscorlib]System.Diagnostics.DebuggerNonUserCodeAttribute::.ctor() = ( 01 00 00 00 ) 
    // Code size       3 (0x3)
    .maxstack  3
    IL_0000:  nop
    IL_0001:  ldc.i4.2
    IL_0002:  ret
  } // end of method Program::get_i

  .method public specialname static int32 
          get_v() cil managed
  {
    // Code size       6 (0x6)
    .maxstack  4
    IL_0000:  ldsfld     int32 '<StartupCode$Example1701>'.$Program::v@7
    IL_0005:  ret
  } // end of method Program::get_v

  .property int32 i()
  {
    .get int32 Program::get_i()
  } // end of property Program::i
  .property int32 v()
  {
    .custom instance void [FSharp.Core]Microsoft.FSharp.Core.CompilationMappingAttribute::.ctor(valuetype [FSharp.Core]Microsoft.FSharp.Core.SourceConstructFlags) = ( 01 00 09 00 00 00 00 00 ) 
    .get int32 Program::get_v()
  } // end of property Program::v
} // end of class Program

.class private abstract auto ansi sealed '<StartupCode$Example1701>'.$Program
       extends [mscorlib]System.Object
{
  .field static assembly int32 v@7
  .custom instance void [mscorlib]System.Diagnostics.DebuggerBrowsableAttribute::.ctor(valuetype [mscorlib]System.Diagnostics.DebuggerBrowsableState) = ( 01 00 00 00 00 00 00 00 ) 
  .field static assembly int32 init@
  .custom instance void [mscorlib]System.Diagnostics.DebuggerBrowsableAttribute::.ctor(valuetype [mscorlib]System.Diagnostics.DebuggerBrowsableState) = ( 01 00 00 00 00 00 00 00 ) 
  .custom instance void [mscorlib]System.Runtime.CompilerServices.CompilerGeneratedAttribute::.ctor() = ( 01 00 00 00 ) 
  .custom instance void [mscorlib]System.Diagnostics.DebuggerNonUserCodeAttribute::.ctor() = ( 01 00 00 00 ) 
  .method public static void  main@() cil managed
  {
    .entrypoint
    // Code size       40 (0x28)
    .maxstack  4
    IL_0000:  nop
    IL_0001:  nop
    IL_0002:  ldstr      "Input a number:"
    IL_0007:  call       void [mscorlib]System.Console::WriteLine(string)
    IL_000c:  call       string [mscorlib]System.Console::ReadLine()
    IL_0011:  call       int32 [mscorlib]System.Int32::Parse(string)
    IL_0016:  stsfld     int32 '<StartupCode$Example1701>'.$Program::v@7
    IL_001b:  ldc.i4.2
    IL_001c:  call       int32 Program::get_v()
    IL_0021:  mul
    IL_0022:  call       void [mscorlib]System.Console::WriteLine(int32)
    IL_0027:  ret
  } // end of method $Program::main@

} // end of class '<StartupCode$Example1701>'.$Program

.class private abstract auto ansi sealed '<StartupCode$Example1701>.$.NETFramework,Version=v4.0,Profile=Client'.AssemblyAttributes
       extends [mscorlib]System.Object
{
} // end of class '<StartupCode$Example1701>.$.NETFramework,Version=v4.0,Profile=Client'.AssemblyAttributes


// =============================================================

// *********** DISASSEMBLY COMPLETE ***********************
// WARNING: Created Win32 resource file Program.res
