// This file is a script that can be executed with the F# Interactive.  
// It can be used to explore and test the library project.
// Note that script files will not be part of the project build.

#r @"C:\Program Files\FSharpPowerPack-2.0.0.0\\bin\FSharp.PowerPack.Metadata.dll"
#I @"Y:/src/Fing/Fing/bin/Debug/"
//#r @"Y:/src/Fing/fparsec/main/Build/VS9/bin/Debug/FParsec.dll"
#r @"FParsec.dll"
#r @"FParsecCS.dll"
//#r "Y:/src/Fing/fparsec/main/Build/VS9/bin/Debug/FParsecCS.dll"
#load @"Y:\src\Fing\Fing\Types.fs"
#load @"Y:\src\Fing\Fing\Parser.fs"
#load @"Y:\src\Fing\Fing\Fing.fs"
// #load "Test.fs"
open Types
let test = Parser.parse >> Fing.index
let at = Fing.debug (Seq.head (Fing.nameFind "abs"))
