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
#load @"Y:\src\Fing\Fing\FSharpTypes.fs"
#load @"Y:\src\Fing\Fing\CSharpTypes.fs"
#load @"Y:\src\Fing\Fing\ParsedTypes.fs"
#load @"Y:\src\Fing\Fing\Parser.fs"
#load @"Y:\src\Fing\Fing\Fing.fs"
// #load "Test.fs"
open Types
let test = Parser.parse >> Types.index
// TODO: Find out how to search for FParsec-local types
// esp difficult ones, like the Parser<char,'u> type synonym
// OK, turns out the problem is identifiers like
// FParsec.Primitives+ReplyStatus
// The parser dies on +
// I don't know what the right entry method is for this
// probably just ReplyStatusN
let parsec = Microsoft.FSharp.Metadata.FSharpAssembly.FromFile @"Y:/src/Fing/Fing/bin/Debug/FParsec.dll"
let ts = Seq.choose id (seq { 
  for e in parsec.Entities do
  for m in e.MembersOrValues do
  yield try Some {Fing.ent=e; Fing.mem=m; Fing.typ=FSharpTypes.cvt m.Type |> Types.index |> FSharpTypes.debinarize} 
        with _ -> None
})
let t = ts |> Seq.nth 35 |> Fing.tipe