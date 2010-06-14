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
#load @"Y:\src\Fing\Fing\ParsedTypes.fs"
#load @"Y:\src\Fing\Fing\FSharpTypes.fs"
#load @"Y:\src\Fing\Fing\CSharpTypes.fs"
#load @"Y:\src\Fing\Fing\Parser.fs"
#load @"Y:\src\Fing\Fing\Fing.fs"
// #load "Test.fs"
open Types
// let core = Microsoft.FSharp.Metadata.FSharpAssembly.FSharpLibrary
let parsec = Microsoft.FSharp.Metadata.FSharpAssembly.FromFile "Y:/src/Fing/Fing/bin/Debug/FParsec.dll"
let ts = seq { // Seq.choose id (seq { 
  for e in parsec.Entities do
  for m in e.MembersOrValues do
  yield {Fing.ent=e; Fing.mem=m; Fing.typ=FSharpTypes.cvt m.Type |> Types.index |> FSharpTypes.debinarize} 
}
let rawts = seq {
  for e in parsec.Entities do
  for m in e.MembersOrValues do
  yield m
}
let indices = 1 |> Seq.unfold (fun i -> Some(i, i+1))
// look for the indices of things that fail in cvt
// TODO: Turn this into a test
Seq.zip indices rawts
 |> Seq.iter (fun (i,t) ->
  try
    (FSharpTypes.cvt t.Type) |> ignore
    printf "." 
  with _ ->
    printfn "%d: %s . %s" i t.LogicalEnclosingEntity.DisplayName t.DisplayName
 )
FParsec.Primitives.preturn
for i,t in Seq.zip indices ts do
  printfn "%d. %s : %s" i (t.mem.DisplayName) (format <| Fing.tipe t)
  