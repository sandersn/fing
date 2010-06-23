// Copyright (c) 2010, Nathan Sanders
// Licence: New BSD. See accompanying documentation.

module Fing
open Microsoft.FSharp.Metadata
open Types
open Util
open Search

type Result = {
  ent : FSharpEntity
  mem : FSharpMemberOrVal
  typ : Typ
}
let entite { ent = e } = e
let membre { mem = m } = m
let tipe { typ = t } = t
let private formatResult { ent = e; mem = m; typ = t } = 
  sprintf "%s.%s\t\t%s" e.DisplayName m.DisplayName (format t)
// TODO: Cache this on disk or something
let mutable private assemblies : Set<string> = Set.empty
let mutable private types : list<Result> = []
let private updateReferences (refs : seq<FSharpAssembly>) =
  types <- 
    [for ref in Seq.append (Seq.singleton FSharpAssembly.FSharpLibrary) refs do
     for e in ref.Entities do
     for m in e.MembersOrValues do
     yield {ent=e; mem=m; typ=FSharpTypes.cvt m.Type |> index |> FSharpTypes.debinarize} ]
let addReferences news =
  assemblies <- assemblies |>Set.union<| set news
  let optionAssembly assembly =
    try
      FSharpAssembly.FromFile assembly |> Some
    with
      | :? System.IO.FileNotFoundException -> printfn "%s was not found" assembly; None
      // Indicates a C# assembly, someday I'll handle this
      | :? System.ArgumentException -> printfn "%s is not an F# assembly" assembly; None 
  updateReferences (Seq.choose id (Seq.map optionAssembly assemblies))
do addReferences []
// Public interface
// (other functions aren't private yet because it's so inconvenient;
// probably I should just move everything else to another module.)
let typeFind s =
  let ty = Parser.parse s |> index |> ParsedTypes.dealias
  types |> Seq.filter (tipe >> matches ty)
let nameFind s = 
  types |> Seq.filter (fun {mem = m} -> m.DisplayName = s)
let search (s : string) =
  if s.Contains "->" then typeFind s else nameFind s
let textSearch s =
  printfn "%s" s
  printfn "Results:"
  Seq.iter (formatResult >> printfn "\t%s") (search s)
let debug t t' = matches t t'