// Copyright (c) 2010, Nathan Sanders
// Licence: New BSD. See accompanying documentation.

module Fing
open Microsoft.FSharp.Metadata
open Types

type Result = {
  ent : FSharpEntity
  mem : FSharpMemberOrVal
  typ : Typ
}
// TODO: Find out a better way to do higher-order accessors
let entite { ent = e } = e
let membre { mem = m } = m
let tipe { typ = t } = t

let private matcher ty ty' =
  ty = ty' // || (ty <=> ty').IsSome
let private formatResult { ent = e; mem = m; typ = t } = 
  sprintf "%s.%s\t\t%s" e.DisplayName m.DisplayName (format t)
// TODO: Cache this on disk or something
let mutable private assemblies : Set<string> = Set.empty
let mutable private types : seq<Result> = Seq.empty
let private updateReferences (refs : seq<FSharpAssembly>) =
  types <- seq {
    for ref in Seq.append (Seq.singleton FSharpAssembly.FSharpLibrary) refs do
    for e in ref.Entities do
    for m in e.MembersOrValues do
    yield try Some {ent=e; mem=m; typ=FSharpTypes.cvt m.Type |> index |> FSharpTypes.debinarize} 
          with _ -> None
  } |> Seq.choose id
let addReferences news =
  assemblies <- assemblies |>Set.union<| set news
  let optionAssembly assemby =
    try
      FSharpAssembly.FromFile assemby |> Some
    with
      | :? System.IO.FileNotFoundException -> None
      | :? System.ArgumentException -> None // Indicates a C# assembly, someday I'll handle this
  updateReferences (Seq.choose id (Seq.map optionAssembly assemblies))
let typeFind s =
  let ty = Parser.parse s |> index |> ParsedTypes.dealias
  types |> Seq.filter (tipe >> matcher ty)
let nameFind s = 
  types |> Seq.filter (fun {mem = m} -> m.DisplayName = s)
let search (s : string) =
  if s.Contains "->" then typeFind s else nameFind s
let textSearch s =
  printfn "%s" s
  printfn "Results:"
  Seq.iter (formatResult >> printfn "\t%s") (search s)
let debug (m : FSharpMemberOrVal) =
  FParsec.Internals.concat3 |> ignore
  FSharpTypes.cvt m.Type
