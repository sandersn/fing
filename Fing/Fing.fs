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
let private core : seq<Result> = 
  seq {
    for e in FSharpAssembly.FSharpLibrary.Entities do
    for m in e.MembersOrValues do
    yield try Some {ent=e; mem=m; typ=FSharpTypes.cvt m.Type |> index |> FSharpTypes.debinarize} 
          with _ -> None
  } |> Seq.choose id
let typeFind s =
  let ty = Parser.parse s |> index |> ParsedTypes.dealias
  core |> Seq.filter (tipe >> matcher ty)
let nameFind s = 
  core |> Seq.filter (fun {mem = m} -> m.DisplayName = s)
let search (s : string) =
  if s.Contains "->" then typeFind s else nameFind s
let textSearch s =
  printfn "%s" s
  printfn "Results:"
  Seq.iter (formatResult >> printfn "\t%s") (search s)
let debug (m : FSharpMemberOrVal) =
  FSharpTypes.cvt m.Type