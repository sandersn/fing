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
let entite { ent = e } = e
let membre { mem = m } = m
let tipe { typ = t } = t
// ported from Haskell's Control.Monad.sequence
// uses foldBack, probably super SUPER inefficient
let listSequence ls =
  let k l l' = [for x in l do for xs in l' do yield x::xs]
  List.foldBack k ls [[]]
(*Probably not ready for prime-time (ie untested):
let seqSequence ss =
  let k s' s = seq {for x in s do for xs in s' do yield Seq.singleton x |> Seq.append <| xs}
  Seq.fold k (Seq.singleton Seq.empty) ss *)
let seqButLast l =
  let len = Seq.length l - 1
  Seq.take len l, Seq.skip len l |> Seq.head
let rec private matches t t'=
  match (t,t') with
  | Arrow ts, Arrow ts' -> matchAllOrders ts ts'
  | Tuple ts, Tuple ts' -> matchAllOrders ts ts'
  | Var v, Var v' -> v = v'
  | Id id, Id id' -> id = id'
  | NamedArg (_, t, isOpt), NamedArg(_, t', isOpt') -> isOpt = isOpt' && matches t t'
  | Generic(t, args), Generic(t', args') -> matches t t' 
                                            && List.length args = List.length args'
                                            && Seq.forall2 matches args args'
  | Array(n, t), Array(n', t') -> n = n' && matches t t'
  | Constraint(c,t), Constraint(c', t') -> c = c' && matches t t'
  | _ -> t = t'
  // TODO: Write matchesConstraint as well instead of punting
  // but right now the extra customisation is unneeded
  // TODO: Maybe also
  // | NamedArg(_, t, false), t' -> matches t t'
  // | t, NamedArg(_, t', false) -> matches t t'
and matchAllOrders ts ts' =
  // TODO: This could probably be sped up by using a MultiSet
  // then overriding equality for Typ.
  // assuming of course that MultiSet only relies on (=), it probably also uses hash.
  // hmf. Probably should profile before doing this, it's a major change
  // that might make things simpler, might not. So the main motivation would be efficiency.
  match (List.length ts - 1,List.length ts' - 1) with
  | 1,1 -> List.forall2 matches ts ts' // short-circuit one-arg functions
  | n,m when n=m -> 
    let args,ret = seqButLast ts
    let args',ret' = seqButLast ts'
    matches ret ret'
    && Seq.exists (Set.ofList >> Set.count >> ((=) n))
                  (listSequence [for x in args do 
                                 yield [for i,y in Seq.zip [0..n - 1] args' do
                                        if matches x y then yield i]])
  | _ -> false
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