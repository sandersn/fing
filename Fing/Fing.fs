module Fing
open Microsoft.FSharp.Metadata
open Parser
open Types

(* an infinite stream of possible variable names - for nicely named de Bruijn indices *)
#nowarn "40" // INFINITE SEQ IS INFINITE
let rec names = // and that's OK.
  let letters = ['a' .. 'z'] |> List.map string
  seq {
    yield! letters
    for n in names do
      for l in letters do
        yield n + l
  }
// this is not really de Bruijn indexing but it is similar
// in particular, I use state to make multi-parameter things like Arrow and Tuple
// less painful to write, ie they are map[M] now instead of fold over tuples.
// also I don't use numbers, because I am wimpy and also I think it's a syntax error.
let rec index t = 
  let nextIndex =
    let names = names.GetEnumerator()
    names.MoveNext() |> ignore
    fun () ->
      let name = names.Current
      names.MoveNext() |> ignore // INFINITE SEQ.MoveNext is always true. Shocking!
      name
  let indices = ref Map.empty
  let rec update v =
    match Map.tryFind v indices.Value with
    | Some i -> i
    | None -> let i = 
                match v with
                | Normal _ | Anonymous -> Normal (nextIndex ())
                | Structural _ -> Structural (nextIndex ())
                | Choice vars -> Choice (List.map update vars)
              indices.Value <- Map.add v i indices.Value
              i
  let lookup v = Map.find v indices.Value
  let rec indexer = function
  | Var v -> Var (update v)
  | Arrow types -> Arrow (List.map indexer types)
  | Tuple types -> Tuple (List.map indexer types)
  | Id id -> Id id
  | NamedArg(n,t,opt) -> NamedArg(n, indexer t, opt)
  | Generic(t,types) -> Generic(indexer t, List.map indexer types)
  | Array(n,t) -> Array (n, indexer t)
  | Constraint(var,t) -> let t' = indexer t
                         Constraint(updateConstraint var, t')
  // updateConstraint is not finished and definitely not tested
  and updateConstraint = function
  | Null var -> Null (lookup var)
  | Struct var -> Struct (lookup var)
  | NotStruct var -> NotStruct (lookup var)
  | DefaultConstructor var -> DefaultConstructor (lookup var)
  // TODO: not sure indexer is the Right Thing .. 
  // even if it is, I might need to spawn a copy of indices good only in this scope
  | Enum(var,t) -> Enum(lookup var, indexer t)
  | Delegate(var,t,t') -> Delegate(lookup var, indexer t, indexer t')
  | Subtype(var,t) -> Subtype(lookup var, indexer t)
  | Sig(var,t,t',prop) -> Sig(lookup var, indexer t, indexer t',prop)
  | TyparConstraint t -> TyparConstraint (indexer t)
  indexer t
////////////
// 
let rec debinarize = function
| Arrow [t; Arrow ts] -> match debinarize (Arrow ts) with
                         | Arrow ts -> Arrow (debinarize t::ts)
                         | _ -> failwith "oh no"
| Arrow types -> Arrow (List.map debinarize types)
| Tuple types -> Tuple (List.map debinarize types)
| NamedArg(n,t,opt) -> NamedArg(n, debinarize t, opt)
| Generic(t,types) -> Generic(debinarize t, List.map debinarize types)
| Array(n,t) -> Array (n, debinarize t)
| Constraint(var,t) -> Constraint(var, debinarize t) // might need to pass through to var someday
| x -> x
let rec dealias = function
| Var v -> Var v
| Arrow types -> Arrow (List.map dealias types)
| Tuple types -> Tuple (List.map dealias types)
| NamedArg(n,t,opt) -> NamedArg(n, dealias t, opt)
| Generic(t,types) -> Generic(dealias t, List.map dealias types)
| Array(n,t) -> Array (n, dealias t)
| Constraint(var,t) -> Constraint(var, dealias t) // might need to pass through to var someday
| Id id when Map.containsKey id aliases -> Id (Map.find id aliases)
| Id id -> Id id

(* Gets something stable from an FSharpEntity so that we can see if two are identical *)
let rec canonicalType (e:FSharpEntity) =
  if (e.IsAbbreviation) then
    canonicalType e.AbbreviatedType.NamedEntity
  else
    e.ReflectionType
(* string -> System.Type *)
let lookupType stype = FSharpAssembly.FSharpLibrary.GetEntity stype |> canonicalType

let isArray (e:FSharpType) =
  let name = e.NamedEntity.DisplayName
  name.StartsWith "[" && name.EndsWith "]"
/// e must be the FSharpType of an array
let dimensions (e:FSharpType) = e.NamedEntity.DisplayName.Length - 1
let tryFindConstraint (param:FSharpGenericParameter) (p,f) =
  match param.Constraints |> Seq.tryFind p with
  | Some(x) -> Some (f x)
  | None -> None
let rec optionsum = function
| [] -> None
| Some(x)::xs -> Some(x)
| None::xs -> optionsum xs
(* FSharpType -> Typ, no longer a Ty<System.Type,string> 
TODO: This is obviously missing newer stuff, because abs comes in as
('T -> 'T), when the REPL instantiates it (int -> int), so I guess that I miss some
constraints in here somewhere
TODO: Namespacing (aliasing is handled via hardcoding)
*)
let rec cvt (e:FSharpType) =
  if e.IsTuple then
    Tuple (e.GenericArguments |> Seq.map cvt |> List.ofSeq)
  elif e.IsFunction then
      Arrow (e.GenericArguments |> Seq.map cvt |> List.ofSeq)
  elif e.IsGenericParameter then
    cvtParam e.GenericParameter // TODO: cvtParam is very much not done
  elif e |> isArray then // It only has in defaulting so far
    Array(dimensions e, e.GenericArguments |> Seq.map cvt |> Seq.head)
  else
    let id = e.NamedEntity |> canonicalType |> string |> Id
    match e.GenericArguments |> Seq.map cvt |> List.ofSeq with
    | [] -> id
    | args -> Generic(id, args)
and cvtParam (param:FSharpGenericParameter) =
  if Seq.isEmpty param.Constraints then
    Var (Normal param.Name)
  else
    match param.Constraints |> Seq.tryFind (fun c -> c.IsDefaultsToConstraint) with
    | Some def -> def.DefaultsToTarget.NamedEntity |> canonicalType |> string |> Id
    | None -> Var (Normal param.Name)
    // param.Constraints |> Seq.map whenify |> Seq.fold SOMETHING param
and whenify (param:FSharpGenericParameter) (con:FSharpGenericParameterConstraint) =
  // NOTE: This is missing several important (but non-syntactic) kinds of constraints
  // particuarly the defaults constraint
  // Also: I have no way to compose whenify's results  in cvtParam. Most types with constraints
  // have multiple constraints, and these are also duplicated for each occurrence of the type
  if con.IsSupportsNullConstraint then
    Null (Normal param.Name)
  elif con.IsReferenceTypeConstraint then
    NotStruct (Normal param.Name)
  elif con.IsNonNullableValueTypeConstraint then
    Struct (Normal param.Name)
  elif con.IsRequiresDefaultConstructorConstraint then
    DefaultConstructor (Normal param.Name)
  elif con.IsEnumConstraint then
    Enum(Normal param.Name, cvt con.EnumConstraintTarget)
  elif con.IsDelegateConstraint then
    Delegate(Normal param.Name, cvt con.DelegateTupledArgumentType, cvt con.DelegateReturnType)
  elif con.IsCoercesToConstraint then
    Subtype(Normal param.Name, cvt con.CoercesToTarget)
  elif con.IsMemberConstraint then
    Sig(Structural param.Name,
        Id con.MemberName,
        Arrow (List.ofSeq (Seq.map cvt con.MemberArgumentTypes) @ [cvt con.MemberReturnType]), 
        Function)
  else
    failwith "basically this style of data structure just sucks"

type Result = {
  ent : FSharpEntity
  mem : FSharpMemberOrVal
  typ : Typ
}
let entite { ent = e } = e
let membre { mem = m } = m
let tipe { typ = t } = t
let matcher ty ty' =
  ty = ty' // && (ty <=> ty').IsSome
let formatResult { ent = e; mem = m; typ = t } = 
  sprintf "%s.%s\t\t%s" e.DisplayName m.DisplayName (format t)
let find s =
  let ty = parse s |> index |> dealias
  seq {
    for e in FSharpAssembly.FSharpLibrary.Entities do
    for m in e.MembersOrValues do
    yield try Some {ent=e; mem=m; typ=cvt m.Type |> index |> debinarize} 
          with _ -> None
  } |> Seq.choose id |> Seq.filter (tipe >> matcher ty) |> Seq.map formatResult
let textFind s =
  printfn "%s" s
  printfn "Results:"
  Seq.iter (printfn "\t%s") (find s)
let nameFind s = seq {
  for e in FSharpAssembly.FSharpLibrary.Entities do
  for m in e.MembersOrValues do
  if m.DisplayName = s then
    yield m //(try Some(index <| cvt m.Type) with _ -> None)
}
let debug (m : FSharpMemberOrVal) =
  cvt m.Type