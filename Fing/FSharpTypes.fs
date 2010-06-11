// Copyright (c) 2010, Nathan Sanders
// Licence: New BSD. See accompanying documentation.


/// code specific to types retrieved from FSharp.Powerpack.Metadata
module FSharpTypes
open Microsoft.FSharp.Metadata
open Types

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
(* Gets something stable from an FSharpEntity so that we can see if two are identical *)
let rec canonicalType (e:FSharpEntity) =
  if (e.IsAbbreviation) then
    canonicalType e.AbbreviatedType.NamedEntity
  else
    let t = e.DisplayName // e.ReflectionType |> string
    let denamespaced = t.Substring (t.IndexOf '+' + 1) // NOTE: denamespacing may not be
    denamespaced |> Seq.takeWhile ((<>) '`') // needed if I use DisplayName instead of
                 |> Array.ofSeq              // #ReflectionType >> string
                 |> (fun s -> new string(s))
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
TODO: cvtParam is not done, it really only does type defaulting as yet
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
    let id = e.NamedEntity |> canonicalType |> Id
    match e.GenericArguments |> Seq.map cvt |> List.ofSeq with
    | [] -> id
    | args -> Generic(id, args)
and cvtParam (param:FSharpGenericParameter) =
  if Seq.isEmpty param.Constraints then
    Var (Normal param.Name)
  else
    match param.Constraints |> Seq.tryFind (fun c -> c.IsDefaultsToConstraint && c.DefaultsToTarget.IsNamed) with
    | Some def -> def.DefaultsToTarget.NamedEntity |> canonicalType |> Id
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
