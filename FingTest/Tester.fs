// Copyright (c) 2010, Nathan Sanders
// Licence: New BSD. See accompanying documentation.

module Tester
open Types
open NUnit.Framework
let safezip l1 l2 = 
  let len = min (List.length l1) (List.length l2)
  Seq.zip (Seq.take len l1) (Seq.take len l2)
let sprints l = Seq.map (sprintf "%A") l |> String.concat ","
let failures = [
 "'a<'a>" // I don't know if this is legal or not.
 "'a<'b>" // I .. uh .. think so?
 "list<'a<'b>>"
 "'a 'a"  // same but with stupid suffix type
 ]
let passresults =
 [Id "int"
 ;Arrow [Id "int"; Id "int"]
 ;Arrow [Id "int"; Id "int"; Id "int"]
 ;Var (Normal "a")
 ;Tuple [Id "int"; Id "int"]
 ;Arrow [Id "int"; Tuple [Id "int"; Id "int"]; Id "int"]
 ;Arrow [Tuple [Id "int"; Id "int"]; Tuple [Id "int"; Id "int"]]
 ;Arrow [Var (Normal "a"); Var (Normal "a")]
 ;Tuple [Var (Normal "a"); Var (Normal "a")]
 ;Tuple [Var Anonymous; Var (Structural "a")]
 ;Id "int"
 ;Arrow [Id "int"; Id "int"]
 ;Arrow [Var (Normal "a"); Var (Normal "a")]
 ;Arrow
  [Tuple [Var (Normal "a"); Var Anonymous];
   Arrow [Id "int"; Var (Structural "a")]]
 ;Id "int"
 ;Id "Microsoft.FSharp.Core.double"
 ;Id "Microsoft.FSharp.Core.list`1"
 ;Id "list"
 ;Generic (Id "list",[])
 ;Generic (Id "list",[Id "int"])
 ;Generic (Id "list",[Arrow [Id "int"; Id "int"]])
 ;Generic (Id "list",[Tuple [Id "int"; Id "int"]])
 ;Generic
  (Id "list",[Tuple [Id "int"; Arrow [Id "int"; Tuple [Id "int"; Id "int"]]]])
 ;Generic (Id "list",[Generic (Id "list",[Id "int"])])
 ;Generic (Id "list",[Var Anonymous])
 ;Generic (Id "list",[Var Anonymous; Var Anonymous])
 ;Generic (Id "list",[Id "int"; Var (Normal "a"); Var Anonymous])
 ;Generic (Id "lazy",[Id "int"])
 ;Generic (Id "lazy",[Arrow [Id "int"; Id "int"]])
 ;Generic (Id "lazy",[Arrow [Var (Normal "a"); Var (Normal "a")]])
 ;Arrow
  [Tuple [Var (Normal "a"); Var Anonymous];
   Generic (Id "lazy",[Arrow [Id "int"; Var (Structural "a")]])]
 ;Generic
  (Id "lazy",
   [Arrow
      [Tuple [Var (Normal "a"); Var Anonymous];
       Arrow [Id "int"; Var (Structural "a")]]])
 ;Generic (Id "lazy",[Id "int"])
 ;Generic (Id "lazy",[Id "Microsoft.FSharp.Core.double"])
 ;Generic (Id "lazy",[Id "Microsoft.FSharp.Core.list`1"])
 ;Generic (Id "lazy",[Id "list"])
 ;Generic (Id "lazy",[Generic (Id "list",[Var (Normal "a")])])
 ;Generic (Id "lazy",[Generic (Id "lazy",[Id "list"])])
 ;Generic (Id "list",[Id "int"])
 ;Generic (Id "lazy",[Generic (Id "list",[Id "int"])])
 ;Generic (Id "list",[Generic (Id "list",[Id "int"])])
 ;Generic (Id "lazy",[Generic (Id "list",[Generic (Id "list",[Id "int"])])])
 ;Array (1,Id "int")
 ;Array (2,Id "int")
 ;Array (8,Id "int")
 ;Generic (Id "lazy",[Array (2,Id "int")])
 ;Generic (Id "lazy",[Array (3,Id "int")])
 ;Constraint (Null (Normal "a"),Generic (Id "list",[Var (Normal "a")]))
 ;Generic
  (Id "lazy",
   [Constraint (Null (Normal "a"),Generic (Id "list",[Var (Normal "a")]))])
 ;Constraint
  (Null (Normal "a"),
   Generic (Id "lazy",[Generic (Id "list",[Var (Normal "a")])]))
 ;Constraint
  (DefaultConstructor (Normal "a"),Generic (Id "list",[Var (Normal "a")]))
 ;Constraint (Struct (Normal "a"),Generic (Id "list",[Var (Normal "a")]))
 ;Constraint (NotStruct (Normal "a"),Generic (Id "list",[Var (Normal "a")]))
 ;Constraint (Enum (Normal "a",Id "int"),Generic (Id "list",[Var (Normal "a")]))
 ;Constraint
  (Delegate (Normal "a",Id "int",Var (Normal "a")),
   Generic (Id "list",[Var (Normal "a")]))
 ;Constraint
  (Subtype (Normal "a",Id "Microsoft.Collections.IComparable"),
   Generic (Id "list",[Var (Normal "a")]))
 ;Constraint
  (Subtype (Normal "a",Var (Normal "b")),
   Generic (Id "list",[Var (Normal "a"); Var (Normal "b")]))
 ;Constraint
  (Sig (Normal "a",Id "read",Arrow [Id "string"; Id "int"],Get),
   Generic (Id "list",[Var (Normal "a")]))
 ;Constraint
  (Sig (Normal "a",Id "read",Arrow [Id "string"; Id "int"],Set),
   Generic (Id "list",[Var (Normal "a")]))
 ;Constraint
  (Sig (Normal "a",Id "read",Arrow [Id "string"; Id "int"],GetSet),
   Generic (Id "list",[Var (Normal "a")]))
 ;Constraint
  (Sig (Normal "a",Id "read",Arrow [Id "string"; Id "int"],GetSet),
   Generic (Id "list",[Var (Normal "a")]))
 ;Constraint
  (Sig
     (Choice [Normal "a"; Normal "b"],Id "read",
      Arrow [Id "string"; Id "int"; Id "stream"],Function),
   Generic (Id "list",[Var (Normal "a")]))
 ;Constraint
  (Sig
     (Normal "a",Generic (Id "read",[Var (Normal "b"); Var (Normal "c")]),
      Arrow [Var (Normal "a"); Var (Normal "b"); Var (Normal "c")],Function),
   Generic (Id "list",[Var (Normal "a")]))
 ;Constraint
  (Sig
     (Normal "a",Id "read",Arrow [Id "stream"; Id "int"; Id "string"],Function),
   Generic (Id "list",[Var (Normal "a")]))
 ;Constraint
  (Sig
     (Normal "a",Id "read",
      Arrow [Id "stream"; Generic (Id "list",[Id "int"]); Id "string"],Function),
   Generic (Id "list",[Var (Normal "a")]))
 ;Constraint
  (Sig
     (Normal "a",Id "read",
      Arrow [Id "stream"; Generic (Id "lazy",[Id "int"]); Id "string"],Function),
   Generic (Id "list",[Var (Normal "a")]))
 ;Constraint
  (Sig
     (Normal "a",Id "read",
      Arrow [Id "stream"; Generic (Id "list",[Id "int"]); Id "string"],Function),
   Generic (Id "list",[Var (Normal "a")]))
 ;Constraint
  (Sig
     (Normal "a",Id "read",Arrow [Id "stream"; Array (1,Id "int"); Id "string"],
      Function),Generic (Id "list",[Var (Normal "a")]))
 ;Constraint
  (Sig
     (Normal "a",Id "read",Arrow [Id "stream"; Array (3,Id "int"); Id "string"],
      Function),Generic (Id "list",[Var (Normal "a")]))
 ;Constraint
  (Sig
     (Normal "a",Id "read",Arrow [Id "stream"; Var (Normal "a"); Id "string"],
      Function),Generic (Id "list",[Var (Normal "a")]))
 ;Constraint
  (Sig
     (Normal "a",Id "read",
      Arrow
        [Id "stream";
         Constraint
           (Sig (Normal "a",Id "read",Arrow [Id "int"; Id "string"],Function),
            Var (Normal "a")); Id "string"],Function),
   Generic (Id "list",[Var (Normal "a")]))
 ;Constraint
  (Sig
     (Normal "a",Id "read",Arrow [Tuple [Id "string"; Id "stream"]; Id "int"],
      Function),Generic (Id "list",[Var (Normal "a")]))
 ;Constraint
  (Sig
     (Normal "a",Id "read",Arrow [NamedArg ("foo",Id "int",true); Id "int"],
      Function),Generic (Id "list",[Var (Normal "a")]))
 ;Constraint
  (Sig
     (Normal "a",Id "read",
      Arrow
        [NamedArg ("foo",Id "int",true); NamedArg ("bar",Id "int",false);
         Id "int"],Function),Generic (Id "list",[Var (Normal "a")]))
 ;Constraint
  (Sig
     (Normal "a",Generic (Id "read",[Var (Normal "a")]),
      Arrow [Var (Normal "a"); Var (Normal "b")],Function),
   Generic (Id "list",[Var (Normal "a")]))
 ;Constraint
  (Sig
     (Normal "a",Generic (Id "read",[Var (Normal "a"); Var (Normal "b")]),
      Arrow [Var (Normal "a"); Var (Normal "b")],Function),
   Generic (Id "list",[Var (Normal "a")]))
 ;Constraint
  (Sig
     (Normal "a",
      Constraint
        (TyparConstraint
           (Constraint (Null (Normal "a"),Var (Choice [Normal "a"; Normal "b"]))),
         Generic (Id "read",[Var (Normal "a"); Var (Normal "b")])),
      Arrow [Var (Normal "a"); Var (Normal "b")],Function),
   Generic (Id "list",[Var (Normal "a")]))
 ;Constraint
  (Sig
     (Normal "a",
      Constraint
        (TyparConstraint
           (Constraint
              (DefaultConstructor (Normal "a"),
               Var (Choice [Normal "a"; Normal "b"]))),
         Generic (Id "read",[Var (Normal "a"); Var (Normal "b")])),
      Arrow [Var (Normal "a"); Var (Normal "b")],Function),
   Generic (Id "list",[Var (Normal "a")]))
 ;Constraint
  (Sig
     (Normal "a",
      Constraint
        (TyparConstraint
           (Constraint
              (Struct (Normal "a"),Var (Choice [Normal "a"; Normal "b"]))),
         Generic (Id "read",[Var (Normal "a"); Var (Normal "b")])),
      Arrow [Var (Normal "a"); Var (Normal "b")],Function),
   Generic (Id "list",[Var (Normal "a")]))
 ;Constraint
  (Sig
     (Normal "a",
      Constraint
        (TyparConstraint
           (Constraint
              (NotStruct (Normal "a"),Var (Choice [Normal "a"; Normal "b"]))),
         Generic (Id "read",[Var (Normal "a"); Var (Normal "b")])),
      Arrow [Var (Normal "a"); Var (Normal "b")],Function),
   Generic (Id "list",[Var (Normal "a")]))
 ;Constraint
  (Sig
     (Normal "a",
      Constraint
        (TyparConstraint
           (Constraint
              (Enum (Normal "a",Id "int"),Var (Choice [Normal "a"; Normal "b"]))),
         Generic (Id "read",[Var (Normal "a"); Var (Normal "b")])),
      Arrow [Var (Normal "a"); Var (Normal "b")],Function),
   Generic (Id "list",[Var (Normal "a")]))
 ;Constraint
  (Sig
     (Normal "a",
      Constraint
        (TyparConstraint
           (Constraint
              (Enum (Normal "a",Generic (Id "lazy",[Id "int"])),
               Var (Choice [Normal "a"; Normal "b"]))),
         Generic (Id "read",[Var (Normal "a"); Var (Normal "b")])),
      Arrow [Var (Normal "a"); Var (Normal "b")],Function),
   Generic (Id "list",[Var (Normal "a")]))
 ;Constraint
  (Sig
     (Normal "a",
      Constraint
        (TyparConstraint
           (Constraint
              (Delegate (Normal "a",Id "int",Id "dword"),
               Var (Choice [Normal "a"; Normal "b"]))),
         Generic (Id "read",[Var (Normal "a"); Var (Normal "b")])),
      Arrow [Var (Normal "a"); Var (Normal "b")],Function),
   Generic (Id "list",[Var (Normal "a")]))
 ;Constraint
  (Sig
     (Normal "a",
      Constraint
        (TyparConstraint
           (Constraint
              (Sig (Normal "a",Id "write",Arrow [Id "int"; Id "baz"],Function),
               Var (Choice [Normal "a"; Normal "b"]))),
         Generic (Id "read",[Var (Normal "a"); Var (Normal "b")])),
      Arrow [Var (Normal "a"); Var (Normal "b")],Function),
   Generic (Id "list",[Var (Normal "a")]))
 ;Constraint
  (Sig
     (Normal "a",
      Constraint
        (TyparConstraint
           (Constraint
              (Sig
                 (Normal "a",Id "write",
                  Arrow
                    [NamedArg ("foo",Id "int",false);
                     NamedArg ("bar",Id "baz",false)],Function),
               Var (Choice [Normal "a"; Normal "b"]))),
         Generic (Id "read",[Var (Normal "a"); Var (Normal "b")])),
      Arrow [Var (Normal "a"); Var (Normal "b")],Function),
   Generic (Id "list",[Var (Normal "a")]))
 ;Constraint
  (Sig
     (Normal "a",
      Constraint
        (TyparConstraint
           (Constraint
              (Subtype (Normal "a",Id "int"),
               Var (Choice [Normal "a"; Normal "b"]))),
         Generic (Id "read",[Var (Normal "a"); Var (Normal "b")])),
      Arrow [Var (Normal "a"); Var (Normal "b")],Function),
   Generic (Id "list",[Var (Normal "a")]))
 ;Constraint
  (Sig
     (Normal "a",
      Constraint
        (TyparConstraint
           (Constraint
              (Subtype (Normal "a",Generic (Id "lazy",[Id "int"])),
               Var (Choice [Normal "a"; Normal "b"]))),
         Generic (Id "read",[Var (Normal "a"); Var (Normal "b")])),
      Arrow [Var (Normal "a"); Var (Normal "b")],Function),
   Generic (Id "list",[Var (Normal "a")]))
 ;Constraint
  (Sig
     (Normal "a",
      Constraint
        (TyparConstraint
           (Constraint
              (Null (Normal "b"),
               Constraint
                 (Null (Normal "a"),Var (Choice [Normal "a"; Normal "b"])))),
         Generic (Id "read",[Var (Normal "a"); Var (Normal "b")])),
      Arrow [Var (Normal "a"); Var (Normal "b")],Function),
   Generic (Id "list",[Var (Normal "a")]))
 ;Constraint
  (Sig
     (Normal "a",
      Constraint
        (TyparConstraint
           (Constraint
              (Null (Normal "a"),Constraint (Null (Normal "a"),Var (Normal "a")))),
         Generic (Id "read",[Var (Normal "a")])),
      Arrow [Var (Normal "a"); Var (Normal "b")],Function),
   Generic (Id "list",[Var (Normal "a")]))
 ;Constraint
  (Sig
     (Normal "a",
      Constraint
        (TyparConstraint
           (Constraint
              (Subtype (Normal "a",Var (Normal "b")),
               Var (Choice [Normal "a"; Normal "b"]))),
         Generic (Id "read",[Var (Normal "a"); Var (Normal "b")])),
      Arrow [Var (Normal "a"); Var (Normal "b")],Function),
   Generic (Id "list",[Var (Normal "a")]))
 ;Constraint
  (Sig
     (Normal "a",
      Constraint
        (TyparConstraint
           (Constraint
              (Subtype (Normal "a",Var (Normal "b")),
               Constraint
                 (Null (Normal "a"),Var (Choice [Normal "a"; Normal "b"])))),
         Generic (Id "read",[Var (Normal "a"); Var (Normal "b")])),
      Arrow [Var (Normal "a"); Var (Normal "b")],Function),
   Generic (Id "list",[Var (Normal "a")]))
 ;Constraint
  (Sig
     (Normal "a",
      Constraint
        (TyparConstraint
           (Constraint
              (Subtype (Normal "a",Var (Normal "b")),
               Constraint (Null (Normal "a"),Var (Normal "a")))),
         Generic (Id "read",[Var (Normal "a")])),
      Arrow [Var (Normal "a"); Var (Normal "b")],Function),
   Generic (Id "list",[Var (Normal "a")]))]
let usedVarResults = 
  let e = Set.empty
  let a = Set.singleton (Normal "a")
  let _a = Set.ofList [Anonymous;Normal "a"]
  let ab = Set.ofList [Normal "a";Normal "b"]
  let _u = Set.singleton Anonymous
  [e;e;e; a; e;e;e; 
   a;a; Set.add (Structural "a") _u; e;e; a; 
   Set.add (Structural "a") _a; e; e; e; e; e; e; e; e; e; e
   _u; _u; _a; e; e; a; Set.add (Structural "a") _a; Set.add (Structural "a") _a; 
   e; e; e; e; a; e;e;e;e;e;e;e;e;e;e;
   a;a;a;a;a;a;a;a;a;
   ab;a;a;a;a;ab; Set.add (Normal "c") ab
   a;a;a;a;a;a;a;a;a;a;a;
   ab;ab;ab;ab;ab;ab;ab;ab;ab;ab;ab;ab;ab;ab;ab;ab;ab]
let substs =
  [Var (Normal "a")
  ;Var (Structural "a")
  ;Var Anonymous
  ;Var (Choice [Normal "a"; Normal "b"; Structural "c"; Anonymous])
  ;Id "foo"
  ;NamedArg("foo", Id "string", false)
  ;Generic(Id "list",[Var (Normal "a")])
  ;Generic(Id "list",[Var (Normal "a");Var(Normal "b")])
  ;Array(8,Id "int")
  ;Array(8,Generic(Id "list",[Var (Normal "a")]))
  ;Constraint( Null (Normal "a"), Id "foo")
  ;Constraint( Null (Normal "a"), Generic(Id "list",[Var (Normal "a")]))
  ]
// it is an error to have a Choice [typar1;typar2;typar3] without typars 1 2 and 3 being
// bound by a containing generic type
// there are probably lots of other errors like this one (basically anything in a constraint)
let rec unboundVars env : Typ -> option<Set<Typar>> = function
| Arrow ts -> try
                Some <| Seq.pick (unboundVars env) ts // this has to catch KeyNotFoundException?? arghh
              with | :? System.Collections.Generic.KeyNotFoundException -> None
| Var (Choice vars) -> 
  match Set.difference (Set.ofList vars) env with
  | vs when vs = Set.empty -> None
  | unbounds -> Some unbounds
| _ -> None
[<TestFixture>] 
type public Tester() =
  [<Test>]
  member public this.ParseTest() = 
    Assert.AreEqual(List.length Main.passes, List.length passresults)
    List.zip passresults (List.map Parser.parse Main.passes) |> List.iter Assert.AreEqual
  [<Test>]
  member this.UsedVarTest() =
    safezip usedVarResults passresults 
    |> Seq.iter (fun (exp,act) -> Assert.AreEqual(exp,Unify.usedVars act,sprintf "%A" act))
  [<Test>]
  member this.SubstTest() =
    let replace a b set = if Set.contains a set 
                          then Set.add b (Set.remove a set) 
                          else set
    let t sub =
      Assert.AreEqual(replace (Normal "a") (Normal "e") (Unify.usedVars sub), 
                      Unify.usedVars (Unify.subst (Normal "a") (Var (Normal "e")) sub),
                      sprintf "%A" sub)
    substs |> Seq.iter t
    passresults |> Seq.iter t
(*
TODO: Order of practical tests:
abs : 't -> 't (sans constraints)
Seq.zip : seq<'a> -> seq<'b> -> seq<'a*'b> (renaming/namespacing of aliases)
abs : int -> int (with constraints, and renaming/namespacing of aliases)
Seq.zip : IEnumerable<'a> -> IEnumerable<'b> -> IEnumerable<'a*'b> (denamespacing w/o dealiasing)
*)