// Copyright (c) 2010, Nathan Sanders
// Licence: New BSD. See accompanying documentation.

module Tester
open Types
open NUnit.Framework
open System.Text.RegularExpressions

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
// Later: I was thinking of Haskell's type system. F#'s is .NET's, which can't
// do higher-kinded stuff. OH WELL, LIVING WITH MEDIOCRITY.
let typedefpasses = ["<'a>"
                ; "<'a,'b>"
                ; "<'a,'b when 'a : null>"
                ; "<'a,'b when 'a : (new : unit -> 'T)>"
                ; "<'a,'b when 'a : struct>"
                ; "<'a,'b when 'a : not struct>"
                ; "<'a,'b when 'a : enum<int> >"
                ; "<'a,'b when 'a : enum<int Lazy> >"
                ; "<'a,'b when 'a : delegate<int,dword> >"
                ; "<'a,'b when 'a : (write : int->baz)>"
                ; "<'a,'b when 'a : (write : foo:int->bar:baz)>"
                ; "<'a,'b when 'a :> int>"
                ; "<'a,'b when 'a :> int Lazy>"
                ; "<'a,'b when 'a : null and 'b : null>"
                ; "<'a when 'a : null and 'a : null>"
                ; "<'a,'b when 'a :> 'b>"
                ; "<'a,'b when 'a : null and 'a :> 'b>"
                ; "<'a when 'a : null and 'a :> 'b>"]
let memberpasses = ["read : stream->int->string"
                    ; "read : stream->int list->string"
                    ; "read : stream->int Lazy->string"
                    ; "read : stream->list<int>->string"
                    ; "read : stream->int[]->string"
                    ; "read : stream->int[,,]->string"
                    ; "read : stream->'a->string"
                    ; "read : stream->'a when 'a : (read : int->string)->string"
                    ; "read : string*stream->int"
                    ; "read : ?foo:int->int"
                    ; "read : ?foo:int->bar:int->int"
                    ] @ [for t in typedefpasses -> "read"+t+" : 'a->'b"]
let passes = ["int"
             ; "int->int"
             ; "int ->   int-> \t int"
             ; "'a"
             ; "int*int"
             ; "int->int*int->int"
             ; "int*int->int*int"
             ; "'a->'a"
             ; "'a*'a"
             ; "_*^a"
             ; "(int)"
             ; "(int->int)"
             ; "('a->'a)"
             ; "('a*_)->(int->^a)"
             ; "(((int)))"
             ; "Microsoft.FSharp.Core.double"
             ; "Microsoft.FSharp.Core.list`1"
             ; "list"
             ; "list<>"
             ; "list<int>"
             ; "list<int->int>"
             ; "list<int*int>"
             ; "list<int*(int->int*int)>"
             ; "list<list<int>>"
             ; "list<_>"
             ; "list<_,_>"
             ; "list<int,'a,_>"
             ; "(int) Lazy"
             ; "(int->int) Lazy"
             ; "('a->'a) Lazy"
             ; "('a*_)->(int->^a) Lazy"
             ; "(('a*_)->(int->^a)) Lazy"
             ; "(((int))) Lazy"
             ; "Microsoft.FSharp.Core.double Lazy"
             ; "Microsoft.FSharp.Core.list`1 Lazy"
             ; "list Lazy"
             ; "list<'a> Lazy"
             ; "list Lazy Lazy"
             ; "int list"
             ; "int list Lazy"
             ; "int list list"
             ; "int list list Lazy"
             ; "int[]"
             ; "int[,]"
             ; "int[,,,,,,,]"
             ; "int[,] Lazy"
             ; "int[,,] Lazy"
             ; "list<'a> when 'a : null"
             ; "list<'a> when 'a : null Lazy"
             ; "list<'a> Lazy when 'a : null"
             ; "list<'a> when 'a : (new : unit -> 'T)"
             ; "list<'a> when 'a : struct"
             ; "list<'a> when 'a : not struct"
             ; "list<'a> when 'a : enum<int>"
             ; "list<'a> when 'a : delegate<int,'a>"
             ; "list<'a> when 'a :> Microsoft.Collections.IComparable"
             ; "list<'a,'b> when 'a :> 'b"
             ; "list<'a> when 'a : (read : string->int with get)"
             ; "list<'a> when 'a : (read : string->int with set)"
             ; "list<'a> when 'a : (read : string->int with get,set)"
             ; "list<'a> when 'a : (read : string->int with set,get)"
             ; "list<'a> when ('a or 'b) : (read : string->int->stream)"
             ; "list<'a> when 'a : (read<'b,'c> : 'a -> 'b -> 'c)"
             ] @ [for m in memberpasses -> "list<'a> when 'a : ("+m+")"
             ] @ ["set<'a> -> list<'a>"]
let test () = 
  Seq.map Parser.parse passes
    (*["'a";
     "(Microsoft.FSharp.Core.int)"; 
     "Microsoft.FSharp.Core.char"; 
     "Microsoft.FSharp.Core.string";
     "Microsoft.FSharp.Core.int * Microsoft.FSharp.Core.int";
     "Microsoft.FSharp.Core.int * Microsoft.FSharp.Core.int * Microsoft.FSharp.Core.int";
     "Microsoft.FSharp.Core.int * (Microsoft.FSharp.Core.int * Microsoft.FSharp.Core.int)";
     "(Microsoft.FSharp.Core.int * Microsoft.FSharp.Core.int) * Microsoft.FSharp.Core.int";
//     "Microsoft.FSharp.Collections.HashSet" 
//     "System.Console"
//     "Microsoft.FSharp.Collections.list`1<Microsoft.FSharp.Core.int>"
    ] *) 
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
 ;Generic (Id "Lazy",[Id "int"])
 ;Generic (Id "Lazy",[Arrow [Id "int"; Id "int"]])
 ;Generic (Id "Lazy",[Arrow [Var (Normal "a"); Var (Normal "a")]])
 ;Arrow
  [Tuple [Var (Normal "a"); Var Anonymous];
   Generic (Id "Lazy",[Arrow [Id "int"; Var (Structural "a")]])]
 ;Generic
  (Id "Lazy",
   [Arrow
      [Tuple [Var (Normal "a"); Var Anonymous];
       Arrow [Id "int"; Var (Structural "a")]]])
 ;Generic (Id "Lazy",[Id "int"])
 ;Generic (Id "Lazy",[Id "Microsoft.FSharp.Core.double"])
 ;Generic (Id "Lazy",[Id "Microsoft.FSharp.Core.list`1"])
 ;Generic (Id "Lazy",[Id "list"])
 ;Generic (Id "Lazy",[Generic (Id "list",[Var (Normal "a")])])
 ;Generic (Id "Lazy",[Generic (Id "Lazy",[Id "list"])])
 ;Generic (Id "list",[Id "int"])
 ;Generic (Id "Lazy",[Generic (Id "list",[Id "int"])])
 ;Generic (Id "list",[Generic (Id "list",[Id "int"])])
 ;Generic (Id "Lazy",[Generic (Id "list",[Generic (Id "list",[Id "int"])])])
 ;Array (1,Id "int")
 ;Array (2,Id "int")
 ;Array (8,Id "int")
 ;Generic (Id "Lazy",[Array (2,Id "int")])
 ;Generic (Id "Lazy",[Array (3,Id "int")])
 ;Constraint (Null (Normal "a"),Generic (Id "list",[Var (Normal "a")]))
 ;Generic
  (Id "Lazy",
   [Constraint (Null (Normal "a"),Generic (Id "list",[Var (Normal "a")]))])
 ;Constraint
  (Null (Normal "a"),
   Generic (Id "Lazy",[Generic (Id "list",[Var (Normal "a")])]))
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
      Arrow [Id "stream"; Generic (Id "Lazy",[Id "int"]); Id "string"],Function),
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
              (Enum (Normal "a",Generic (Id "Lazy",[Id "int"])),
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
              (Subtype (Normal "a",Generic (Id "Lazy",[Id "int"])),
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
   Generic (Id "list",[Var (Normal "a")]))
 ;Arrow [Generic (Id "set", [Var (Normal "a")]);
         Generic (Id "list", [Var (Normal "a")])]]
let usedVarResults = 
  let e = Set.empty
  let a = Set.singleton (Normal "a")
  let _a = Set.ofList [Anonymous;Normal "a"]
  let ab = Set.ofList [Normal "a";Normal "b"]
  let aab = Set.add (Choice (List.ofSeq ab)) a
  let abab = Set.add (Choice (List.ofSeq ab)) ab
  let _u = Set.singleton Anonymous
  [e;e;e; a; e;e;e; 
   a;a; Set.add (Structural "a") _u; e;e; a; 
   Set.add (Structural "a") _a; e; e; e; e; e; e; e; e; e; e
   _u; _u; _a; e; e; a; Set.add (Structural "a") _a; Set.add (Structural "a") _a; 
   e; e; e; e; a; e;e;e;e;e;e;e;e;e;e;
   a;a;a;a;a;a;a;a;a;
   ab;a;a;a;a;aab; Set.add (Normal "c") ab
   // memberpasses
   a;a;a;a;a;a;a;a;a;a;a;
   // typedefpasses
   ab;ab;abab;abab;abab;abab;abab;abab;abab;abab;abab;abab;abab;abab;ab; abab;abab;ab;
   // new stuff
   a]
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
  let core = Microsoft.FSharp.Metadata.FSharpAssembly.FSharpLibrary
  // let parsec = Microsoft.FSharp.Metadata.FSharpAssembly.FromFile "Y:/src/Fing/Fing/bin/Debug/FParsec.dll"
  let ts = seq {
    for e in core.Entities do
    for m in e.MembersOrValues do
    yield {Fing.ent=e; Fing.mem=m; Fing.typ=FSharpTypes.cvt m.Type |> Types.index |> FSharpTypes.debinarize} 
  }
  let finder (t : Fing.Result) =
    Fing.typeFind (format t.typ) |> Seq.tryFind ((=) t)
  let arrayShuffle (ara : 'a[]) =
    let rnd = System.Random()
    for i in [ara.Length .. -1 .. 1] do
      let j = rnd.Next i
      let tmp = ara.[j]
      ara.[j] <- ara.[i-1]
      ara.[i-1] <- tmp
    ara
  [<Test>]
  member public this.ParseTest() = 
    Assert.AreEqual(List.length passes, List.length passresults)
    List.zip passresults (List.map Parser.parse passes) |> List.iter Assert.AreEqual
  [<Test>]
  member this.UsedVarTest() =
    safezip usedVarResults passresults 
    |> Seq.iteri (fun i (exp,act) -> 
                   Assert.AreEqual(exp,Unify.usedVars act,sprintf "%d. %A" i act))
// Disabled until I decide this code is actually necessary (it's probably not)
//  [<Test>]
//  member this.SubstTest() =
//    let replace a b set = if Set.contains a set 
//                          then Set.add b (Set.remove a set) 
//                          else set
//    let t sub =
//      Assert.AreEqual(replace (Normal "a") (Normal "e") (Unify.usedVars sub), 
//                      Unify.usedVars (Unify.subst (Normal "a") (Var (Normal "e")) sub),
//                      sprintf "%A" sub)
//    substs |> Seq.iter t
//    passresults |> Seq.iter t
  /// Make sure that every function in FSharp.Core can be found if you at least search
  /// for the exact type obtained from the FSharpType itself.
  /// (argument-swapped tests will come later) 
  [<Test>]
  member this.SmokeTest() =
    ts |> Seq.iter (fun t -> 
                     Assert.AreEqual(Some t, finder t,
                                     t.mem.DisplayName + ":" + format t.typ))
  [<Test>]
  member this.ShuffleSmokeTest() =
    let shuffled = function
    | Arrow ts when List.length ts > 2 -> 
      let args,res = Fing.seqButLast ts
      let args' = arrayShuffle (Array.ofSeq args) |> List.ofArray
      Arrow (args' @ [res])
    | t -> t
    ts |> Seq.iter (fun t ->
                     Assert.AreEqual(Some t,
                                     finder t,
                                     t.mem.DisplayName + ":" + format t.typ))
[<TestFixture>]
type public TypeTester() =
  let dewhite s = Regex.Replace(s, @"\s+|\(|\)", "")
  // TODO: I skipped the stupid aliasing stuff by mistake. That needs testing,
  // if only to show how bad it is.
  [<Test>]
  member this.TestFormat() =
    // TODO: Fails because formatConstraint is not finished.
    //   Finish this next.
    safezip passes passresults 
    |> Seq.iteri (fun i (s,t) -> 
         Assert.AreEqual(Parser.parse s, Types.format t |> Parser.parse, i.ToString()))
