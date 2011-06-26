module Fing.TypeTests
open NUnit.Framework
open Microsoft.FSharp.Metadata
open Util
open Types
open TestCases
open TestUtil

[<TestFixture>]
type public TypeTester() =
  let foldTypar empty singleton concat = 
    let rec fold = function
    | Var v -> singleton v
    | Arrow types -> concat (List.map fold types)
    | Tuple types -> concat (List.map fold types)
    | Id _ -> empty
    | NamedArg(_,t,_) -> fold t
    | Generic (t,types) ->
      concat (fold t :: List.map fold types)
    | Array(_,t) -> fold t
    | Constraint(when_,t) ->
      concat [fold t; foldWhen when_]
    and foldWhen = function
    | Null v -> singleton v
    | Struct v -> singleton v
    | NotStruct v -> singleton v
    | DefaultConstructor v -> singleton v
    | Enum(v, t) -> concat [singleton v; fold t]
    | Delegate(v, t, t') -> concat [singleton v; fold t; fold t']
    | Subtype(v, t) -> concat [singleton v; fold t]
    | Sig(v, t, t', p) -> concat [singleton v; fold t; fold t']
    | TyparConstraint cons -> concat (List.map foldWhen cons)
    fold
  let usedVars = foldTypar [] (fun x -> [x]) List.concat
  let usedIndices = foldTypar Set.empty Set.singleton Set.unionMany
  [<Test>]
  member this.TestFormat() =
    safezip (List.map Parser.parse passes) 
            (List.map (Types.format >> Parser.parse) passresults )
    |> testall
  [<Test>]
  member this.TestIndex() =
    let randomise seq = Seq.zip seq (seq |> Array.ofSeq |> Array.shuffle)
    let subst (t, shuffles) =
      let map = Map.ofSeq shuffles
      let subst' = function
      | Var v -> Some (Var (Map.find v map))
      | _ -> None
      Types.mapSimple subst' t
    let testPropEq f g = safezip (List.map f passresults) (List.map g passresults) |> testall
    // 1. length (usedIndices t) == length (usedIndices (index t))
    testPropEq (usedIndices >> Set.count) 
               (Types.index >> usedIndices >> Set.count)
    // 2. length (set (leftToRightListOfVariableNames (index t))) == length (usedIndices (index t))
    testPropEq (Types.index >> usedVars >> set >> Set.count)
               (Types.index >> usedIndices >> Set.count)
  [<Test>]
  member this.TestRevMap() =
    testWith Types.revMap [
      Map.empty, Map.empty
      Map.ofList [("a","b")], Map.ofList [("b","a")]
      Map.ofList [("a","b");("a","c")], Map.ofList [("c", "a")]
    ]
  member this.TestMapTypar() =
    ()
  [<Test>]
  member this.TestMapSimple() =
    // 1. id == mapSimple Some
    forallt (fun t -> (t, Types.mapSimple Some t))
    // 2. id == mapSimple None 
    forallt (fun t -> (t, Types.mapSimple (constant None) t))
  [<Test>]
  member this.TestMap() =
    // 1. id == map Some Some
    forallt (fun t -> (t, Types.map (fun _ _ t -> Some t) (fun _ _ w -> Some w) t))
    // 2. id == map None None
    forallt (fun t -> (t, Types.map (fun _ _ _ -> None) (fun _ _ _ -> None) t))
    // 3. replacing all vars with anonymous via map means that usedVars t == { Anonymous }
    let anonymiser _ _ = function
    | Var _ -> Some (Var Anonymous)
    | _ -> None
    let anonymiserW kt kw = function
    | Null _ -> Some (Null Anonymous)
    | Struct _ -> Some (Struct Anonymous)
    | NotStruct _ -> Some (NotStruct Anonymous)
    | DefaultConstructor _ -> Some (DefaultConstructor Anonymous)
    | Enum(_,t) -> Some (Enum(Anonymous, kt t))
    | Delegate(_,t,t') -> Some(Delegate(Anonymous, kt t, kt t'))
    | Subtype (_,t) -> Some(Subtype(Anonymous, kt t))
    | Sig(_,t,t',prop) -> Some(Sig(Anonymous, kt t, kt t', prop))
    | _ -> None
    forallt (fun t ->
              match Types.map anonymiser anonymiserW t |> usedVars with
              | [] -> (Set.empty, Set.empty)
              | vs -> (Set.singleton Anonymous, set vs))
  [<Test>]
  member this.TestUsedIds() =
    // this is a DSL because I used Sun-style indentation for my square brackets
    // and I passed a function. It's the future!
    testWith Types.usedIds [
      Set.empty, Var Anonymous
      Set.empty, Var (Normal "a")
      Set.singleton "int", Id "int"
      Set.singleton "int", Arrow [Id "int"; Id "int"]
      Set.ofList ["int"; "double"], Arrow [Id "int"; Id "double"]
      Set.ofList ["int"; "double"], Arrow [Id "int"; Var Anonymous; Id "double"]
    ]
[<TestFixture>]
type public FSharpTypeTester() =
  let core = FSharpAssembly.FSharpLibrary
  let rawts = seq { 
    for e in core.Entities do 
    for m in e.MembersOrValues do 
    yield m
  }
  [<Test>]
  member this.TestDebinarise() =
    let tests = [
      Arrow [Id "int"], Arrow [Id "int"]
      Arrow [Id "int"; Id "int"], Arrow [Id "int"; Id "int"]
      Arrow [Id "int"; Id "int"; Id "int"], Arrow [Id "int"; Arrow [Id "int"; Id "int"]]
      // right branching only
      Arrow [Arrow [Id "int"; Id "int"]; Id "int"], Arrow [Arrow [Id "int"; Id "int"]; Id "int"]
      Var Anonymous, Var Anonymous


      Constraint
        (Sig
           (Choice [Normal "a"; Normal "b"],Id "read",
            Arrow [Id "string"; Id "int"; Id "stream"],Function),
         Generic (Id "list",[Var (Normal "a")])), 
      Constraint
        (Sig
           (Choice [Normal "a"; Normal "b"],Id "read",
            Arrow [Id "string"; Arrow [Id "int"; Id "stream"]],Function),
         Generic (Id "list",[Var (Normal "a")]))
      Constraint
        (Sig
          (Normal "a",Generic (Id "read",[Var (Normal "b"); Var (Normal "c")]),
           Arrow [Var (Normal "a"); Var (Normal "b"); Var (Normal "c")],Function),
         Generic (Id "list",[Var (Normal "a")])),
      Constraint
        (Sig
          (Normal "a",Generic (Id "read",[Var (Normal "b"); Var (Normal "c")]),
           Arrow [Var (Normal "a"); Var (Normal "b"); Var (Normal "c")],Function),
         Generic (Id "list",[Var (Normal "a")]))
    ]
    testWith FSharpTypes.debinarize tests
  [<Test>]
  member this.TestIsArray() =
    let gca = rawts |> Seq.find (fun m -> m.DisplayName = "GetCustomAttributes")
    Assert.IsFalse (FSharpTypes.isArray (gca.Type.GenericArguments.[0]))
    Assert.IsTrue (FSharpTypes.isArray (gca.Type.GenericArguments.[1].GenericArguments.[1]))
  [<Test>]
  member this.TestDimensions() =
    let gca = rawts |> Seq.find (fun m -> m.DisplayName = "GetCustomAttributes")
    let ara = gca.Type.GenericArguments.[1].GenericArguments.[1]
    Assert.AreEqual ("[]", ara.NamedEntity.DisplayName)
    Assert.AreEqual (1,FSharpTypes.dimensions gca.Type.GenericArguments.[1].GenericArguments.[1])
  [<Test>]
  member this.TestCvt() =
    for t in rawts do
      if t.Type.IsFunction then
        Assert.IsTrue (match FSharpTypes.cvt t.Type with
                       | Arrow _ -> true
                       | _ -> false)
      elif t.Type.IsTuple then
        Assert.IsTrue (match FSharpTypes.cvt t.Type with
                       | Tuple _ -> true
                       | _ -> false)
    let getT name = rawts |> Seq.find (fun t -> t.DisplayName = name)
    let t1 = getT "GetCustomAttributes"
    let t2 = getT "GetExceptionFields"
    let t3 = getT "Cast"
    let intmod (t : FSharpMemberOrVal) = t.DisplayName = "( % )" 
                                         && not t.Type.GenericArguments.[0].IsTuple
    let t4 = rawts |> Seq.find intmod
    Assert.AreEqual(1, rawts |> Seq.filter intmod |> Seq.length)
    // TODO: test int defaulting
    // and someday whenify and canonicalType when those features are to be completed
    Assert.IsTrue (match FSharpTypes.cvt t1.Type with
                   | Arrow _ -> true
                   | _ -> false)
    Assert.AreEqual(FSharpTypes.cvt t1.Type.GenericArguments.[0], Id "UnionCaseInfo")
    Assert.AreEqual(FSharpTypes.cvt t1.Type.GenericArguments.[1].GenericArguments.[1],
                    Array (1, Id "Object"))
    Assert.IsTrue (match FSharpTypes.cvt t2.Type.GenericArguments.[0] with
                   | Tuple _ -> true
                   | _ -> false)
    Assert.AreEqual(FSharpTypes.cvt t2.Type.GenericArguments.[0].GenericArguments.[1],
                    Generic (Id "Option", [Id "BindingFlags"]))
    Assert.AreEqual(FSharpTypes.cvt t3.Type.GenericArguments.[1],
                    Generic (Id "Expr", [Var (Normal "T")]))
    Assert.AreEqual(FSharpTypes.cvt t4.Type.GenericArguments.[0],
                    Id "Int32")
