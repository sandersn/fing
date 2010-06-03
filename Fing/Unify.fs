module Unify
open Types
open Parser
open Microsoft.FSharp.Metadata
// fancy unification stuff (not implemented yet)
// mainly: list<'a> |=> seq<'a> (interface)
//         list<'a> |=> 'a (most-general unification)
//         Stack<'a> |=> StretchList<'a> (subtype, I htink my example is wrong)
//           (also, with proper respect for co-variance and contra-variance)
//           (ugh)

let check t = function // TODO: this is a no-op right now because I can't verify any of these
| Null var -> true // because Id should contain FSharpType or whatever instead of string
| Struct var -> true
| NotStruct var -> true
| DefaultConstructor var -> true
| Enum(var,t) -> true
| Delegate(var,domain,range) -> true
| Subtype(var,t) -> true
| Sig(var,id,t,prop) -> true
| TyparConstraint t -> true


let rec subst' v t = function
| Complex(tag,l) -> Complex(tag, l |> List.map (subst' v t))
| Param i when i = v -> t
| Param j -> Param j
(* substitute type t for variable v in the target type *)
let rec subst v t = function
| Var i when i = v -> t
| Arrow types -> Arrow (List.map (subst v t) types)
| Tuple types -> Tuple (List.map (subst v t) types)
| NamedArg (name,typ,opt) -> NamedArg(name,subst v t typ,opt)
| Generic (typ,types) -> Generic (subst v t typ,List.map (subst v t) types)
| Array (n,typ) -> Array (n,subst v t typ)
| Constraint (quand,typ) when check t quand -> subst v t typ
| typ -> typ // Id and non-matching Var

(* get type variables used in a type *)
let rec stringify = function // this function is cheesy because it's a hack and needs to disappear
| Anonymous -> Set.singleton "_"
| Structural a -> Set.singleton a
| Normal a -> Set.singleton a
| Choice vars -> Set.unionMany (List.map stringify vars)

let rec usedVars = function // should be Typ -> Set<Typar> not Set<string>
| Var (Choice vars) -> Set.ofList vars
| Var v -> Set.singleton v
| Arrow types -> Set.unionMany (List.map usedVars types)
| Tuple types -> Set.unionMany (List.map usedVars types)
| Id _ -> Set.empty
| NamedArg(_,t,_) -> usedVars t
| Generic(t,types) -> usedVars t |>Set.union<| Set.unionMany (List.map usedVars types)
| Array(_,t) -> usedVars t
| Constraint(var,t) -> usedConstraintVars var |>Set.union<| usedVars t
and usedConstraintVars = function
| Null var -> Set.singleton var
| Struct var -> Set.singleton var
| NotStruct var -> Set.singleton var
| DefaultConstructor var -> Set.singleton var
| Enum(var,t) -> var |>Set.add<| usedVars t
| Delegate(var,t,t') -> var |>Set.add<| usedVars t |>Set.union<| usedVars t'
| Subtype(var,t) -> var |>Set.add<| usedVars t
| Sig(Choice(vars),t,t',_) -> 
  (Set.ofList vars) |>Set.union<| usedVars t |>Set.union<| usedVars t'
| Sig(var,t,t',_) -> var |>Set.add<| usedVars t |>Set.union<| usedVars t'
| TyparConstraint t -> usedVars t
let rec usedVars' = function
| Param i -> Set.singleton i;
| Complex(tag, l) -> Set.unionMany (List.map usedVars' l)

type Env = list<Typar * Typ>
let both f (x,y) = (f x, f y)
let guard b f = if b then f () else None
(* Find most general unifier (if any) for two types 
for example, make sure that
list<'a> -> int <=> list<list<'a>> -> int
't <=> ('a -> 'b)
't <=> ('a * 'b)
but
not ('t `mgu`
*)
let (<=>) t1 t2 =
  let rec mgu subs = function
  | [] -> Some subs
  | (Complex(tag1,l1),Complex(tag2,l2))::rest ->
       if tag1 <> tag2 then
         None
       else
         let rec loop r = function
         | [],[] -> mgu subs r
         | [],_ | _,[] -> None
         | x::xs, y::ys -> loop ((x,y)::r) (xs,ys)
         loop rest (l1,l2)
  | (Param i, Param j)::rest when i = j -> mgu subs rest
  | ((Param i, x) | (x, Param i))::rest ->
       if (Set.contains i (usedVars' x)) then
         None (* type would be infinite when unifying *)
       else
         mgu ((i,x)::subs) (rest |> List.map (fun (t1,t2) -> (subst' i x t1, subst' i x t2)))
  let rec mgu' (subs : Env) rest = function
  // Note: subs is unneeded here because tyvar substitutions happen immediately
  // MAYBE you only need subs with backtracking
  // or MAYBE it would be more elegant to use it to look up substitutions each time
  | (Arrow l1, Arrow l2) ->
    // this version is less efficient than the previous version because it fully traverses
    // both lists twice. But it's a lot easier to understand.
    guard (List.length l1 = List.length l2)
      (fun _ -> Some ([], List.zip l1 l2 @ rest))
  | (Tuple l1, Tuple l2) ->
    guard (List.length l1 <> List.length l2)
      (fun _ -> Some ([], List.zip l1 l2 @ rest))
  | (Var (Normal i), Var (Normal j)) when i = j -> Some([],rest)
  | (Var (Structural i), Var (Structural j)) when i = j -> Some([],rest)
  | ((Var Anonymous, Var _) | (Var _, Var Anonymous)) -> Some([],rest)
  | ((Var i, t) | (t, (Var i))) ->
    guard (usedVars t |>(Set.contains<| i)) // Infinite type if unified
      (fun _ -> Some([(i,t)], (rest |> List.map (both (subst i t)))))
  | (Id i, Id j) when i = j -> Some([],rest)
  // optional args are treated same as others for now
  | (NamedArg (i,t,opt), NamedArg (j,t',opt')) -> Some([],(t,t')::rest)
  | (Generic(t,args),Generic(t',args')) when List.length args = List.length args' -> 
    Some([], (t,t')::List.zip args args')
  | (Array(n,t),Array(n',t')) when n = n' -> Some([],(t,t')::rest)
  // still have to handle (w,w') somehow, probably need another function for that
  | (Constraint(w,t),Constraint(w',t')) -> Some([],(t,t')::rest)
  | _ -> None // avoid blue lines for now...
  let rec mgu'' (subs : Env) = function
  | [] -> Some subs
  | (t::ts) -> 
    match mgu' subs ts t with
    | None -> None
    | Some(subs',ts') -> mgu'' (subs' @ subs) ts'
    
  // TODO: Finish this!
  mgu'' [] [(t1,t2)]

(* finds entities in the F# library with the requested signature, modulo type parameter unification *)
let find' s =
  let ty = parse s
  let vars = usedVars ty
  seq {
    for e in FSharpAssembly.FSharpLibrary.Entities do
    for m in e.MembersOrValues do
      (* need try/catch to avoid error on weird types like "[]`1" *)
      match (try Some(FSharpTypes.cvt m.Type) with _ -> None) with
      | Some ty2 ->
        (* rename all type variables from the query to avoid incorrectly unifying with type variables in signatures *)
        let used = usedVars ty2
        let newVars = Seq.choose 
                       (fun v -> 
                         if Set.contains (Normal v) used || Set.contains (Structural v) used
                         then Some(Var (Normal v))
                         else None)
                       Types.names
        //let ty = Map.fold (fun t v p -> subst v p t) ty varMap
        let ty = Map.foldBack subst (Map.ofSeq (Seq.zip vars newVars)) ty
        match ty <=> ty2 with
        | None -> ()
        | Some _ -> yield sprintf "%s.%s.%s" e.Namespace e.DisplayName m.DisplayName 
      | _ -> () }
