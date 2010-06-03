module Types

/// type aliases. Probably there is an existing list of these
/// but for now I have just hard coded a few of my favourites
let aliases =
  Map.ofList 
    ["int", "System.Int32"
     "seq", "System.Collections.Generic.IEnumerable`1[T]"
     "bool", "System.Boolean"
     "string", "System.String"
     "char", "System.Char"
     "set", "Microsoft.FSharp.Collections.FSharpSet`1[T]"
     "map", "Microsoft.FSharp.Collections.FSharpMap`2[TKey,TValue]"
     "option", "Microsoft.FSharp.Collections.FSharpOption`1[T]"
     "list", "Microsoft.FSharp.Collections.FSharpList`1[T]"
     "float", "System.Double"
     "double", "System.Double"
     "option", "Microsoft.FSharp.Core.FSharpOption`1[T]"
    ]
let revMap m = Map.toSeq m |> Seq.map (fun (k,v) -> (v,k)) |> Map.ofSeq
let unaliases = revMap aliases
type Typar = Anonymous
           | Normal of string
           | Structural of string
           | Choice of list<Typar>
type Property = Function | Get | Set | GetSet
type Typ = Arrow of list<Typ>
         | Tuple of list<Typ>
         | Var of Typar
         | Id of string
         | NamedArg of string * Typ * bool
         | Generic of Typ * list<Typ>
         | Array of int * Typ
         | Constraint of When * Typ
and When = Null of Typar
         | Struct of Typar
         | NotStruct of Typar
         | DefaultConstructor of Typar
         | Enum of Typar * Typ
         | Delegate of Typar * Typ * Typ
         | Subtype of Typar * Typ
         | Sig of Typar * Typ * Typ * Property
         | TyparConstraint of Typ // where Typ = Constraint
let rec format = function
| Arrow ts -> "(" + String.concat " -> " (Seq.map format ts) + ")"
| Tuple ts -> "(" + String.concat " * " (Seq.map format ts) + ")"
| Var v -> formatTypar v
| Id name when Map.containsKey name unaliases -> Map.find name unaliases
| Id name -> name
| NamedArg (name,t,opt) -> (if opt then "?" else "") + name + ":" + format t
| Generic (t,ts) -> format t + "<" + String.concat "," (Seq.map format ts) + ">"
| Array(n,t) -> format t + "[" + String.replicate (n-1) "," + "]"
| Constraint(con,t) -> format t + " when " + formatConstraint con
and formatTypar = function
| Anonymous -> "_"
| Normal name -> "'" + name
| Structural name -> "^" + name
| Choice typars -> "(" + String.concat " or " (Seq.map formatTypar typars) + ")"
and formatConstraint = function
| x -> x.ToString () // HA HA obviously not done
type Tag<'ty> = | Toople | Arr | Ground of 'ty
type Ty<'ty,'a> = 
  | Param of 'a 
  | Complex of Tag<'ty> * Ty<'ty,'a> list
