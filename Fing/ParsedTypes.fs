// Copyright (c) 2010, Nathan Sanders
// Licence: New BSD. See accompanying documentation.


/// code specific to types parsed from user text
module ParsedTypes
open Types

// WHOA! Hardcoding aliases into a map called 'aliases' is a terrible idea!
// TODO: Fix this to search FSharp.Core for aliases and store them.
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


