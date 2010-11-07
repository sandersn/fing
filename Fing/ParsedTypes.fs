// Copyright (c) 2010, Nathan Sanders
// Licence: New BSD. See accompanying documentation.


/// code specific to types parsed from user text
module ParsedTypes
open Types

// WHOA! Hardcoding aliases into a map called 'aliases' is a terrible idea!
// TODO: Fix this to search FSharp.Core for aliases and store them.
let dealias =
  let dealias' = function
  | Id id when Map.containsKey id aliases -> Some (Id (Map.find id aliases))
  | _ -> None
  Types.map dealias' id  // might need to use something besides id someday
