// Copyright (c) 2010, Nathan Sanders
// Licence: New BSD. See accompanying documentation.
module Main


[<EntryPoint>]
let main args =
  let args,argmap = Opt.parse args
  let getrefs s = Seq.choose id (Opt.mapGet s argmap Seq.empty)
  let references = (getrefs "r" |>Seq.append<| getrefs "reference")
  Fing.addReferences references
  match Seq.toList args with
  | [ t ] -> Fing.textSearch t
  | _ -> printfn @"Fing is F# API Search.

Usage:

  fing ""F# type""
  -OR-
  fing ""F# function""

Example:

  fing ""int -> int""
  fing ""('T -> bool) -> seq<'T> -> 'T option""
  fing ""seq<'a> -> seq<'b> -> seq<'c> -> seq<'a * 'b * 'c>""
  
  fing abs
  fing tryFind
  fing ""( ~~~ )""
  (NOTE: Parentheses and spaces are required for operators)
  
To reference assemblies besides FSharp.Core, use

  fing -r Assembly.dll ""'a -> 'b""
  -OR-
  fing --reference:Assembly.dll ""'a -> 'b""
  
For F# type syntax, refer to
  research.microsoft.com/en-us/um/cambridge/projects/fsharp/manual/spec.html
or, for an introduction, see 
  lorgonblog.spaces.live.com/Blog/cns!701679AD17B6D310!1077.entry
   "
  0
