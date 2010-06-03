// Copyright (c) 2010, Nathan Sanders
// Licence: New BSD. See accompanying documentation.
module Main
open Fing
// Problems:
// 1. Not clear which classes are visible to reflection
//  (fails: System.Int32, System.Console, Microsoft.FSharp.Collections.HashSet [<T>|<'T>])
// 2. Not clear how to add namespaces to unqualified types
//  (aren't some of these abbreviations?
//   is there a 'current' namespace where these can be looked up?)
// 3. Does parsing of tuples, `nnn and arrow types work?
//  (I need to test comprehensively each FParsec parser)
// 4. I don't parse #seq<int>, for example ("flexible type")
//  or delegate of int -> int ("stupid named delegate type")
//  or int[]
//  or int array / int list
//  or int[,] / int[,,]
let typedefpasses = ["<'a>"
                ; "<'a,'b>"
                ; "<'a,'b when 'a : null>"
                ; "<'a,'b when 'a : (new : unit -> 'T)>"
                ; "<'a,'b when 'a : struct>"
                ; "<'a,'b when 'a : not struct>"
                ; "<'a,'b when 'a : enum<int> >"
                ; "<'a,'b when 'a : enum<int lazy> >"
                ; "<'a,'b when 'a : delegate<int,dword> >"
                ; "<'a,'b when 'a : (write : int->baz)>"
                ; "<'a,'b when 'a : (write : foo:int->bar:baz)>"
                ; "<'a,'b when 'a :> int>"
                ; "<'a,'b when 'a :> int lazy>"
                ; "<'a,'b when 'a : null and 'b : null>"
                ; "<'a when 'a : null and 'a : null>"
                ; "<'a,'b when 'a :> 'b>"
                ; "<'a,'b when 'a : null and 'a :> 'b>"
                ; "<'a when 'a : null and 'a :> 'b>"]
let memberpasses = ["read : stream->int->string"
                    ; "read : stream->int list->string"
                    ; "read : stream->int lazy->string"
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
             ; "(int) lazy"
             ; "(int->int) lazy"
             ; "('a->'a) lazy"
             ; "('a*_)->(int->^a) lazy"
             ; "(('a*_)->(int->^a)) lazy"
             ; "(((int))) lazy"
             ; "Microsoft.FSharp.Core.double lazy"
             ; "Microsoft.FSharp.Core.list`1 lazy"
             ; "list lazy"
             ; "list<'a> lazy"
             ; "list lazy lazy"
             ; "int list"
             ; "int list lazy"
             ; "int list list"
             ; "int list list lazy"
             ; "int[]"
             ; "int[,]"
             ; "int[,,,,,,,]"
             ; "int[,] lazy"
             ; "int[,,] lazy"
             ; "list<'a> when 'a : null"
             ; "list<'a> when 'a : null lazy"
             ; "list<'a> lazy when 'a : null"
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
             ] @ [for m in memberpasses -> "list<'a> when 'a : ("+m+")"]
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
[<EntryPoint>]
let main args =
  // printfn "%s" (find "('a -> Microsoft.FSharp.Core.bool) -> Microsoft.FSharp.Collections.list`1<'a> -> Microsoft.FSharp.Core.int" |> Seq.head)
  // printfn "%s" (find "Microsoft.FSharp.Core.int -> Microsoft.FSharp.Core.int" |> Seq.head)
  // test () |> Seq.iter (printfn "%A")
  textSearch "('a -> bool) -> list<'a> -> 'a"
  textSearch "int -> int"
  textSearch "int -> int -> int"
  textSearch "abs"
  textSearch "tryFind"
  textSearch "( ~~~ )"
  // FSharpAssembly.FSharpLibrary.Entities |> Seq.iter (printfn "%A")
  0
