// Copyright (c) 2010, Nathan Sanders
// Licence: New BSD. See accompanying documentation.

module Search
open Types
open Util

/// ported from Haskell's Control.Monad.sequence
/// uses foldBack, probably super SUPER inefficient
/// an AWESOME version would use this code for very short lists,
/// switching to the odometer version over arrays for longer ones.
let listSequence (ls : 'b list list) =
  let k l l' = [for x in l do for xs in l' do yield x::xs]
  List.foldBack k ls [[]]
/// Simple recursive version maintains original list order
/// but is probably quite inefficient and is O(n) in stack space.
let rec roundRobin : list<'a> -> list<'a * list<'a>> = function
| [] -> []
| x::xs -> (x,xs) :: List.map (fun (y,ys) -> (y,x::ys)) (roundRobin xs)
/// Simple recursive version is prone to stack overflow on long lists
/// So factorials above 5 are thrown out.
/// This is OK as long as these three functions are only used here;
/// they should be rewritten if they move to Util.
let rec factorialOrder = function
| [] -> [[]]
| l when List.length l > 5 -> [l]
| l -> [for x,xs in roundRobin l do for ys in factorialOrder xs -> x::ys]
let argOrder ts =
  if List.length ts = 2 then [ts]
  else let args,ret = seqButLast ts
       List.map (fun args -> args @ [ret]) (factorialOrder (List.ofSeq args))
let rec variants : Typ -> Typ list = function
| Arrow ts -> 
  ts |> List.map variants |> argOrder |> List.collect listSequence |> List.map Arrow
| Tuple ts -> 
  ts |> List.map variants |> factorialOrder |> List.collect listSequence |> List.map Tuple
| Var v -> [Var v]
| Id id -> [Id id]
| NamedArg(name, t, isOpt) -> List.map (fun t -> NamedArg(name, t, isOpt)) (variants t)
// TODO: subtype variants go here
| Generic(t, args) -> [for t in variants t do 
                         for args in List.map variants args |> listSequence do
                           yield Generic(t, args)]
| Array(n, t) -> List.map (cr Array n) (variants t)
// constraint variants go here (maybe)
| Constraint(c, t) -> List.map (cr Constraint c) (variants t)
let rec matches user library = 
  List.exists (fun t -> index t = library) (variants user)
