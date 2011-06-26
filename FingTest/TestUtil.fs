module Fing.TestUtil
open Util
open TestCases
open NUnit.Framework
// these are surprisingly easy to write and understand in their non-general tuple forms
let safezip l1 l2 = 
  let len = min (List.length l1) (List.length l2)
  Seq.zip (Seq.take len l1) (Seq.take len l2)
let sprints l = Seq.map (sprintf "%A") l |> String.concat ","
let testall (results : seq<'a*'a>) =
  Seq.iteri (fun i (exp,act) ->
               Assert.AreEqual(exp :> obj,act :> obj, sprintf "%d. %A" i act))
            results
let testWith f pairs = Seq.map (second f) pairs |> testall
let forallt f = testall (List.map f passresults)

