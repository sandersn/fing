// Copyright (c) 2010, Nathan Sanders
// Licence: New BSD. See accompanying documentation.
module Util
let chompchomp (s : string) = 
  if s.Length > 2 then s.[1..s.Length - 2] else s
// TODO: There are plenty of utilities scattered around the project already.
// Most should probably migrate here.
let cr f x y = f(x,y) 
let constant a b = a
module Seq =
  let butLast l =
    let len = Seq.length l - 1
    Seq.take len l, Seq.skip len l |> Seq.head
module Array =
  let shuffle a = 
    let rnd = System.Random ()
    let len = Array.length a
    let aswap i j =
      let tmp = a.[i]
      a.[i] <- a.[j]
      a.[j] <- tmp    
    for i = len - 1 downto 0 do
      aswap i (rnd.Next (i + 1))
    a