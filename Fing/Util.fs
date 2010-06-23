// Copyright (c) 2010, Nathan Sanders
// Licence: New BSD. See accompanying documentation.

module Util
let chompchomp (s : string) = 
  if s.Length > 2 then s.[1..s.Length - 2] else s
// TODO: There are plenty of utilities scattered around the project already.
// Most should probably migrate here.
let cr f x y = f(x,y) 
let constant a b = a
let seqButLast l =
  let len = Seq.length l - 1
  Seq.take len l, Seq.skip len l |> Seq.head