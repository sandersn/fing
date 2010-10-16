// Copyright (c) 2010, Nathan Sanders
// Licence: New BSD. See accompanying documentation.

module Opt
open System.Text.RegularExpressions
type Argument = Bare of string | Parameter of string * option<string>
let dearg = function
| Bare value -> Some value
| Parameter(_,arg) -> arg
let mapGet key map value =
  if Map.containsKey key map then map.[key] else value
/// valid parameter forms:
/// {-,/,--}param{ ,=,:}{(",')value(",')}
///
/// Examples:
///  -param1 value1 --param2 /param3:"Test-:-work"
///  /param4=happy -param4 '--=nice=--'
/// TODO: Still messes up last bare parameter for:
/// [|"bare1"; "-param1"; "value1"; "--param2"; "/param3:\"Test-:-work\"";
///   "-param1"; "--param2:foo"; "bare2"|]
/// TODO: Bring back unquoted param values from below commented section
let parse (args : string[]) =
  let unquote = 
    let r = Regex(@"^['""]?(.*?)['""]?$", RegexOptions.IgnoreCase ||| RegexOptions.Compiled)
    fun s -> r.Replace(s, "$1")
  let isParam =
    let r = Regex(@"^-{1,2}|^/", RegexOptions.IgnoreCase ||| RegexOptions.Compiled)
    fun s -> (r.Match s).Success
  let unParam (s : string) =
    let s' = s.Substring 1
    if s'.[0] = '-' then s'.Substring 1 else s'
  let isSingle (s : string) = s.Contains "=" || s.Contains ":"
  let fromSingle (p : string) =
    match p.Split([| '='; ':' |], 2) with
    | [| p; a |] -> Parameter(p, Some (unquote a))
    | _ -> failwith "Catastrophic failure in string.Split"
  let rec bare = function
  | [] -> []
  | (a::args) when isParam a -> 
    match unParam a with
    | p when isSingle p -> fromSingle p :: bare args
    | p -> param p args
  | (a::args) -> Bare a :: bare args
  and param p = function
  | [] -> if isSingle p then [fromSingle p] else [Parameter(p, None)]
  | (a::args) when isParam a -> 
    (if isSingle p then fromSingle p else Parameter(p, None)) 
    :: param (unParam a) args
  | args when isSingle p -> fromSingle p :: bare args
  | (x::args) -> Parameter(p, Some (unquote x)) :: bare args
  let separateBares arglists = 
    let argmap = Map.ofSeq arglists |> Map.map (fun _ l -> Seq.map dearg l)
    let bares = mapGet "" argmap Seq.empty |> Seq.map Option.get
    bares, Map.remove "" argmap
  Seq.groupBy (function
               | Bare arg -> ""
               | Parameter(name,arg) -> name)
              (bare (Array.toList args)) |> separateBares

  (*let args' = Array.toList args
  let parameters = new StringDictionary()
  let mutable parameter = None : string option
  // let parts = [] : string list
  for txt in args' do
    let parts = splitter.Split(txt, 3)
    match parts.Length with
    | 1 -> match parameter with
           | Some p -> if not <| parameters.ContainsKey p then
                         parameters.Add(p, unquote parts.[0])
                       parameter <- None
           | None -> failwith "No parameter waiting for value."
    | 2 -> match parameter with
           | Some p -> if not <| parameters.ContainsKey p then
                         parameters.Add(p, "true")
           | None -> ()
           parameter <- Some parts.[1]
    | 3 -> match parameter with
           | Some p -> if not <| parameters.ContainsKey p then
                         parameters.Add(p, "true")
           | None -> ()
           parameter <- Some parts.[1]
           if parameter.IsSome && not <| parameters.ContainsKey parameter.Value then
             parameters.Add(parameter.Value, unquote parts.[2])
           parameter <- None
    | _ -> failwith "Catastrophic failure in Regex.Split(string, int)"
  match parameter with
  | Some p -> if not <| parameters.ContainsKey parameter.Value then
                parameters.Add(parameter.Value, "true")
  | None -> ()
  parameters*)
  
