// Copyright (c) 2010, Nathan Sanders
// Licence: New BSD. See accompanying documentation.

module Parser
open Microsoft.FSharp.Metadata
open System.Text.RegularExpressions
open Util
open Types
open FParsec.Primitives
open FParsec.CharParsers
// Brian's explanation:
//   http://lorgonblog.spaces.live.com/blog/cns!701679AD17B6D310!1077.entry
// Official documentation:
//   http://msdn.microsoft.com/en-us/library/dd233230(VS.100).aspx
// Spec (Beta 2 from 2009/11, not RC)
//   http://research.microsoft.com/en-us/um/cambridge/projects/fsharp/manual/spec.html#_Toc245030785

//////////////
let identifierChar c =
  match c with
  | '.' | '`' | '_' -> true
  | _ when ('A' <= c && c <= 'Z') || ('a' <= c && c <= 'z') || ('0' <= c && c <= '9') ->
    true
  | _ -> false
let rec identifierLoop (st : FParsec.State<'u>) (acc : char list)  =
  match st.Iter.Read() with
  | c when identifierChar c -> identifierLoop st.Next (c :: acc)
  | _ -> (new string(List.toArray <| List.rev acc), st)
let rec skipSpace (st : FParsec.State<'u>) =
  match st.Iter.Read() with // maybe need to check for EOS here ?
  | ' ' | '\t' -> skipSpace st.Next
  | _ -> st
let tokeniser f expected : Parser<string, 'u> =
  fun state -> 
    let st = skipSpace state
    let c = st.Iter.Read()
    match c with
    | FParsec.CharParsers.EOS -> Reply(Error, FParsec.Error.expectedError "end of file", state)
    | c when identifierChar c -> 
      let (t,stAfter) = identifierLoop st.Next [c]
      if f t then
        Reply(t, stAfter)
      else
        Reply(Error, FParsec.Error.backtrackError stAfter (FParsec.Error.expectedError expected), state)
        // Reply(Error, FParsec.Error.expectedError expected, st) // or stAfter??? or state?
    | '-' ->
      let stPeek = st.Next
      match stPeek.Iter.Read() with
      | '>' -> 
        if f "->" then
          Reply("->", stPeek.Next)
        else
          Reply(Error, FParsec.Error.backtrackError stPeek (FParsec.Error.expectedError expected), state)
      | _ -> Reply(Error, FParsec.Error.expectedError "Dash only allowed in -> digraph", state)
    | ':' ->
      let stPeek = st.Next
      match stPeek.Iter.Read() with
      | '>' ->
        if f ":>" then
          Reply(":>", stPeek.Next)
        else
          Reply(Error, FParsec.Error.backtrackError stPeek (FParsec.Error.expectedError expected), state)
      | _ -> 
        if f ":" then
          Reply(":", stPeek)
        else
          Reply(Error, FParsec.Error.backtrackError stPeek (FParsec.Error.expectedError expected), state)
    | '*' | '<' | '>' | '(' | ')' | '[' | ']' | ',' | '\'' | '^' | '#' | '?' ->
      if f (string c) then
        Reply(string c, st.Next)
      else
        Reply(Error, FParsec.Error.backtrackError st (FParsec.Error.expectedError expected), state)
    | _ -> Reply(Error, FParsec.Error.expectedError "Identifier or punctuation", state)
let tok s = tokeniser ((=) s) s
let notTok ts = tokeniser (fun t -> not <| List.exists ((=) t) ts) "Non-keyword"
/////////////
// utils
let passthrough t l = 
  match (t,l) with
  | _,[] -> failwith "This can't happen because it's sepBy_1_"
  | _,[pass] -> pass
  | typ,kids -> typ kids
let devar t =
  match t with
  | (Var var) -> var
  | _ -> failwith "Not a Var"  
let nestTypes = List.fold (|>)
///////// start parsing! /////////
let (typeP,typeref) = createParserForwardedToRef ()
let (constraintP,conref) = createParserForwardedToRef ()
let identP = notTok ["lazy"; "with"; "when"; "if"; "else"; "do"; "new"; //"get"; "set"
                     "<"; ">"; "["; "]"; "("; ")"; "#"; ":"; "?"; "->"; ","
                     "-"; "*"; "'"; "_"; "^"]
let tupleP = sepBy1 typeP (tok "*") |>> passthrough Tuple
let arrowP = sepBy1 tupleP (tok "->") |>> passthrough Arrow
let postfixP id = between (tok "<") (tok ">") (sepBy arrowP (tok ",")) |>> (cr Generic id)
let longidentP = identP >>= (fun id -> (postfixP (Id id)) <|>% Id id)
let typevarP = 
  let anon = tok "_" >>. preturn (Var Anonymous)
  let normal = tok "'" >>. identP |>> (Var << Normal)
  let structural = tok "^" >>. identP |>> (Var << Structural)
  choice [anon; normal; structural]
let arrayP = parse {
  let! dims = between (tok "[") (tok "]") (many (tok ","))
  return cr Array (1 + List.length dims)
}
let traitP var = 
  let typevarConstraintsP = tok "when" >>. sepBy1 constraintP (tok "and")
  let argNameSpecP = parse {
    let! optional = opt (tok "?")
    let! argname = identP
    let! _ = tok ":"
    return (optional.IsSome, argname)
  }
  let argSpecP = parse {
    let! argspec = opt (attempt argNameSpecP)
    let! t = typeP
    match argspec with
    | Some(optional,name) -> return NamedArg(name,t,optional)
    | None -> return t
  }
  let argsSpecP = parse {
    let! args = sepBy1 argSpecP (tok "*")
    match args with
    | [arg] -> return arg
    | args -> return Tuple args
  }
  let curriedSigP = parse {
    let! arg = argsSpecP
    let! _ = tok "->"
    let! args = sepBy1 argsSpecP (tok "->")
    return Arrow (arg::args)
  }
  let typevarDefnsP = parse {
    let! _ = tok "<"
    let! defns = sepBy1 typevarP (tok ",")
    let! (constraints : option<list<Typ -> Typ>>) = opt typevarConstraintsP
    let! _ = tok ">"
    // TODO: This is the part to change: remove Var << Choice and call to nestTypes
    // hopefully without destroying any semantics.
    // 1. first apply the delayed constraints to a dummy type
    // 2. then strip out the dummy type, leaving just the constraints
    return (defns, constraints
                   |> Option.map 
                     (List.map ((|>) (Id "Dummy"))
                      >> List.map (function
                                   | Constraint(con,dummy) -> con
                                   | _ -> failwith "constraint parser emitted non-constraint")))

  }
  let accessorsP =
    let getset = tok "get" >>. tok "," >>. tok "set" >>. preturn ()
    let setget = tok "set" >>. tok "," >>. tok "get" >>. preturn ()
    tok "with" >>. choice [attempt getset >>. preturn GetSet
                          ; attempt setget >>. preturn GetSet
                          ; tok "get" >>. preturn Get
                          ; tok "set" >>. preturn Set]
  let memberSigP var = parse {
    let! id = identP
    let! (vars,con) = typevarDefnsP <|>% ([],None)
    let! _ = tok ":"
    let! sign = curriedSigP
    let! property = accessorsP <|>% Function
    let t = if vars=[] then Id id else Generic (Id id, vars)
    let t' = match con with
             | None -> t
             | Some(con) -> Constraint(TyparConstraint con,t)
    return cr Constraint (Sig(var,t',sign,property))
  }
  between (tok "(") (tok ")") (memberSigP var)
let singleTypevarP = 
  let varP = typevarP |>> devar
  let constructorP = attempt (tok "(" >>. tok "new" >>. tok ":" >>. tok "unit" >>. tok "->"
                              >>. tok "'" >>. tok "T" >>. tok ")" >>. preturn ())
  let subtypeP var = tok ":>" >>. parse {
    let! t = arrowP
    return cr Constraint (Subtype(var,t))
  }
  let enumP var = tok "enum" >>. parse {
    let! t = between (tok "<") (tok ">") arrowP
    return cr Constraint (Enum(var,t))
  }
  let delegateP var = 
    tok "delegate" >>. between (tok "<") (tok ">") (parse {
      let! leftT = arrowP
      let! _ = tok ","
      let! rightT = arrowP
      return cr Constraint (Delegate(var,leftT,rightT))
    })
  let conP var = 
    tok ":" >>. choice [tok "null" >>. preturn (cr Constraint (Null var))
                       ; tok "struct" >>. preturn (cr Constraint (Struct var))
                       ; constructorP >>. preturn (cr Constraint (DefaultConstructor var))
                       ; tok "not" >>. tok "struct" >>. preturn (cr Constraint (NotStruct var))
                       ; enumP var
                       ; delegateP var
                       ; traitP var
                       ]
  varP >>= (fun var -> conP var <|> subtypeP var)
let typevarChoiceP = parse {
  let! typars = between (tok "(") (tok ")") (sepBy typevarP (tok "or"))
  let typars' = List.map devar typars
  let! _ = tok ":"
  let! tmp = traitP Anonymous
  match tmp (Id "DUMMY") with
  | Constraint(Sig(_,id,mem,prop),_) -> return cr Constraint (Sig(Choice typars',id,mem,prop))
  | _ -> failwith "Holy crap this program is crap"
}  
let typeSuffixP = choice [arrayP
                         ; tok "when" >>. constraintP
                         ; (identP |>> (fun id t -> Generic (Id id,[t])))]
let termP = between (tok "(") (tok ")") arrowP <|> typevarP <|> longidentP
do
  conref := typevarChoiceP <|> singleTypevarP
  typeref := parse {
    let! t = termP
    let! tmp = opt (many typeSuffixP)
    match tmp with 
    | None -> return t
    | Some suffixes -> return nestTypes t suffixes
  }
(* parse a string into a type *)
let parse (s:string) = 
  match FParsec.CharParsers.run (arrowP .>> eof) s with
  | Success(t,(),_) -> t
  | Failure(s,err,()) -> failwith s // or maybe failwith err? I'm not sure
