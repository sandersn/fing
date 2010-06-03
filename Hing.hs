import Text.ParserCombinators.Parsec
import Control.Monad (sequence, msum, liftM, liftM2, when)
import Data.Either (rights)
import Data.List (intercalate)
import Data.Maybe (isJust)
-------------
list x = [x]
devar (Var var) = var
--------------
tokeniser = choice [try (many1 (digit <|> letter <|> oneOf ".`_"))
                   , string "->"
                   , try (string ":>")
                   , (return . list =<< oneOf "*<>()[],'^#:?")
                   ]
tok t = try $ do
  spaces
  x <- tokeniser
  if x==t then return x else fail "Token not recognised"
toks = mapM tok
notTok ts = try $ do
  spaces
  x <- tokeniser
  if x `elem` ts then fail "Incorrect token" else return x
---------------
data Typ = Arrow [Typ]
         | Tuple [Typ]
         | Var Typar
         | Id String
         | NamedArg String Typ Bool
         | Generic Typ [Typ]
         | Array Int Typ
         | Constraint When Typ
           deriving Show
data Typar = Anonymous
           | Normal String
           | Structural String
           | Choice [Typar] -- a choice between type variables.
             deriving Show
data When = Null Typar
          | Struct Typar
          | NotStruct Typar
          | DefaultConstructor Typar
          | Enum Typar Typ
          | Delegate Typar Typ Typ
          | Subtype Typar Typ
          | Sig Typar Typ Typ Property
          | TyparConstraint Typ -- s.t. Typ = Constraint
            deriving Show
data Property = Function | Get | Set | GetSet deriving Show
------------------------
arrowP = liftM (toplevel Arrow) $ sepBy1 tupleP (tok "->")
tupleP = liftM (toplevel Tuple) $ sepBy1 typeP (tok "*")
toplevel _ [] = error "This can't happen because it's sepBy_1_"
toplevel _ [passthrough] = passthrough
toplevel typ kids = typ kids
typeP = do
  t <- termP
  option t (return . nestTypes t . reverse =<< many typeSuffixP)
nestTypes = foldr ($)
termP = typevarP <|> between (tok "(") (tok ")") arrowP <|> longidentP
typevarP = choice [anon, normal, structural]
  where anon = tok "_" >> return (Var Anonymous) -- 4 TEH LULZ
        normal = tok "'" >> liftM (Var . Normal) identP
        structural = tok "^" >> liftM (Var . Structural) identP
        -- TODO: Assert that it's only digits and letters
identP = notTok ["lazy", "with", "when", "if", "else", "do", "new", "get", "set"
                , "<", ">", "[", ",", "]", "(", ")", "#", ":", "?"
                , "->", "*", "'", "_", "^", ":>"]
longidentP = do
  id <- identP
  option (Id id) (postfixNameP (Id id))
postfixNameP id =
  liftM (Generic id) $ between (tok "<") (tok ">") (sepBy arrowP (tok ","))
typeSuffixP = choice [arrayP <?> "an array type"
                      , tok "lazy" >> return (Generic (Id "lazy") . list)
                      , tok "when" >> constraintP
                      , (do id <- identP; return (Generic (Id id) . list))
                                  <?> "a stupid suffix type"]
arrayP = do
  dims <- between (tok "[") (tok "]") (many (tok ","))
  return $ Array (1+length dims)
constraintP = typevarChoiceP <|> singletypevar
  where singletypevar = do
          Var var <- typevarP
          subtype var <|> (tok ":" >>
                           choice [tok "null"
                                   >> return (Constraint (Null var))
                                  , tok "struct"
                                   >> return (Constraint (Struct var))
                                  ,try (toks (words "( new : unit -> ' T )")
                                        >> return (Constraint (DefaultConstructor var)))
                                  ,  toks (words "not struct")
                                   >> return (Constraint (NotStruct var))
                                  , enum var
                                  , traitP var
                                  , delegate var])
        subtype var = do
          tok ":>"
          t <- arrowP
          return $ Constraint (Subtype var t)
        enum var = do
          tok "enum"
          t <- between (tok "<") (tok ">") arrowP
          return $ Constraint (Enum var t)
        delegate var = do
          tok "delegate"; tok "<"
          leftT <- arrowP
          tok ","
          rightT <- arrowP
          tok ">"
          return $ Constraint (Delegate var leftT rightT)
traitP var =
  between (tok "(") (tok ")") (memberSigP var)
typevarChoiceP = do
  typars <- between (tok "(") (tok ")") (sepBy typevarP (tok "or"))
  let typars' = map devar typars
  tok ":"
  Constraint (Sig _ id member prop) _ <- liftM ($ Id "DUMMY") (traitP Anonymous)
  return $ Constraint (Sig (Choice typars') id member prop)
memberSigP var = do
  id <- identP
  (vars,constraint) <- option ([],Nothing) typevarDefnsP
  tok ":"
  sig <- curriedSigP
  property <- option Function accessorsP
  let t = if null vars then Id id else Generic (Id id) vars
  case constraint of
    Nothing -> return $ Constraint (Sig var t sig property)
    Just con -> return $ Constraint (Sig var
                                         (Constraint (TyparConstraint con) t)
                                         sig
                                         property)
typevarDefnsP = do
  tok "<"
  defns <- sepBy1 typevarP (tok ",")
  constraints <- optionMaybe typevarConstraintsP
  tok ">"
  case (defns,constraints) of
    (_,Nothing) -> return (defns, Nothing)
    ([defn],Just cs) -> return (defns,Just $ nestTypes defn cs)
    (defns,Just cs) ->
      return (defns,Just$nestTypes (Var (Choice (map devar defns))) cs)
typevarConstraintsP = do
  tok "when"
  sepBy1 constraintP (tok "and")
curriedSigP = do
  arg <- argsSpecP
  tok "->"
  args <- sepBy1 argsSpecP (tok "->")
  return $ Arrow (arg:args)
argsSpecP = do
  args <- sepBy1 argSpecP (tok "*")
  case args of
    [arg] -> return arg
    args -> return $ Tuple args
argSpecP = do
  argspec <- optionMaybe (try argNameSpecP)
  t <- typeP
  case argspec of
    Just (optional,name) -> return $ NamedArg name t optional
    Nothing -> return t
argNameSpecP = do
  optional <- optionMaybe (tok "?")
  argname <- identP
  tok ":"
  return $ (isJust optional, argname)
accessorsP = do
  tok "with"
  choice [try (toks (words "get , set") >> return GetSet)
         , try (toks (words "set , get") >> return GetSet)
         , tok "get" >> return Get
         , tok "set" >> return Set]
--------------
run s = parse (do t <- arrowP; eof; return t) "F#" s
passes = ["int"
         , "int->int"
         , "int->int->int"
         , "'a"
         , "int*int"
         , "int->int*int->int"
         , "int*int->int*int"
         , "'a->'a"
         , "'a*'a"
         , "_*^a"
         , "(int)"
         , "(int->int)"
         , "('a->'a)"
         , "('a*_)->(int->^a)"
         , "(((int)))"
         , "Microsoft.FSharp.Core.double"
         , "Microsoft.FSharp.Core.list`1"
         , "list"
         , "list<>"
         , "list<int>"
         , "list<int->int>"
         , "list<int*int>"
         , "list<int*(int->int*int)>"
         , "list<list<int>>"
         , "list<_>"
         , "list<_,_>"
         , "list<int,'a,_>"
         , "(int) lazy"
         , "(int->int) lazy"
         , "('a->'a) lazy"
         , "('a*_)->(int->^a) lazy"
         , "(('a*_)->(int->^a)) lazy"
         , "(((int))) lazy"
         , "Microsoft.FSharp.Core.double lazy"
         , "Microsoft.FSharp.Core.list`1 lazy"
         , "list lazy"
         , "list<'a> lazy"
         , "list lazy lazy"
         , "int list"
         , "int list lazy"
         , "int list list"
         , "int list list lazy"
         , "int[]"
         , "int[,]"
         , "int[,,,,,,,]"
         , "int[,] lazy"
         , "int[,,] lazy"
         , "list<'a> when 'a : null"
         , "list<'a> when 'a : null lazy"
         , "list<'a> lazy when 'a : null"
         , "list<'a> when 'a : (new : unit -> 'T)"
         , "list<'a> when 'a : struct"
         , "list<'a> when 'a : not struct"
         , "list<'a> when 'a : enum<int>"
         , "list<'a> when 'a : delegate<int,'a>"
         , "list<'a> when 'a :> Microsoft.Collections.IComparable"
         , "list<'a,'b> when 'a :> 'b"
         , "list<'a> when 'a : (read : string->int with get)"
         , "list<'a> when 'a : (read : string->int with set)"
         , "list<'a> when 'a : (read : string->int with get,set)"
         , "list<'a> when 'a : (read : string->int with set,get)"
         , "list<'a> when ('a or 'b) : (read : string->int->stream)"
         , "list<'a> when 'a : (read<'b,'c> : 'a -> 'b -> 'c)"
         ]
         ++ ["list<'a> when 'a : ("++m++")" | m <- memberpasses]
fails = ["with"
        ,"do"
        ,"if"
        ,"when"]
memberpasses = ["read : stream->int->string"
               , "read : stream->int list->string"
               , "read : stream->int lazy->string"
               , "read : stream->list<int>->string"
               , "read : stream->int[]->string"
               , "read : stream->int[,,]->string"
               , "read : stream->'a->string"
               , "read : stream->'a when 'a : (read : int->string)->string"
               , "read : string*stream->int"
               , "read : ?foo:int->int"
               , "read : ?foo:int->bar:int->int"
               ] ++ ["read"++t++" : 'a->'b" | t <- typedefpasses]
memberfails = ["read : string*stream"
              , "read : stream"
              , "read : stream->when->int"
              , "read : stream->with->int"
              , "read : when->with->if"
              ]
typedefpasses = ["<'a>"
                , "<'a,'b>"
                , "<'a,'b when 'a : null>"
                , "<'a,'b when 'a : (new : unit -> 'T)>"
                , "<'a,'b when 'a : struct>"
                , "<'a,'b when 'a : not struct>"
                , "<'a,'b when 'a : enum<int> >"
                , "<'a,'b when 'a : enum<int lazy> >"
                , "<'a,'b when 'a : delegate<int,dword> >"
                , "<'a,'b when 'a : (write : int->baz)>"
                , "<'a,'b when 'a : (write : foo:int->bar:baz)>"
                , "<'a,'b when 'a :> int>"
                , "<'a,'b when 'a :> int lazy>"
                , "<'a,'b when 'a : null and 'b : null>"
                , "<'a when 'a : null and 'a : null>"
                , "<'a,'b when 'a :> 'b>"
                , "<'a,'b when 'a : null and 'a :> 'b>"
                , "<'a when 'a : null and 'a :> 'b>"
                ]
main = do
  putStrLn "    Full tests"
  mapM_ display passes
  putStrLn "    Full fails"
  mapM_ (print :: Typ -> IO ()) $ rights $ map run fails
  {- putStrLn "    Member Sig fails"
  mapM_ print $ rights $ map rms memberfails -}
  where display test = do
          putStrLn $ "    " ++ test
          (print :: Either ParseError Typ -> IO ()) $ run test
