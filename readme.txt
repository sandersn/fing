Fing is F# API Search. It's inspired by Hoogle, although it is
not a port. Fing has two uses. The obvious is to search for a function by name:

    $ fing abs
    
    abs
    Results:
            Operators.abs           (int -> int)

But Fing is not really needed for this; Bing (or Google) will just do as well
with "msdn abs". Although, it is nice for symbols:

    $ fing "( ~~~ )"
    ( ~~~ )
    Results:
            Operators.( ~~~ )               (int -> int)

Fing is really useful when you are about to write something
that is likely to exist already in the standard library. Say you
want a function that looks for an item in a list that satisfies some
property and returns None if it can't find one. The type of this function is

    ('a -> bool) -> list<'a> -> 'a option
    (the property)  (the list)  (the optional return value)

So you say

    $ fing "('a -> bool) -> list<'a> -> 'a option"
    ('a -> bool) -> list<'a> -> 'a option
    Results:
	    List.tryFind		(('a -> bool) -> list<'a> -> option<'a>)
	
Fing has a full parser and ignores argument order, so you can also search for:

    'T list -> ('T -> bool) -> 'T option

Or even silly variants like:

    (('foonly3000 -> bool) -> ('foonly3000 list) -> ('foonly3000 option))

To reference another F# assembly, use the argument -r or --reference:

    $ fing -r FParsec.dll "char -> bool"

The placement and usage of the references is flexible, 
you can pass at the end too:

    $ fing -r FParsec.dll "char -> bool" --reference=SomethingElse.dll

However, (de)aliasing doesn't work properly yet, so not all types may be searchable.
Aliases with type variables do not work yet. For example, this means that very little
of FParsec is searchable because it relies heavily on the alias:

    type Parser<'t,'u> = State<'t> -> Reply<'u,'t>

API:

The above functionality is available from the API:

Use 
 - Fing.search : string -> seq<Fing.Result>
 - Fing.textSearch : string -> unit, to print a formatted list of results
 - Fing.nameFind : string -> seq<Fing.Result>, to search for functions by name only
 - Fing.typeFind : string -> seq<Fing.Result> , to search for functions by type only
 - Fing.addReferences : seq<string> -> unit, to add references by filename
   (there is no way to add references by directly passing assemblies yet)
---------------

What's new in version 0.1:

 1. Refer to assemblies other than FSharp.Core
 2. Search for arguments in any order.
 3. Assembly information is parsed eagerly, meaning better performance in fsi
    but longer startup time and more memory usage.
 4. Fewer bugs due to better tests! OK, better test.
 5. Fixed some of the manual aliases.

---------------

Fing is still not very friendly for easy setup of the source.
Here are some notes:

The code depends on FParsec, which is included, and the FSharp Powerpack,
which is not. It is released under the new BSD licence. 
See license.txt for details.

You will need to install the FSharp Powerpack. 
This is easy to find on Codeplex. Make sure
that the project refers to it once it is installed.

If you run FingTest/Script.fsx, you will get a REPL that is ready for some
interactive testing.
   
Current shortcomings in the implementation:
 - No indexing or pickling, so there is a delay at startup.
   (This may only be noticeable on old VMs like the one I develop on currently.)
 - Abbreviations are hard-coded (incorrectly), so some will not work,
   eg I know that I have not added 'decimal' yet.
 - No fancy type relations work yet, so you can't get Seq module answers
   for a List search.

Current shortcomings in the code:
 - Tests lag implementation:
   * The parser is pretty well tested, at least for positive examples.
   * A smoke test checks that every function in FSharp.Core can be found,
     at least if you know the correct argument order.
   * SubstTest fails, but Unify.subst is currently unused.
   * Otherwise, unit tests are basically non-existent.
 - There is some dead code -- The original toy version posted by kvb on
   Stack Overflow has already some fancy type unification. This will be useful
   some day but for now I moved it into Unify, because it returns many
   unexpected search results for the simple case.
   Eventually I will need to run multiple searches in parallel and probably
   rank the unified and inherited results below the simple ones.
 - Too many files are included--for example, all of FParsec.
