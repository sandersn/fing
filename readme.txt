This is the first release of Fing. It is very rudimentary and not really
ready for public viewing.

However, because Git makes everything public immediately, here are some notes:

The code depends on FParsec, which is included, and the FSharp Powerpack,
which is not. It is released under the revised BSD licence.

It is not yet friendly for setup, but to make things work, you need to
install the FSharp Powerpack. This is easy to find on Codeplex. Make sure
that the project refers to it once it is installed.

If you run FingTest/Script.fsx, you will get a REPL that is ready for some
interactive testing. This is really the only way to get answers besides
editting Fing.main.

Use 
 - Fing.find to return a list of results
 - Fing.textFind to print a formatted list of results
 - Fing.nameFind to search for functions by name

Current shortcomings in the code:
 - Tests lag implementation -- I decided that I needed to stop delaying
   and get something working fast, even if it's not working for some cases.
 - There is some dead code -- The original toy version posted by kvb on
   Stack Overflow has already some fancy type unification. This will be useful
   some day but for now I moved it into Unify, because it returns many
   unexpected search results for the simple case.
   Eventually I will need to run multiple searches in parallel and probably
   rank the unified and inherited results below the simple ones.
 - Too many files are included--for example, all of FParsec.
 - The licence is not made clear in every file. This code is released under
   the revised BSD licence, but this is the only file that mentions that.
   
Current shortcomings in the implementation:
 - Only searches F# Core.
 - No indexing.
 - Only textual interface. Actually, it is just an API with some text output.
   You have to edit main to search for something else.
 - Abbreviations are hard-coded (incorrectly), so some will not work,
   eg I know that I have not added 'decimal' yet.
 - No fancy type relations work yet, so you can't get Seq module answers
   for a List search.