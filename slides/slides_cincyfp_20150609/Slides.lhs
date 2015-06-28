% Haskell on Embedded Systems, part 1
% Chris Hodapp
% CincyFP, 2015-06-09

Approaches to embedded Haskell
====

*Disclaimer:* I made these categories up, and I'm excluding languages like Cryptol and Idris that only incidentally involve Haskell.

- Full compilation, reduced runtime approach
- Static analysis approach
- Compiled EDSL approach (the focus of this presentation)

Full compilation, reduced runtime
====

This compiles Haskell code to run directly on an embedded target.

This requires:

- reducing the fairly heavy Haskell runtime,
- optimizing the garbage collection,
- making the lazy evaluation less of a pain in the ass.

Ajhc from Kiwamu Okabe is the only example of this I found (it could compile and execute on ARM Cortex-M3/M4).  His subsequent switch to the ATS language may be a hint.

Static analysis
====

This uses on an existing compiler for certain stages (such as the parsing and type-checking), but a custom back-end to actually produce code.  This may adapt or disallow certain constructs.

GHC readily accomodates this by allowing developers to invoke GHC functionality, from Haskell, as a library.

*CλaSH* from Christiaan Baaij uses this to compile a subset of Haskell to VHDL and SystemVerilog.  CλaSH disallows certain things: recursive functions, recursive types, side effects, floating-point...

Compiled EDSL
====

This uses an EDSL (embedded domain-specific language) inside of Haskell to direct the process of code generation to a lower-level representation. (Otherwise called: *compiling*.)

Note that in this case, Haskell code never actually *runs* on the embedded target.  Rather, it uses your specifications in the EDSL to build a representation of what *will* run there.

The code that runs on the target is entirely decoupled from the Haskell runtime.

Compiled EDSL: Examples
====

Examples:

- *Atom* for reactive, hard real-time, synchronous systems,
- *Copilot* for stream-oriented, hard real-time systems,
- *SBV* for theorem proving oriented around SMT,
- *Ivory* for safe systems programming,
- The entire *Lava* family for circuit design and verification.

Atom example (boilerplate)
====

> import Language.Atom
> 
> main :: IO ()
> main = do
>    (sched, _, _, _, _) <- compile "atom_example" atomCfg example
>    putStrLn $ reportSchedule sched
>    
> atomCfg :: Config
> atomCfg = defaults { cFuncName = "atom_tick"
>                    , cStateName = "state_example"
>                    , cCode = prePostCode
>                    , hCode = prePostHeader
>                    , cRuleCoverage = False
>                    }

Atom example
====

> tickSecond :: Atom (V Word64)
> tickSecond = do
>   clock <- word64 "clock_sec" 0
>   period 1000 $ exactPhase 0 $ atom "second" $ incr clock
>   return clock

Compiled EDSL: Atom output
====

(C code)

Compiled EDSL: Atom byproducts
====

(timing report or error)

My workflow, for those interested
====

Emacs has some lovely Haskell support: [https://github.com/haskell/haskell-mode](https://github.com/haskell/haskell-mode)

References
====

Nearly everything that I reference should have a link at:

[http://haskellembedded.github.io/pages/links.html](http://haskellembedded.github.io/pages/links.html)

An Atom introduction that I wrote is at:

[http://haskellembedded.github.io/posts/2015-02-17-atom-examples.html](http://haskellembedded.github.io/posts/2015-02-17-atom-examples.html)

In explaining the "How?" and "What?", I probably ignored much of the "Why?", and I wrote a post here that explains some of this:

[http://haskellembedded.github.io/posts/2015-02-06-how-i-got-here.html](http://haskellembedded.github.io/posts/2015-02-06-how-i-got-here.html)
