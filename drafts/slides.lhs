% Haskell on Embedded Systems, part 1
% Chris Hodapp
% CincyFP, 2015-06-09

# Approaches to embedded Haskell

*Disclaimer:* I made these categories up, and I'm excluding languages like Cryptol and Idris that only incidentally involve Haskell.

- Full compilation, reduced runtime approach
- Static analysis approach
- Compiled EDSL approach (the focus of this presentation)

# Full compilation, reduced runtime

This compiles Haskell code to run directly on an embedded target.

This requires:

- reducing the fairly heavy Haskell runtime,
- optimizing the garbage collection,
- making the lazy evaluation less of a pain in the ass.

Ajhc from Kiwamu Okabe is the only example of this I found (it could compile and execute on ARM Cortex-M3/M4).  His subsequent switch to the ATS language is probably not a good sign.

# Static analysis

This uses on an existing compiler for certain stages (such as the parsing and type-checking), but a custom back-end to actually produce code.  This often involves compromises on how much of the Haskell language it actually supports; some constructs are disallowed.

GHC readily accomodates this by allowing developers to invoke GHC functionality, from Haskell, as a library.

*CλaSH* from Christiaan Baaij does this to compile a subset of Haskell to VHDL and SystemVerilog.

# Compiled EDSL

This uses an EDSL (embedded domain-specific language) inside of Haskell to direct the process of code generation to a lower-level representation. (Otherwise called: *compiling*.)

Note that in this case, Haskell code never actually *runs* on the embedded target; it is entirely decoupled from the Haskell runtime.

From the target's perspective, nothing about Haskell (run-time, type system, or otherwise) exists.

From Haskell's perspective, the target's runtime exists only in the abstract - it may reason about it in generalities.

# Compiled EDSL: Examples

Examples:

- *Atom & Copilot* - Synchronous, deterministic, reactive systems
- *SBV* - Theorem prover oriented around SMT
- *Ivory* - Systems programming
- The entire *Lava* family - Circuit design and verification
- If suitably motivated, one could use Haskell's LLVM libraries for basically this.

# Atom example (boilerplate)

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

# Atom example (

> tickSecond :: Atom (V Word64)
> tickSecond = do
>   clock <- word64 "clock_sec" 0
>   period 1000 $ exactPhase 0 $ atom "second" $ incr clock
>   return clock

# Compiled EDSL: Atom output

(C code)

# Compiled EDSL: Atom byproducts

(timing report or error)

# Why Haskell?

- It's a suitable host for EDSLs.
- Its type system is powerful enough to help substantially with things like correctness and safety.
