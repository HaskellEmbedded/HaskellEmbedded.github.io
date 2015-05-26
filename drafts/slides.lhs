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

CλaSH from Christiaan Baaij does this to compile a subset of Haskell to VHDL and SystemVerilog.

# Compiled EDSL

This uses an EDSL (embedded domain-specific language) inside of Haskell to direct the process of code generation to a lower-level representation. (Otherwise called: *compiling*.)

Note that in this case, Haskell code never actually *runs* on the embedded target; it is entirely decoupled from the Haskell runtime.

# Compiled EDSL: Examples

Examples:

- *Atom & Copilot* - Synchronous, deterministic, reactive systems
- *SBV* - Theorem prover oriented around SMT
- *Ivory* - Systems programming
- The entire *Lava* family - Circuit design and verification
- If suitably motivated, one could use Haskell's LLVM libraries for basically this.

# Compiled EDSL: Atom input

```haskell
module Foo where

import Bar
```

# Compiled EDSL: Atom output

(C code)

# Compiled EDSL: Atom byproducts

(timing report or error)

# Why Haskell?

Its type system is powerful enough to help substantially with things like correctness and safety.
