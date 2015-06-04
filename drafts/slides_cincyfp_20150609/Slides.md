% Embedded Haskell, part 1: EDSLs & Metaprogramming
% Chris Hodapp
% CincyFP, 2015-06-09

# Why use Haskell there at all?

> 1. Because you can.
> 2. With apologies to Kernighan & Ritchie: Because C is kind of horrid for this. (And with apologies to no one: Because C++ is also a train-wreck.)
> 3. Because Haskell has a very powerful type system, and this can work wonders when dealing with embedded systems that are notoriously finicky and fragile.
> 4. Because arcane concepts like "functional programming" and "pure functions" and "monads" turn out to be useful for something besides ivory-tower navel-gazing.

# Approaches to embedded Haskell

*Disclaimer:* I made these categories up, and I'm excluding languages like Cryptol and Idris that only incidentally involve Haskell.

- Full compilation, reduced runtime approach
- Static analysis approach
- Compiled EDSL approach (the focus of this presentation)

# Full compilation, reduced runtime

This compiles Haskell code to run directly on an embedded target.  This requires:

- reducing the fairly heavy Haskell runtime,
- optimizing the garbage collection,
- making the lazy evaluation less of a pain.

. . .

Ajhc ([https://github.com/ajhc/ajhc](https://github.com/ajhc/ajhc)), a JHC-derived compiler from Kiwamu Okabe of METASEPI, is the only example of this I found - it could compile and execute on ARM Cortex-M3/M4.  His subsequent switch to the ATS language may be a hint.

# Static analysis

This uses an existing compiler for certain stages (such as the parsing and type-checking), but a custom back-end to actually produce code.  This may adapt or disallow certain constructs.

GHC readily accomodates this by allowing developers to invoke GHC functionality, from Haskell, as a library.  (For instance: *ghcjs*)

. . .

*CλaSH* ([http://www.clash-lang.org/](http://www.clash-lang.org/)) from Christiaan Baaij uses this to compile a subset of Haskell to VHDL and SystemVerilog.  CλaSH disallows certain things: recursive functions, recursive types, side effects, floating-point...

# Compiled EDSL

This uses an EDSL (embedded domain-specific language) inside of Haskell to direct the process of code generation to a lower-level representation. (Otherwise called: *compiling*.)

Note that in this case, Haskell code never actually *runs* on the embedded target.  Rather, it uses your specifications in the EDSL to build a representation of what *will* run there - in other words, a sort of metaprogramming.

The code that runs on the target is entirely decoupled from the Haskell runtime.

# Compiled EDSL: Examples

> - The entire *Lava* family for circuit design and verification.  (Compiles to: VHDL, Verilog, netlist?)
> - *Atom* for reactive, hard real-time, synchronous systems.  (Compiles to: C)
> - *Copilot* for stream-oriented, hard real-time systems.  (Compiles to: C via Atom & SBV, CBMC model checker)
> - *SBV* for theorem proving oriented around SMT.  (Compiles to: C)
> - *Ivory* for safe systems programming.  (Compiles to: C, LLVM(?), CVC4 & ACL2 theorem provers)

# Atom EDSL

The official definition: "Atom is a Haskell EDSL for designing hard realtime embedded software. Based on guarded atomic actions (similar to STM), Atom enables highly concurrent programming without the need for mutex locking. In addition, Atom performs compile-time task scheduling and generates code with deterministic execution time and constant memory use, simplifying the process of timing verification and memory consumption in hard realtime applications. Without mutex locking and run-time task scheduling, Atom eliminates the need and overhead of RTOSes for many embedded applications."

Short version:
Atom is a *synchronous language*: One specifies rules that apply on specific clock ticks, and all state changes are atomic. Feed a specification into Atom, and Atom generates fairly bulletproof, deterministic C code.

# Imaginary Atom scenario

> 1. You have a fairly resource-constrained microcontroller.
> 2. It contains a hardware watchdog timer which you must reset every 15 milliseconds.
> 3. You must monitor an input voltage.  If it ever goes below 1.9 V, then the chip must cease all other operation (aside from resetting the watchdog) until it has stayed above 2.0 V for at least 10 milliseconds.
> 4. There is an input button.  If it is pressed for at least 50 milliseconds, its respective output pin should be toggled.
> 5. There are incidentally 15 other buttons (and respective output pins) that behave the same way.

# My workflow, for those interested

- Haskell code contains:
	- Specifications in Atom & Ivory
    - Build targets for Shake (a Haskell-based build system)	

- Shake build is responsible for:
    - Generating C code with Atom & Ivory
	- Compiling & linking C code with GCC
	- Producing a firmware image and commands for flashing it

- GDB communicates with GDB server
- JLink/OpenOCD implements GDB server and communicates with target via SWD
- Emacs communicates with GDB, handles Haskell and C code, and handles Shake build

# References & Mindless Self-Promotion

Nearly everything that I reference should have a link at: [http://haskellembedded.github.io/pages/links.html](http://haskellembedded.github.io/pages/links.html)

My Atom introduction is at: [http://haskellembedded.github.io/posts/2015-02-17-atom-examples.html](http://haskellembedded.github.io/posts/2015-02-17-atom-examples.html)

In explaining the "How?" and "What?", I probably ignored much of the "Why?", and this explains some of that: [http://haskellembedded.github.io/posts/2015-02-06-how-i-got-here.html](http://haskellembedded.github.io/posts/2015-02-06-how-i-got-here.html)

See the *#haskell-embedded* IRC channel on Freenode to find me (*hodapp*) and a bunch of other people who are way better at this than I am.
