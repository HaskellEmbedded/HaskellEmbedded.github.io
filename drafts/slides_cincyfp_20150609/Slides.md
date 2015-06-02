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

Ajhc from Kiwamu Okabe is the only example of this I found (it could compile and execute on ARM Cortex-M3/M4).  His subsequent switch to the ATS language may be a hint.

# Static analysis

This uses on an existing compiler for certain stages (such as the parsing and type-checking), but a custom back-end to actually produce code.  This may adapt or disallow certain constructs.

GHC readily accomodates this by allowing developers to invoke GHC functionality, from Haskell, as a library.

*CλaSH* from Christiaan Baaij uses this to compile a subset of Haskell to VHDL and SystemVerilog.  CλaSH disallows certain things: recursive functions, recursive types, side effects, floating-point...

# Compiled EDSL

This uses an EDSL (embedded domain-specific language) inside of Haskell to direct the process of code generation to a lower-level representation. (Otherwise called: *compiling*.)

Note that in this case, Haskell code never actually *runs* on the embedded target.  Rather, it uses your specifications in the EDSL to build a representation of what *will* run there.

The code that runs on the target is entirely decoupled from the Haskell runtime.

# Compiled EDSL: Examples

Examples:

- *Atom* for reactive, hard real-time, synchronous systems,
- *Copilot* for stream-oriented, hard real-time systems,
- *SBV* for theorem proving oriented around SMT,
- *Ivory* for safe systems programming,
- The entire *Lava* family for circuit design and verification.

# Atom EDSL

The official definition: "Atom is a Haskell DSL for designing hard realtime embedded software. Based on guarded atomic actions (similar to STM), Atom enables highly concurrent programming without the need for mutex locking. In addition, Atom performs compile-time task scheduling and generates code with deterministic execution time and constant memory use, simplifying the process of timing verification and memory consumption in hard realtime applications. Without mutex locking and run-time task scheduling, Atom eliminates the need and overhead of RTOSs for many embedded applications."

Short version:
Atom is a *synchronous language*: One specifies rules that apply on specific clock ticks, and all state changes are atomic. Feed a specification into Atom, and Atom generates fairly bulletproof, deterministic C code.

# Atom example

Ponder the following imaginary scenario:

1. You have a fairly resource-constrained microcontroller.
2. It contains a hardware watchdog timer which you must reset every 5 milliseconds.
3. You must monitor an input voltage.  If it ever goes below 1.9 V, then the chip must cease all other operation (aside from resetting the watchdog) until it has stayed above 2.0 V for at least 10 milliseconds.
4. There is an input button.  If it is pressed for at least 50 milliseconds, its respective output pin should be toggled.
5. There are incidentally 15 other buttons (and respective output pins) that behave the same way.

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

# References

Nearly everything that I reference should have a link at:

[http://haskellembedded.github.io/pages/links.html](http://haskellembedded.github.io/pages/links.html)

An Atom introduction that I wrote is at:

[http://haskellembedded.github.io/posts/2015-02-17-atom-examples.html](http://haskellembedded.github.io/posts/2015-02-17-atom-examples.html)

In explaining the "How?" and "What?", I probably ignored much of the "Why?", and I wrote a post here that explains some of this:

[http://haskellembedded.github.io/posts/2015-02-06-how-i-got-here.html](http://haskellembedded.github.io/posts/2015-02-06-how-i-got-here.html)
