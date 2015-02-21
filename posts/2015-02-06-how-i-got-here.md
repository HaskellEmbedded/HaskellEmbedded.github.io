---
title: How I Got Here (i.e. to using Haskell for embedded code)
date: February 6th, 2015
author: Chris Hodapp
tags: haskell, atom, ivory, copilot, idris, sbv, python
---

One work project since near the begining of 2014 has been the design of an embedded system for which power usage and stability are two primary concerns. It's not controlling an airplane or a nuclear plant - malfunctions will not be catastrophic - but stability still matters.

My boss had decided that for various reasons too numerous and extensive to list here, we needed to have support for Bluetooth Low Energy. Our system at the time ran on the venerable TI MSP430 and I started looking at ways to support BLE from this. All of TI's BLE offerings at the time ran on the 8051 and appeared to require proprietary toolchains. After surveying what else was available, we had eventually decided on the [Nordic nRF51822](https://www.nordicsemi.com/eng/Products/Bluetooth-Smart-Bluetooth-low-energy/nRF51822), a microcontroller based on an ARM Cortex-M0, supported by standard GCC, and providing all the necessary peripherals for BLE - and simply eliminated the MSP430 altogether.

It quickly evolved to a distributed system - some parts were sensors or actuators that were BLE peripherals, another part ran constantly as a BLE central, another part ended up as a more powerful, Internet-connected Linux machine that ran only sparingly.

A few patterns emerged:

- All parts needed to be speaking the same protocol, which necessitated keeping various structures and constants in sync at the software each part ran.
- The nRF51822 board acting as the BLE central was responsible for juggling a lot of concurrent operations, because nearly all API calls were async.
- The nRF51822 boards ran different software, but still shared various bits of code and constants.

The two languages here were C for the nRF51822, and Python for the Linux machine. I tried to build abstractions that could sensibly represent these patterns, but were sufficiently lightweight in C. One was in Python to keep data types in sync; for Linux, it handled it at runtime in Python, and for the microcontroller, it generated C code with the requisite `memcpy` calls and proper offsets and lengths. The C code also had various FSMs (some explicit, some not) for managing the concurrent operations. At later examination they were nearly all ad-hoc coroutines that were 'hiding' a very simple concurrent description in their details.

Seeing that I had ad-hoc coroutines hidden everywhere in my code, I tried to integrate in some lightweight RTOSes that provided some form of cooperative thread or coroutine. I looked at [Atomthreads](http://atomthreads.com/), [RIOT OS](http://www.riot-os.org/), [Contiki](http://www.contiki-os.org/), [FreeRTOS](http://www.freertos.org/), and [ChibiOS/RT](http://www.chibios.org/dokuwiki/doku.php), but the issue that stopped me in all cases was that they appeared they would interfere with Nordic's firmware, which I required for the Bluetooth Low Energy functionality. Finally, I discovered [cocoOS](http://www.cocoos.net/intro.html) which provided just the cooperative threads and basic messaging primitives, and due to its simplicity required only minimal implementation and nothing very low-level. I quickly ported it and it was working seemingly wonderfully.

I had recently read Joe Armstrong's famous [thesis on Erlang](http://www.erlang.org/download/armstrong_thesis_2003.pdf), and found it very cogent and pragmatic, but still accessible. He defines on page 19 *Concurrency Oriented Programming*. I had a thought that perhaps cocoOS gave me the primitives I needed to let me apply the methodology that Armstrong described, and express the system as a design around concurrent processes. With clean system design in hand and hopeful visions of elegance in my head, I set about working on this - until reaching an abrupt ending when I ran out of memory with around 2 cooperative tasks and almost nothing else compiled in.

Around the same point, I had also extended my Python code generation to generate code for some more complex protocols and refactored it to make use of a very handy library, [Cog](https://www.python.org/about/success/cog/).

At some point I saw that these all were sides of the same ugly dilemma (trilemma?):

- I could write larger amounts of code that would 'hide' the simpler representations inside.
- I could attempt to streamline the above with code generation via macros.
- I could build or use runtime abstractions that helped me approximate those simpler representations, but incurred too much overhead.

`#define` and `#include` existed, but they addressed only the smallest of cases, and I felt like C++ was no cure either. Cog helped streamline the code generation, but my code generation still felt like primitive macros. My view of code generation in general was grim too, from seeing its use at other jobs. In particular:

1. The need for automatically-generated code probably means that the underlying language lacks the right abstractions (or that you do not understand them).
2. Either you incur the complexity of implementing the code generation in that same (lacking or ill-understood) language, or you incur the mental overhead of having to coordinate with another language at the same time - possibly a third language, if you handle specifications in a separate language.
3. You don't fix a problem in automatically-generated code - you fix the code generator or the specifications, which is nearly always more difficult and has more consequences.
4. The code generator's scope tends to expand anywhere generated code touches manually-written code.

Of course, avoiding code generation completely is senseless. Compilers are code generators. They run on CPUs probably created (gate-wise) by some form of code generator. The problem is not code generation, but code generation as ad-hoc automation that buries complexity underneath a layer of fresh complexity.

Luckily, plenty of people (besides compiler writers) seem to get code generation right. After my failure to make any progress with cocoOS, I had started reading about [Atom](https://github.com/tomahawkins/atom), an EDSL in Haskell which generates C code for hard realtime embedded software. It looked very promising and solid, but too narrow in scope for what I needed. Shortly after, I found [Copilot](https://github.com/leepike/Copilot), another Haskell EDSL for some similar purposes which generates C either via Atom or [SBV](https://hackage.haskell.org/package/sbv) (also a Haskell EDSL).
[Experience Report: a Do-It-Yourself High-Assurance Compiler](http://ntrs.nasa.gov/archive/nasa/casi.ntrs.nasa.gov/20120014570.pdf) from NASA focuses on Copilot, but identifies some general benefits from the paradigm; it is a short, informative read that I highly recommend.

I stumbled into [Ivory](http://ivorylang.org/) from Galois Inc. (another Haskell EDSL, of course) and knew quickly that I had found something very powerful for my own use-case. Their [experience report](https://github.com/GaloisInc/smaccmpilot-experiencereport/blob/master/embedded-experience.pdf?raw=true) is a quick read, but a good summary of Ivory (as well as [Tower](http://ivorylang.org/tower-overview.html), yet another Haskell EDSL). It's still a rather young project, and documentation and examples are sparse. However, it is under active development, and its developers (particularly Lee Pike, who I've bothered with plenty of emails) have been very responsive to my inquiries.

(To be clear, these all are Haskell EDSLs oriented to code generation. All of them in some fashion allow the programmer to express a specification within Haskell, and then generate C code at runtime. Haskell code is not actually being built for any embedded target - although, some authors here had interest in doing this via [JHC](http://repetae.net/computer/jhc/), and the [Idris](http://www.idris-lang.org/) language extracts the entire runtime and program to C with some recent work going into making this light enough for embedded targets.  A certain [Dr. Franco](http://gauss.ececs.uc.edu/franco_files/franco.html) at my university insisted to me several years ago that code generation from a specification was the way of the future, and was most pleased when I suggested this year that perhaps he was correct.)

After some more reading and examination here, I started migrating the code over to a combination of Atom and Ivory. Atom quickly proved useful for managing a lot of the concurrency that was present, and generally handling high-level scheduling. The transition from C code to Ivory was rough in spots, but Ivory proved very useful too for representing all kinds of general functionality.

Haskell's power as a language has been proving immensely useful here. Effectively, the EDSLs have moved the level of meaningful abstraction up into code-generation, and this has both given me much better abstraction and let me eliminate some runtime overhead.

More in-depth examples and explanations on things like Atom, Copilot, Ivory, and SBV will follow on this blog.

# References
## Papers
1. Armstrong, J. Making reliable distributed systems in the presence of software errors. [http://www.erlang.org/download/armstrong_thesis_2003.pdf](http://www.erlang.org/download/armstrong_thesis_2003.pdf)
2. Pike, L., Wegmann, N., Niller, S., & Goodloe, A. (2012). Experience Report: a Do-It-Yourself High-Assurance Compiler. [http://ntrs.nasa.gov/archive/nasa/casi.ntrs.nasa.gov/20120014570.pdf](http://ntrs.nasa.gov/archive/nasa/casi.ntrs.nasa.gov/20120014570.pdf)
3. Hickey, P. C., Pike, L., Elliott, T., Bielman, J., & Launchbury, J. (2014) Building Embedded Systems with Embedded DSLs (Experience Report). *ACM 978-1-4503-2873-9/14/09.* [https://github.com/GaloisInc/smaccmpilot-experiencereport/blob/master/embedded-experience.pdf?raw=true](https://github.com/GaloisInc/smaccmpilot-experiencereport/blob/master/embedded-experience.pdf?raw=true)

## Websites
1. cocoOS [http://www.cocoos.net/intro.html](http://www.cocoos.net/intro.html)
2. Cog: Python Success Stories [https://www.python.org/about/success/cog/](https://www.python.org/about/success/cog/)
3. atom: A DSL for embedded hard realtime applications. [http://hackage.haskell.org/package/atom](http://hackage.haskell.org/package/atom) and [https://github.com/tomahawkins/atom](https://github.com/tomahawkins/atom)
4. sbv: SMT Based Verification: Symbolic Haskell theorem prover using SMT solving. [https://hackage.haskell.org/package/sbv](https://hackage.haskell.org/package/sbv) and [https://leventerkok.github.io/sbv/](https://leventerkok.github.io/sbv/)
5. copilot: A stream DSL for writing embedded C programs. [http://hackage.haskell.org/package/copilot](http://hackage.haskell.org/package/copilot) and [https://leepike.github.io/Copilot/](https://leepike.github.io/Copilot/)
6. Ivory, an eDSL for safe systems programming. [http://ivorylang.org/](http://ivorylang.org/) and [https://github.com/GaloisInc/ivory](https://github.com/GaloisInc/ivory)
7. Jhc Haskell Compiler. [http://repetae.net/computer/jhc/](http://repetae.net/computer/jhc/)
