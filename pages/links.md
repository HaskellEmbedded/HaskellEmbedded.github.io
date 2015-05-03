---
title: Links
---

Posts reference these links in context, but they are still useful in a central spot.

# EDSLs: Code Generation
- [Ivory][] ([hackage][ivory-hackage], [GitHub repo][ivory-github]) is a more general EDSL from Galois "for safer systems programming". Ivory is still quite young, but looks very promising.
    - This experience report is probably the best introduction: [Building Embedded Systems with Embedded DSLs (Experience Report)][ivory-paper]
    - Their GitHub repo contains a [paper][ivory-paper2] that is still a work-in-progress.
- [Copilot][] ([hackage][copilot-hackage]) is a "stream language for generating hard real-time C code" also from Galois. Documentation on Copilot is excellent.
    - This report from NASA gives a good explanation: [Experience Report: a Do-It-Yourself High-Assurance Compiler][nasa-copilot]
    - Copilot has backends for both Atom and SBV. Anthony Cowley's talk at NYHUG, [Abstractions for the Functional Roboticist][functionalroboticist-video] (& [slides PDF][functionalroboticist]), talks about both Copilot and SBV, among other things.
- [Atom][atom-hackage] ([GitHub repo][atom-github]) is a language from [Tom Hawkins][] for hard realtime embedded software.
    - Documentation and examples are rather limited, but our [few posts](/tags/atom.html) on Atom compile nearly everything we know on it.
- [SBV][] ([GitHub repo][sbv-github], [GitHub page][sbv-githubio]) is more specific to [SMT][]-based verification in Haskell, but also generates verified C code. Documentation and examples on SBV are excellent.

# Not Haskell per se, but Haskell-inspired and Haskell-hosted
- [Idris][] ([hackage][idris-hackage]) is a language from Edwin Brady, "a general purpose language with full dependent types." It is a recent language, but still has many good papers, talks, and references.
    - The paper from Brady & Hammond, [Resource-safe Systems Programming with Embedded Domain Specific Languages][idris-paper], explains a particular application of Idris that looks very practical.
- [Cryptol][] is yet another Galois creation, a DSL for specifying cryptographic algorithms.

# EDSLs: Circuit Synthesis
- From Christiaan Baaij is [CλaSH: CAES Language for Synchronous Hardware][CλaSH] ([GitHub][clash-github], [hackage][clash-hackage] and its associated [tutorial][clash-tutorial]. CλaSH is a "functional hardware description language that borrows both its syntax and semantics from the functional programming language Haskell."
    - arjanb pointed us to [CλaSH FPGA Starter](https://christiaanb.github.io/posts/clash-fpga-starter/), a quick tutorial for the [Terasic DE0-Nano](https://www.terasic.com.tw/cgi-bin/page/archive.pl?Language=English&CategoryNo=139&No=593).
- Lava is an entire family of Haskell-based DSLs unto itself, but the seminal 1998 paper appears to be [Lava: Hardware Design in Haskell](http://www.cse.chalmers.se/edu/year/2012/course/TDA956/Papers/Lava98.pdf) - which defines it as "a tool to assist circuit designers in specifying, designing, verifying and implementing hardware."
    - One of its authors, Satnam Singh, has a blog entry on it as well: [The Lava Hardware Description Language](http://blog.raintown.org/p/lava.html).

# FPGA/ASIC Implementations
- [The Reduceron](http://www.cs.york.ac.uk/fp/reduceron/) is some very interesting work, forked/revived as a newer [GitHub project](https://github.com/tommythorn/Reduceron).

# Compiling to Embedded Targets
- The [Jhc][]-based [Ajhc][] compiler ([GitHub][ajhc-github], [Hackage][ajhc-hackage]) is a now defunct-project, but seemed to have the aim of compiling Haskell to various other platforms (especially given their "Haskell everywhere" tag).
    - [Ajhc demo for Cortex-M3/4 board][ajhc-demo-cortex-m3] gives their example of compiling Haskell to an ARM Cortex M3/M4.
    - The 2014 paper [Metasepi Report: Writing NetBSD Sound Drivers in Haskell: A Reentrant Haskell Compiler for Operating Systems Programming][ajhc-clh-paper] from Kiwamu Okabe (METASEPI) and Takayuki Muranushi (Kyotu University) talks about the experiences and challenges with using a Haskell compiler for targeting embedded code.
    - Kiwamu Okabe has a number of presentations up at [SlideShare][masterq-slideshare] about his work with METASEPI and Ajhc.

[CλaSH]: http://www.clash-lang.org
[clash-github]: https://github.com/clash-lang
[clash-hackage]: https://hackage.haskell.org/package/clash-ghc
[clash-tutorial]: http://hackage.haskell.org/package/clash-prelude/docs/CLaSH-Tutorial.html
[functionalroboticist]: http://acowley.github.io/NYHUG/FunctionalRoboticist.pdf "Cowley, A. (2014). Abstractions for the Functional Roboticist."
[functionalroboticist-video]: https://vimeo.com/77164337
[atom-github]: https://github.com/tomahawkins/atom
[atom-hackage]: http://hackage.haskell.org/package/atom "atom: A DSL for embedded hard realtime applications. (hackage)"
[copilot-hackage]: http://hackage.haskell.org/package/copilot
[copilot]: https://github.com/leepike/Copilot
[cryptol]: https://github.com/GaloisInc/cryptol
[idris-paper]: http://eb.host.cs.st-andrews.ac.uk/drafts/dsl-idris.pdf "Brady, E. & Hammond, K. Resource-safe Systems Programming with Embedded Domain Specific Languages."
[idris]: http://www.idris-lang.org/
[idris-hackage]: https://hackage.haskell.org/package/idris
[ivory-github]: https://github.com/GaloisInc/ivory
[ivory-hackage]: https://hackage.haskell.org/package/ivory
[ivory-paper]: https://github.com/GaloisInc/smaccmpilot-experiencereport/blob/master/embedded-experience.pdf?raw=true "Hickey, P. C., Pike, L., Elliott, T., Bielman, J., & Launchbury, J. (2014) Building Embedded Systems with Embedded DSLs (Experience Report)."
[ivory-paper2]: https://github.com/GaloisInc/ivory/tree/master/ivory-paper
[ivory]: http://ivorylang.org/
[lava]: http://blog.raintown.org/p/lava.html
[llvm-general]: https://hackage.haskell.org/package/llvm-general
[nasa-copilot]: http://ntrs.nasa.gov/archive/nasa/casi.ntrs.nasa.gov/20120014570.pdf "Pike, L., Wegmann, N., Niller, S., & Goodloe, A. (2012). Experience Report: a Do-It-Yourself High-Assurance Compiler."
[reduceron]: https://github.com/tommythorn/Reduceron
[sbv-github]: https://github.com/LeventErkok/sbv
[sbv-githubio]: https://leventerkok.github.io/sbv/
[sbv]: https://hackage.haskell.org/package/sbv
[smt]: https://en.wikipedia.org/wiki/Satisfiability_Modulo_Theories
[tom hawkins]: http://tomahawkins.org/
[ajhc-demo-cortex-m3]: https://github.com/ajhc/demo-cortex-m3
[Ajhc]: http://ajhc.metasepi.org
[ajhc-github]: https://github.com/ajhc/ajhc
[ajhc-hackage]: https://hackage.haskell.org/package/ajhc
[ajhc-clh-paper]: http://www.metasepi.org/doc/metasepi-icfp2014.pdf "Metasepi Report: Writing NetBSD Sound Drivers in Haskell (Okabe, K. & Muranushi, T.)"
[Jhc]: http://repetae.net/computer/jhc/
[masterq-slideshare]: http://www.slideshare.net/master_q/presentations
