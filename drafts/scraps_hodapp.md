---
title: How I Got Here (i.e. to using Haskell for embedded code)
author: Chris Hodapp
---

Some ideas:

- What my experiences were prior to this
- My results with Ivory
- More specific Ivory examples?  I could probably create a simplified example.
- Atom & concurrency - some examples here would be most helpful.
- What about the Ruby creator quote that I quite like?
- For Ivory: What I ran into with C forcing a coupling between compilation unit and local representation.

Stuff that might be out of the scope of this blog:

- Why did I like Ruby?
- Why did I like Python?
- What did I find interesting about Lisp?

Links to cover:

- http://techoverflow.net/blog/2014/07/28/using-quasiquotation-for-more-readable-atom-code/
- http://techoverflow.net/blog/2014/07/28/yet-another-atom-arduino-blinker/

Big Gaping Holes in This Example
====
Things I still have not touched:

1. Phases (in any useful sense)
2. Probes
3. Assertions
4. Coverage checking
5. What the Atom compiler outputs
6. What generated code looks like
7. Periods overriding later periods
8. The practical notion that you can apply periods and phases to entire sub-rules if you like



TODO below:
 - Links to Copilot calls
 - Functional Roboticist slides

Copilot and Atom share some similarities (besides the fact that Copilot can use Atom for code generation).  Both are synchronous, realtime languages.

As some of the posts on Atom say, Atom's basic unit is a *rule* which involves some atomic operation.  That rule might have restrictions such as being active only at certain clock ticks or under certain conditions.

Copilot's basic unit is a *stream*.  Streams have a value at every clock tick.  That value might come from some built-in streams (such as constant values or clocks), from sampling an external variable or function, or from operations on existing streams (such as adding, logical AND, or referring to past values).  *Triggers* are the mechanism for calling anything external.

Copilot also has some decent papers written about it, and it has two very helpful features that are not present in Atom: pretty-printing the specification ('prettyPrint'), and interpreting the specification ('interpret') for a given number of cycles.
