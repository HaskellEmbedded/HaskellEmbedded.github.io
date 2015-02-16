---
title: Examples for the Atom library (DRAFT)
author: Chris Hodapp
---

In the [last post](./2015-02-06-how-i-got-here.html) I talked briefly about [Atom](http://hackage.haskell.org/package/atom), a Haskell EDSL from [Tom Hawkins](http://tomahawkins.org/) for hard realtime embedded software. I hope to go into a little more detail here.

Background
==========

As that post mentioned, I was running into problems handling concurrency in software for my embedded target (a [Nordic nRF51822](https://www.nordicsemi.com/eng/Products/Bluetooth-Smart-Bluetooth-low-energy/nRF51822)). My embedded software had operations throughout that required some fairly specific timing, for instance:

1. Activate some circuit.
2. Wait *at least* 300 milliseconds for that circuit to settle.
3. Trigger the circuit to send a pulse.
4. Wait *exactly* 145 microseconds, and set circuit to receive mode.
5. Wait *at least* a millisecond.
6. Send a command over SPI to trigger a reading.
7. Wait *at most* 1 millisecond for an acknowledgement.
8. Repeat 6 and 7 to read the contents of 3 or 4 other registers.

This was simple enough, except that the processor had to share a dozen similar processes simultaneously. The delay function could delay precisely, but by busy-looping, blocking other execution and wasting power, a precious resource here. Timer callbacks and state machines could share execution delay for longer periods, but at the cost of extra complexity.

I looked at a variety of solutions which implemented schedulers that ran directly on the target, but nearly all of them appeared to either interfere with Nordic's firmware or require too extensive of an implementation. The one that I finally could port, [cocoOS](http://www.cocoos.net/intro.html), had the rather show-stopping issue that its primitives were simply using more resources than I had available.

Introduction to Atom
====================

At this point I started looking at Atom. Its documentation is a bit lacking, and with no updates on its Hackage page in 2 years, I expect no change in that - which is a shame, because this is substantial industry work that was open-sourced. I'm slowly understanding it from what I can find, mainly:

- Hackage documentation:
    - [Language.Atom.Code](http://hackage.haskell.org/package/atom-1.0.12/docs/Language-Atom-Code.html)
    - [Language.Atom.Common](http://hackage.haskell.org/package/atom-1.0.12/docs/Language-Atom-Common.html)
    - [Language.Atom.Example](http://hackage.haskell.org/package/atom-1.0.12/docs/src/Language-Atom-Example.html)
    - [Language.Atom.Expressions](http://hackage.haskell.org/package/atom-1.0.12/docs/Language-Atom-Expressions.html)
    - [Language.Atom.Language](http://hackage.haskell.org/package/atom-1.0.12/docs/Language-Atom-Language.html)
- some slides from creator Tom Hawkins in 2008, [Controlling Hybrid Vehicles with Haskell](http://cufp.galois.com/2008/slides/HawkinsTom.pdf). Some names in the example code have since changed, such as `atom` replacing `system`.
- a more formal example from Lee Pike, [An Atomic Fibonacci Server: Exploring the Atom (Haskell) DSL](https://leepike.wordpress.com/2009/05/05/an-atomic-fibonacci-server-exploring-the-atom-haskell-dsl/). His links to code examples appear to be down, but the Wayback Machine has old copies.
- a 2009 blog post from Don Stewart, [Atom : a domain specific language for hard realtime applications](https://archhaskell.wordpress.com/2009/08/01/atom-a-domain-specific-language-for-hard-realtime-applications/)
- two now-offline blog posts from John Van Enk at blog.sw17ch.com, [Atom & Arduino : Some Hacking (pt. 1)](https://web.archive.org/web/20110812162216/http://blog.sw17ch.com/wordpress/?p=84) and [Atom & Arduino : First Program (pt. 2)](https://web.archive.org/web/20110812162107/http://blog.sw17ch.com/wordpress/?p=111)
- [atom-msp430](https://github.com/eightyeight/atom-msp430), 3rd-party code for interfacing with MSP430 microcontrollers.

Atom's approach is a bit different than anything I'd mentioned in the prior section. As Hawkins' slides mention at page 12, Atom's compiler handles scheduling and synchronization, avoiding the need for locks, semaphores, or any kind of scheduling at run-time. As a Haskell EDSL, it also moves the abstraction up into Haskell rather than trying to make abstractions accessible from C, and that is not a trivial benefit.

Lee Pike in his link refers to Atom as a [synchronous language](https://en.wikipedia.org/wiki/Synchronous_programming_language): one specifies rules that fire on clock ticks, and state changes are atomic. This is good to bear in mind when understanding Atom definitions.

Example
====

This file ([2015-02-13-atom-examples.lhs](2015-02-13-atom-examples.lhs)) is a Literate Haskell file, so it is runnable with the example explained here. Only `cabal install atom` should be needed.

Standard Boilerplate
----

> module Main where
> 
> import Language.Atom
> import Language.Atom.Unit
> import GHC.Word

`main` just runs the Atom compiler via [`Language.Atom.Compile.compile`](http://hackage.haskell.org/package/atom-1.0.12/docs/Language-Atom-Compile.html#v:compile). The function below will produce C source code in `atomExample.c` and `atomExample.h`, and it will call [`reportSchedule`](http://hackage.haskell.org/package/atom-1.0.12/docs/Language-Atom-Compile.html#v:reportSchedule) to output some (maybe) meaningful information.

`example` is the important part and it's defined a few sections below.

> main :: IO ()
> main = do
>    (sched, _, _, _, _) <- compile "atom_example" atomCfg example
>    putStrLn $ reportSchedule sched

Configuration
----

`atomCfg` gives some code generation configuration via [`Language.Atom.Code.Config`](http://hackage.haskell.org/package/atom-1.0.12/docs/Language-Atom-Code.html#t:Config). Most items here have sane defaults and are out of scope for this example.  I define two optional ones that are quite important in the generated code:

- `cFuncName` is the name of the top-level C function that must be called at regular intervals, such as by a timer interrupt.
- `cStateName` is the name of a C struct that includes all global state.

> atomCfg :: Config
> atomCfg = defaults { cFuncName = "atom_tick"
>                    , cStateName = "state_example"
>                    , cCode = prePostCode
>                    , hCode = prePostHeader
>                    }

Pre & Post Code
----

The Hackage documentation defines `cCode` and `hCode` better, including the arguments I'm ignoring. For now, all I have here is some comments that go above and below in the generated code and generated header, respectively:

> prePostCode :: [Name] -> [Name] -> [(Name, Type)] -> (String, String)
> prePostCode _ _ _ =
>   ( unlines [ "// ---- This source is automatically generated by Atom ----"
>             ]
>   , unlines [ "// ---- End automatically-generated source ----"
>             ])
> 
> prePostHeader :: [Name] -> [Name] -> [(Name, Type)] -> (String, String)
> prePostHeader _ _ _ =
>   ( unlines [ "// ---- This header is automatically generated by Atom ----"
>             ]
>   , unlines [ "// ---- End automatically-generated header ----"
>             ])

Actual Atom
----

Finally, I may describe the actual code that does something. Here is `example`, our top-level definition that we pass to the Atom compiler. This is the first appearance of the slightly-redundantly-named [`Language.Atom.Language.Atom`](http://hackage.haskell.org/package/atom-1.0.12/docs/Language-Atom-Language.html) monad, which "captures variable and transition rule declarations."

> example :: Atom ()
> example = do
> 
>   clock <- getClock
>   
>   period 10 $ atom "period10" $ do
>     printStrLn "Hello world!"

I will also make `clock`, a 64-bit integer which keeps track of time in seconds. I'll call `atom_tick` (the function I set earlier with `cFuncName`) at intervals of exactly 1 msec - and so I set the period to 1000 to have this rule fire every 1000 ticks, or 1 second.

> getClock :: Atom (V Word64)
> getClock = do
>   clock <- word64 "clock_sec" 0
>   period 1000 $ phase 0 $ atom "second" $ incr clock
>   return clock


References
==========

Slides
------
1. Hawkins, T. (2008). Controlling Hybrid Vehicles with Haskell.  [http://cufp.galois.com/2008/slides/HawkinsTom.pdf](http://cufp.galois.com/2008/slides/HawkinsTom.pdf).

Websites
--------
1. atom: A DSL for embedded hard realtime applications. [http://hackage.haskell.org/package/atom](http://hackage.haskell.org/package/atom) and [https://github.com/tomahawkins/atom](https://github.com/tomahawkins/atom).
2. An Atomic Fibonacci Server: Exploring the Atom (Haskell) DSL. [https://leepike.wordpress.com/2009/05/05/an-atomic-fibonacci-server-exploring-the-atom-haskell-dsl/](https://leepike.wordpress.com/2009/05/05/an-atomic-fibonacci-server-exploring-the-atom-haskell-dsl/).
