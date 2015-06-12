---
title: Examples for the Atom library
date: February 17, 2015
author: Chris Hodapp
tags: haskell, atom
---

*Later update: I've tried to introduce some of the documentation and explanation below into the Atom repository, so ignore my complaints of it being undocumented.*

In the [last post][] I talked briefly about Atom ([hackage][atom-hackage] & [github][atom-github]), a Haskell EDSL from [Tom Hawkins][hawkins] for hard realtime embedded software. I aim here to cover more detail.

Background {#background}
====

As that post mentioned, I was running into problems handling concurrency in software for my embedded target (a [Nordic nRF51822][nrf51822]). My embedded software had operations throughout that required some fairly specific timing, for instance:

1. Activate some circuit.
2. Wait *at least* 300 milliseconds for that circuit to settle.
3. Trigger the circuit to send a pulse.
4. Wait *exactly* 145 microseconds, and set circuit to receive mode.
5. Wait *at least* a millisecond.
6. Send a command over SPI to trigger a reading.
7. Wait *at most* 1 millisecond for an acknowledgement.
8. Repeat 6 and 7 to read the contents of 3 or 4 other registers.

This was simple enough, except that the processor had to share a dozen similar processes simultaneously. The delay function could delay precisely, but by busy-looping, blocking other execution and wasting power, a precious resource here. Timer callbacks and state machines could share execution delay for longer periods, but at the cost of extra complexity.

I looked at a variety of solutions which implemented schedulers that ran directly on the target, but nearly all of them appeared to either interfere with Nordic's firmware or require too extensive of an implementation. The one that I finally could port, [cocoOS][], had the rather show-stopping issue that its primitives were simply using more resources than I had available.

Introduction to Atom {#intro}
====

At this point I started looking at Atom. Its documentation is a bit lacking, and with no updates on its Hackage page in 2 years, I expect no change in that - which is a shame, because this is substantial industry work that was open-sourced. I'm slowly understanding it from what I can find, mainly:

- Hackage documentation:
    - [Language.Atom.Code](http://hackage.haskell.org/package/atom-1.0.12/docs/Language-Atom-Code.html)
    - [Language.Atom.Common](http://hackage.haskell.org/package/atom-1.0.12/docs/Language-Atom-Common.html)
    - [Language.Atom.Example](http://hackage.haskell.org/package/atom-1.0.12/docs/src/Language-Atom-Example.html)
    - [Language.Atom.Expressions](http://hackage.haskell.org/package/atom-1.0.12/docs/Language-Atom-Expressions.html)
    - [Language.Atom.Language](http://hackage.haskell.org/package/atom-1.0.12/docs/Language-Atom-Language.html)
- some slides from creator Tom Hawkins in 2008, [Controlling Hybrid Vehicles with Haskell][hawkins-hybrids]. Some names in the example code have since changed, such as `atom` replacing `system`.
- a more formal example from Lee Pike, [An Atomic Fibonacci Server: Exploring the Atom (Haskell) DSL][pike-atom-fibonacci]. His links to code examples appear to be down, but the Wayback Machine has old copies. 
- a 2009 blog post from Don Stewart, [Atom : a domain specific language for hard realtime applications][stewart-archhaskell]
- two now-offline blog posts from John Van Enk at blog.sw17ch.com, [Atom & Arduino : Some Hacking (pt. 1)][atom-arduino1] and [Atom & Arduino : First Program (pt. 2)][atom-arduino2]
- [atom-msp430][], 3rd-party code for interfacing with MSP430 microcontrollers.

Atom's approach is a bit different than anything I'd mentioned in the prior section. As Hawkins' slides mention at page 12, Atom's compiler handles scheduling and synchronization, avoiding the need for locks, semaphores, or any kind of scheduling at run-time. As a Haskell EDSL, it also moves the abstraction up into Haskell rather than trying to make abstractions accessible from C, and that is not a trivial benefit. To be clear: Atom's compiler produces C source code from a specification in its Haskell EDSL.

Lee Pike in his link refers to Atom as a [synchronous language][]: one specifies rules that fire on clock ticks, and state changes are atomic. Bear this in mind when understanding Atom definitions.

Example {#example}
====

This post's [source code][] is a Literate Haskell file, so you may run it directly. Only `cabal install atom` should be needed.  The source code gives an example Atom specification which does two things:

1. Keeps track of a global clock in seconds.
2. Monitors a sensor periodically via some external function calls, and performs some action whenever the sensor's value exceeds a threshold.

The rest of the source code is for configuration and interfacing with the Atom compiler.

Standard Boilerplate {#boilerplate}
----

> module Main where
> 
> import Language.Atom
> import Language.Atom.Unit
> import GHC.Word
> 
> main :: IO ()
> main = do
>    (sched, _, _, _, _) <- compile "atom_example" atomCfg example
>    putStrLn $ reportSchedule sched

`main` just runs the Atom compiler via [Language.Atom.Compile.compile](http://hackage.haskell.org/package/atom-1.0.12/docs/Language-Atom-Compile.html#v:compile). The function below produces C source code in `atom_example.c` and `atom_example.h`, and it calls [reportSchedule](http://hackage.haskell.org/package/atom-1.0.12/docs/Language-Atom-Compile.html#v:reportSchedule) to output some (maybe) meaningful information. (More on this is later on in [Atom Compiler Output](#output).)

I define the important part, `example`, a few sections below.

Configuration {#config}
----

`atomCfg` gives some code generation configuration via [Language.Atom.Code.Config](http://hackage.haskell.org/package/atom-1.0.12/docs/Language-Atom-Code.html#t:Config). Most items here have sane defaults and are out of scope for this example; I turn off rule coverage checking with `cRuleCoverage`.  I define two optional ones that are quite important in the generated code:

- `cFuncName` is the name of a top-level C function you must call at regular intervals, such as by a timer interrupt.
- `cStateName` is the name of a C struct that includes all global state.

> atomCfg :: Config
> atomCfg = defaults { cFuncName = "atom_tick"
>                    , cStateName = "state_example"
>                    , cCode = prePostCode
>                    , hCode = prePostHeader
>                    , cRuleCoverage = False
>                    }

Pre & Post Code {#prepost}
----

The [Hackage documentation]((http://hackage.haskell.org/package/atom-1.0.12/docs/Language-Atom-Code.html#t:Config)) defines `cCode` and `hCode` better, including the arguments I'm ignoring. `prePostCode` (the value of `cCode`) provides C code that Atom inserts verbatim above and below the generated C code. `prePostHeader` & `hCode` are the same thing but for the generated C header.

*(As a side note, Shae discovered Uli Köhler's relevant post at Tech Overflow on a cleaner way of expressing the C code: [Using QuasiQuotation for more readable Atom code][quasiquotation-atom].)*

Below begins a pretty minimal implementation for a `main()` function and a simulated sensor.

> prePostCode :: [Name] -> [Name] -> [(Name, Type)] -> (String, String)
> prePostCode _ _ _ =
>   ( unlines [ "// ---- This source is automatically generated by Atom ----"
>             , "#include <stdlib.h>"
>             , "#include <stdio.h>"
>             , "#include <unistd.h>"
>             , ""
>             , "bool g_sensor_ready;"
>             , "uint16_t g_sensor_value;"
>             , "void sensor_on(void);"
>             , "void sensor_off(void);"
>             , "void sensor_trigger(void);"
>             ]

`main` sets up a 1(ish) millisecond timer call to that important `atom_tick` function set for `cFuncName` in the [Configuration](#config) section:

(*Disclaimer:* In any real application, use something like a timer interrupt. This example is meant to be short and functional, and I know of no short POSIX way to get 1 msec timer callbacks, so just accept that it is wildly inaccurate.)

>   , unlines [ "int main(void) {"
>             , "    while (true) {"
>             , "        atom_tick();"
>             , "        usleep(1000);"
>             , "    }"
>             , "    return 0;"
>             , "}"

`sensor_on()` and `sensor_off()` are no-ops, besides their output:

>             , "void sensor_on(void) {"
>             , "    printf(\"%lu: sensor_on()\\n\", __global_clock);"
>             , "}"
>             , ""
>             , "void sensor_off(void) {"
>             , "    printf(\"%lu: sensor_off()\\n\", __global_clock);"
>             , "}"
>             , ""

`sensor-trigger()` either immediately gets a random value for the sensor value, or around 25% of the time, does nothing:

(To be realistic this should run in the background somehow, but I am trying to keep this example short.)

>             , "void sensor_trigger(void) {"
>             , "    if (rand() % 4) {"
>             , "        g_sensor_value = rand();"
>             , "        g_sensor_ready = true;"
>             , "        printf(\"%lu: Triggered sensor, value=%u\\n\","
>             , "               __global_clock, g_sensor_value);"
>             , "    }"
>             , "}"
>             , ""
>             , "// ---- End automatically-generated source ----"
>             ])

The pre and post section for the header is basically blank:

> prePostHeader :: [Name] -> [Name] -> [(Name, Type)] -> (String, String)
> prePostHeader _ _ _ =
>   ( unlines [ "// ---- This header is automatically generated by Atom ----"
>             ]
>   , unlines [ "// ---- End automatically-generated header ----"
>             ])

*(Note that the generated C source does not `#include` this header - unless you add it yourself in `prePostCode`.)*

Top-level (*example*) rule {#toplevelrule}
----

Finally, I may describe `example`, the top-level Atom specification. This is the first appearance of the slightly-redundantly-named [Language.Atom.Language.Atom](http://hackage.haskell.org/package/atom-1.0.12/docs/Language-Atom-Language.html) monad, which "captures variable and transition rule declarations."

> example :: Atom ()
> example = do
> 
>   clock <- tickSecond
>
>   checkSensor 40000 $ do
>     printStrLn "Sensor value over threshold!"

I define `tickSecond` and `checkSensor` below. The arguments to `checkSensor` are, respectively, a sensor threshold, and an action to trigger if the sensor exceeds that threshold - more on this later.

*tickSecond* sub-rule {#ticksecond}
----

> tickSecond :: Atom (V Word64)
> tickSecond = do
>   clock <- word64 "clock_sec" 0
>   period 1000 $ exactPhase 0 $ atom "second" $ incr clock
>   return clock

Note a few new things:

- I define `clock` via [word64](http://hackage.haskell.org/package/atom-1.0.12/docs/Language-Atom-Language.html#v:word64),  which introduces a local variable, an unsigned 64-bit integer. "clock_sec" is a C name - more specifically, the field name inside the struct above whose name I set with `cStateName`. 0 is an initial value.
- I define a sub-rule with [period](http://hackage.haskell.org/package/atom-1.0.12/docs/Language-Atom-Language.html#v:period) which executes at 1 / 1000 of the base rate of the system. I also use [exactPhase](http://hackage.haskell.org/package/atom-1.0.12/docs/Language-Atom-Language.html#v:exactPhase) to dictate that it runs first in that period (i.e. at [phase](http://hackage.haskell.org/package/atom-1.0.12/docs/Language-Atom-Language.html#v:phase) 0).
- I return `clock` in the monad. Its `V Word64` type wraps a standard Haskell `Data.Word.Word64`.

I mentioned *base rate of the system.* That base rate is the rate at which the C code calls `atom_tick` (the function set earlier with `cFuncName`) - nominally, once per millisecond. Thus, the sub-rule above executes at 1 / 1000 of this - once every second.

I give the sub-rule a unique name ("second"), and the sub-rule then increments `clock` via [incr](http://hackage.haskell.org/package/atom-1.0.12/docs/Language-Atom-Language.html#v:incr), once per second.

*checkSensor* sub-rule {#checksensor}
----
Next, suppose I have a sensor I want to monitor, but getting a sensor measurement is a process like this:

1. Power it on via a C call `sensor_on`.
2. Wait at least 10 milliseconds for it to settle.
3. Trigger a measurement via a C call `sensor_trigger`.
4. Wait for some external variable `g_sensor_ready` to indicate true.
5. Receive our result in another external variable, `g_sensor_value`.
6. If, 50 milliseconds after step (2), no measurement has arrived, power the sensor off via `sensor_off`.

Suppose, also, we want a threshold value and an action to take if the sensor exceeds that threshold.

I attempt to do this below:

> checkSensor :: Word16 -> Atom () -> Atom ()
> checkSensor threshold overThresholdAction = atom "check_sensor" $ do
>   ready <- return $ bool' "g_sensor_ready"
>   sensorValue <- return $ word16' "g_sensor_value"
>   warmup <- timer "warmup"
>   triggered <- bool "triggered" False
>   sensorOn <- bool "sensor_on" False
> 
>   period 2000 $ phase 500 $ atom "powerOn" $ do
>     call "sensor_on"
>     triggered <== false
>     ready <== false
>     sensorOn <== true
>     startTimer warmup $ Const 10
>   
>   atom "trigger" $ do
>     cond $ timerDone warmup &&. not_ (value triggered) &&. value sensorOn
>     triggered <== true
>     call "sensor_trigger"
>     
>   atom "checkSensorValue" $ do
>     cond $ value ready
>     ready <== false
>     sensorOn <== false
>     call "sensor_off"
>     atom "checkThreshold" $ do
>       cond $ value sensorValue >. Const threshold
>       overThresholdAction
>   
>   period 2000 $ phase 550 $ atom "powerOff" $ do
>     cond $ value sensorOn
>     ready <== false
>     printStrLn "Sensor timeout."
>     call "sensor_off"

(*Another disclaimer:* In a real application, you should probably avoid things like [`printStrLn`](http://hackage.haskell.org/package/atom-1.0.12/docs/Language-Atom-Unit.html#v:printStrLn) that may block indefinitely for I/O.)

I use a few new constructs here:

- *External variables:* I introduce `ready` using [bool'](http://hackage.haskell.org/package/atom-1.0.12/docs/Language-Atom-Language.html#v:bool-39-), tying it to an external C variable `g_sensor_ready` (and likewise `sensorValue` to `g_sensor_value` using [word16'](hackage.haskell.org/package/atom-1.0.12/docs/Language-Atom-Language.html#v:word16-39-)).
- *External calls:* I call several external C functions using [call](http://hackage.haskell.org/package/atom-1.0.12/docs/Language-Atom-Language.html#v:call). If you refer to their prototypes in the *Pre & Post Code* section, they are all `void` functions taking no arguments.
- *Timers:* `warmup` is a [Timer](http://hackage.haskell.org/package/atom-1.0.12/docs/Language-Atom-Common.html#t:Timer) which I use to count down 10 ticks (10 milliseconds) from the time of powering on the sensor (see [startTimer](http://hackage.haskell.org/package/atom-1.0.12/docs/Language-Atom-Common.html#v:startTimer) in the `powerOn` rule).
- *Conditionals:* The rule `trigger` makes use of [cond](http://hackage.haskell.org/package/atom-1.0.12/docs/Language-Atom-Language.html#v:cond) and [&&.](http://hackage.haskell.org/package/atom-1.0.12/docs/Language-Atom-Expressions.html#v:-38--38-.) to execute once that timer has finished, given that the sensor is on and has *not* been triggered. The other rules besides `powerOn` use [cond](http://hackage.haskell.org/package/atom-1.0.12/docs/Language-Atom-Language.html#v:cond) in similar ways.

Note that the rule `powerOn` has period 2000 and phase 500: It runs every 2 seconds, offset by 1/2 second. The rules `trigger` and `checkSensorValue` implicitly have period 1 - they run at every clock tick. `powerOff` has the same period and a phase 50 ticks (50 milliseconds) after.

The *External calls* note above also bears further examination. I mention that all the functions are of type `void f(void)`. Atom directly handles only this type of external call, as far as I know, and I suspect that this is by design. Any communication with external code, then, must be through variables - no function parameters, no return values, no callbacks.

Also, do you recall the end of [Introduction](#intro) saying that Atom is a [synchronous language][]? Note carefully that  [atom](http://hackage.haskell.org/package/atom-1.0.12/docs/Language-Atom-Language.html#v:atom) creates a node with an *atomic rule*, and treat everything inside of it as happening simultaneously. Because of this, the sub-sub-rule `checkThreshold` inside of `checkSensorValue` must be separated with `atom` if its conditions are separate.

The `atom "check_sensor"` at the top may not be strictly necessary as it has only more `atom` nodes beneath it. However, this hierarchy will emerge in the organization and identifiers in the generated code.

Atom Compiler Output {#output}
----

Build the code and run it to generate the code and output a schedule report from [reportSchedule](http://hackage.haskell.org/package/atom-1.0.12/docs/Language-Atom-Compile.html#v:reportSchedule):

```
$ ghc --make 2015-02-17-atom-examples.lhs
[1 of 1] Compiling Main             ( 2015-02-17-atom-examples.lhs, 2015-02-17-atom-examples.o )
Linking 2015-02-17-atom-examples ...
$ ./2015-02-17-atom-examples
Rule Scheduling Report

Period  Phase  Exprs  Rule
------  -----  -----  ----
  1000      0      4  atom_example.second
  2000    500      5  atom_example.check_sensor.powerOn
  2000    550      4  atom_example.check_sensor.powerOff
     1      0     14  atom_example.check_sensor.trigger
     1      0      5  atom_example.check_sensor.checkSensorValue.checkThreshold
     1      0      5  atom_example.check_sensor.checkSensorValue
               -----
                  37

Hierarchical Expression Count

  Total   Local     Rule
  ------  ------    ----
      37       0    atom_example
      33       0      check_sensor
      10       5        checkSensorValue
       5       5          checkThreshold
       4       4        powerOff
       5       5        powerOn
      14      14        trigger
       4       4      second

```

This should look familiar - it is the rule names and hierarchy starting at `atom_example`. It should have produced `atom_example.c` and `atom_example.h` as well, which you may build and run:

```
$ gcc -o atom_example.o atom_example.c
$ ./atom_example.o
500: sensor_on()
510: Triggered sensor, value=9158
510: sensor_off()
2500: sensor_on()
2510: Triggered sensor, value=18547
2510: sensor_off()
4500: sensor_on()
4510: Triggered sensor, value=23807
4510: sensor_off()
6500: sensor_on()
6510: Triggered sensor, value=22764
6510: sensor_off()
8500: sensor_on()
8510: Triggered sensor, value=31949
8510: sensor_off()
10500: sensor_on()
10510: Triggered sensor, value=55211
Sensor value over threshold!
10510: sensor_off()
12500: sensor_on()
12510: Triggered sensor, value=7931
12510: sensor_off()
14500: sensor_on()
14510: Triggered sensor, value=57670
Sensor value over threshold!
14510: sensor_off()
16500: sensor_on()
Sensor timeout.
16550: sensor_off()
18500: sensor_on()
18510: Triggered sensor, value=2132
18510: sensor_off()
...
```

Most lines begin with a number which is the global clock.

Closing
====
Hopefully, this provided a meaningful introduction. The [next post] covers how to use another Atom feature, *probes*.

[source code]: ./2015-02-17-atom-examples.lhs
[last post]: ./2015-02-06-how-i-got-here.html
[next post]: ./2015-02-20-atom-part-2-probes.html

[hawkins]: http://tomahawkins.org/
[hawkins-hybrids]: http://cufp.galois.com/2008/slides/HawkinsTom.pdf "Controlling Hybrid Vehicles with Haskell. Hawkins, T. (2008)."
[atom-hackage]: http://hackage.haskell.org/package/atom "atom: A DSL for embedded hard realtime applications. (hackage)"
[atom-github]: https://github.com/tomahawkins/atom "atom: A DSL for embedded hard realtime applications. (github)"
[nrf51822]: https://www.nordicsemi.com/eng/Products/Bluetooth-Smart-Bluetooth-low-energy/nRF51822
[pike-atom-fibonacci]: https://leepike.wordpress.com/2009/05/05/an-atomic-fibonacci-server-exploring-the-atom-haskell-dsl/ "An Atomic Fibonacci Server: Exploring the Atom (Haskell) DSL. Pike. L. (2009)."
[cocoos]: http://www.cocoos.net/intro.html
[stewart-archhaskell]: https://archhaskell.wordpress.com/2009/08/01/atom-a-domain-specific-language-for-hard-realtime-applications "Atom : a domain specific language for hard realtime applications. Stewart, D. (2009)."
[atom-arduino1]: https://web.archive.org/web/20110812162216/http://blog.sw17ch.com/wordpress/?p=84 "Atom & Arduino : Some Hacking (pt. 1). Van Enk, J. (2009)."
[atom-arduino2]: https://web.archive.org/web/20110812162107/http://blog.sw17ch.com/wordpress/?p=111 "Atom & Arduino : First Program (pt. 2). Van Enk, J. (2009)."
[atom-msp430]: https://github.com/eightyeight/atom-msp430
[synchronous language]: https://en.wikipedia.org/wiki/Synchronous_programming_language
[quasiquotation-atom]: http://techoverflow.net/blog/2014/07/28/using-quasiquotation-for-more-readable-atom-code/ "Using QuasiQuotation for more readable Atom code. Köhler, U. (2014)."
