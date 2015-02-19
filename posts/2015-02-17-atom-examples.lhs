In the [last post][] I talked briefly about [Atom][atom-hackage], a Haskell EDSL from [Tom Hawkins][hawkins] for hard realtime embedded software. I hope to go into a little more detail here.

Background
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

Introduction to Atom
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

Atom's approach is a bit different than anything I'd mentioned in the prior section. As Hawkins' slides mention at page 12, Atom's compiler handles scheduling and synchronization, avoiding the need for locks, semaphores, or any kind of scheduling at run-time. As a Haskell EDSL, it also moves the abstraction up into Haskell rather than trying to make abstractions accessible from C, and that is not a trivial benefit.

Lee Pike in his link refers to Atom as a [synchronous language][]: one specifies rules that fire on clock ticks, and state changes are atomic. This is good to bear in mind when understanding Atom definitions.

Example
====

This post's [source code][] is a Literate Haskell file, so you may run it directly. Only `cabal install atom` should be needed.  The source code gives an example Atom specification which does two things:

1. Keeps track of a global clock in seconds.
2. Monitors a sensor periodically via some external function calls, and performs some action whenever the sensor's value exceeds a threshold.

The rest of the source code is for configuration and interfacing with the Atom compiler.

Standard Boilerplate
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

`main` just runs the Atom compiler via [Language.Atom.Compile.compile](http://hackage.haskell.org/package/atom-1.0.12/docs/Language-Atom-Compile.html#v:compile). The function below will produce C source code in `atom_example.c` and `atom_example.h`, and it will call [reportSchedule](http://hackage.haskell.org/package/atom-1.0.12/docs/Language-Atom-Compile.html#v:reportSchedule) to output some (maybe) meaningful information.

`example` is the important part and it's defined a few sections below.

Configuration
----

`atomCfg` gives some code generation configuration via [Language.Atom.Code.Config](http://hackage.haskell.org/package/atom-1.0.12/docs/Language-Atom-Code.html#t:Config). Most items here have sane defaults and are out of scope for this example; we turn off rule coverage checking with `cRuleCoverage`.  I define two optional ones that are quite important in the generated code:

- `cFuncName` is the name of the top-level C function that must be called at regular intervals, such as by a timer interrupt.
- `cStateName` is the name of a C struct that includes all global state.

> atomCfg :: Config
> atomCfg = defaults { cFuncName = "atom_tick"
>                    , cStateName = "state_example"
>                    , cCode = prePostCode
>                    , hCode = prePostHeader
>                    , cRuleCoverage = False
>                    }

Pre & Post Code
----

The Hackage documentation defines `cCode` and `hCode` better, including the arguments I'm ignoring. This supplies C code that Atom will insert verbatim above and below the generated code and generated headers.  For now, I've inserted some definitions and prototypes that you will need to implement yourself if you want this code to actually build (and whose purpose I explain later).

> prePostCode :: [Name] -> [Name] -> [(Name, Type)] -> (String, String)
> prePostCode _ _ _ =
>   ( unlines [ "// ---- This source is automatically generated by Atom ----"
>             , "extern bool g_sensor_ready;"
>             , "extern uint16_t g_sensor_value;"
>             ]
>   , unlines [ "// ---- End automatically-generated source ----"
>             ])
> 
> prePostHeader :: [Name] -> [Name] -> [(Name, Type)] -> (String, String)
> prePostHeader _ _ _ =
>   ( unlines [ "// ---- This header is automatically generated by Atom ----"
>             , "void sensor_on(void);"
>             , "void sensor_off(void);"
>             , "void sensor_trigger(void);"
>             , "void on_sensor_over_threshold(void);"
>             ]
>   , unlines [ "// ---- End automatically-generated header ----"
>             ])

Top-level (`example`) Rule
----

Finally, I may describe the actual code that does something. Here is `example`, my top-level definition that I pass to the Atom compiler. This is the first appearance of the slightly-redundantly-named [Language.Atom.Language.Atom](http://hackage.haskell.org/package/atom-1.0.12/docs/Language-Atom-Language.html) monad, which "captures variable and transition rule declarations."

> example :: Atom ()
> example = do
> 
>   clock <- tickSecond
>
>   checkSensor 16384 $ do
>     call "on_sensor_over_threshold"
>     printStrLn "Sensor value over threshold!"

I define both `tickSecond` and `checkSensor` below. The arguments to `checkSensor` are, respectively, a sensor threshold, and an action to trigger if the sensor exceeds that threshold - more on this later.

`tickSecond` Rule
----

> tickSecond :: Atom (V Word64)
> tickSecond = do
>   clock <- word64 "clock_sec" 0
>   period 1000 $ exactPhase 0 $ atom "second" $ incr clock
>   return clock

Note a few new things:

- I define `clock` via [word64](http://hackage.haskell.org/package/atom-1.0.12/docs/Language-Atom-Language.html#v:word64),  which introduces a local variable, an unsigned 64-bit integer. "clock_sec" is a C name - more specifically, the field name inside the struct above whose name I set with `cStateName`. 0 is an initial value.
- I've defined a sub-rule with [period](http://hackage.haskell.org/package/atom-1.0.12/docs/Language-Atom-Language.html#v:period) which executes at 1 / 1000 of the base rate of the system. I've also used [exactPhase](http://hackage.haskell.org/package/atom-1.0.12/docs/Language-Atom-Language.html#v:exactPhase) to dictate that it runs first in that period (i.e. at [phase](http://hackage.haskell.org/package/atom-1.0.12/docs/Language-Atom-Language.html#v:phase) 0).
- I return `clock` in the monad. Its `V Word64` type wraps a standard Haskell `Data.Word.Word64`.

I mentioned *base rate of the system.* That base rate is the rate at which I call `atom_tick` (the function set earlier with `cFuncName`). I intend to write the C code to call it at intervals of 1 millisecond. Thus, the sub-rule above executes at 1 / 1000 of this - once every second.

I give the sub-rule a unique name ("second"), and the sub-rule is responsible for incrementing `clock` via [incr](http://hackage.haskell.org/package/atom-1.0.12/docs/Language-Atom-Language.html#v:incr), once per second.

`checkSensor` Rule
----
Next, suppose I have a sensor I want to monitor, but getting a sensor measurement is a process like this:

1. Power it on via a C call `sensor_on`.
2. Wait at least 10 milliseconds for it to settle.
3. Trigger a measurement via a C call `sensor_trigger`.
4. Wait for some external variable `g_sensor_ready` to indicate true.
5. Receive our result in another external variable, `g_sensor_value`.
6. Power the sensor off via `sensor_off`.

Suppose, also, we want a threshold value and an action to take if the sensor exceeds that threshold.

I attempt to do this below:

> checkSensor :: Word16 -> Atom () -> Atom ()
> checkSensor threshold action = atom "check_sensor" $ do
>   ready <- return $ bool' "g_sensor_ready"
>   sensor_value <- return $ word16' "g_sensor_value"
>   warmup <- timer "warmup"
> 
>   period 2000 $ phase 500 $ atom "powerOn" $ do
>     call "sensor_on"
>     startTimer warmup $ Const 10
>   
>   atom "trigger" $ do
>     cond $ timerDone warmup
>     ready <== false
>     call "sensor_trigger"
>     
>   atom "checkSensorValue" $ do
>     cond $ value ready
>     ready <== false
>     call "sensor_off"
>     cond $ value sensor_value >. Const threshold
>     action

While `atom "check_sensor"` may not (as I understand it) be strictly necessary here, that call to [atom](http://hackage.haskell.org/package/atom-1.0.12/docs/Language-Atom-Language.html#v:atom) defines this whole thing as a sub-rule, and this hierarchy will emerge in the organization and identifiers in the generated code.

I use a few new constructs here:

- *External variables:* I introduce `ready` using [bool'](http://hackage.haskell.org/package/atom-1.0.12/docs/Language-Atom-Language.html#v:bool-39-), tying it to an external C variable `g_sensor_ready` (and likewise `sensor_value` to `g_sensor_value` using [word16'](hackage.haskell.org/package/atom-1.0.12/docs/Language-Atom-Language.html#v:word16-39-)).
- *External calls:* I call several external C functions using [call](http://hackage.haskell.org/package/atom-1.0.12/docs/Language-Atom-Language.html#v:call). If you refer to their prototypes in the *Pre & Post Code* section, they are all `void` functions taking no arguments, and as far as I know, Atom handles only this sort of external call.
- *Timers:* `warmup` is a [Timer](http://hackage.haskell.org/package/atom-1.0.12/docs/Language-Atom-Common.html#t:Timer) which I use to count down 10 ticks (10 milliseconds) from the time of powering on the sensor (see [startTimer](http://hackage.haskell.org/package/atom-1.0.12/docs/Language-Atom-Common.html#v:startTimer) in the `powerOn` rule).
- *Conditionals:* The rule `trigger` makes use of [cond](http://hackage.haskell.org/package/atom-1.0.12/docs/Language-Atom-Language.html#v:cond) to execute only when that timer has finished. The rule `checkSensorValue` likewise uses it to execute only when the measurement is ready, and to execute `action` only when the value exceeds the threshold.

Note also that the rule `powerOn` has period 2000 and phase 500: It runs every 2 seconds, offset by 1/2 second. The other two rules implicitly have period 1 - they run at every clock tick.

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

[source code]: ./2015-02-17-atom-examples.lhs
[last post]: ./2015-02-06-how-i-got-here.html

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
