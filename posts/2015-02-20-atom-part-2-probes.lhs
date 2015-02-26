---
title: Atom, part 2: Probes
date: February 20, 2015
author: Chris Hodapp
tags: haskell, atom
---

Probes
====

The [last post][] introduced [Atom][atom-hackage] with a simple example that I used to cover the basics. I'm going to extend this to cover [probes][hackage-probe]. Probes are a bit vague. The only explicit information I can find is:

- A [StackOverflow][stackoverflow-haskell] reply from [Tom Hawkins][hawkins] - Atom's author, incidentally.
- The Atom source code, [here](http://hackage.haskell.org/package/atom-1.0.12/docs/src/Language-Atom-Language.html#probe) and [here](http://hackage.haskell.org/package/atom-1.0.12/docs/src/Language-Atom-Elaboration.html#elaborate).

Here is my own working definition after some examples: *A probe allows inspecting any expression in Atom, remotely to its context, and at any desired rate.*

The type signature suggests that inserting a probe is a matter of inserting [probe][hackage-probe] anywhere inside of an Atom specification, along with a probe name and an expression, and that accessing probes is a matter of examining [probes][hackage-probes].

The preamble is identical to the prior example:

> module Main where
> 
> import Language.Atom
> import Language.Atom.Unit
> import GHC.Word
> import GHC.Int
> 
> main :: IO ()
> main = do
>    (sched, _, _, _, _) <- compile "atom_example2" atomCfg example
>    putStrLn $ reportSchedule sched
> 
> atomCfg :: Config
> atomCfg = defaults { cFuncName = "atom_tick"
>                    , cStateName = "state_example"
>                    , cCode = prePostCode
>                    , hCode = prePostHeader
>                    , cRuleCoverage = False
>                    }

In the last post I ignored the arguments to the functions set in `cCode` and `hCode` in [Config](http://hackage.haskell.org/package/atom-1.0.12/docs/Language-Atom-Code.html#t:Config). The third argument is a list of probes along with their types.

The below code is identical, save for three changes:

- The definition of `probeStr` which turns a probe name & type into a String.
- Assigning a name `probeList` to the `[(Name,Type)]` argument in `prePostCode`.
- Appending `map probeStr probeList` to the list. This serves no functional purpose, it just adds comments into the code to illustrate what probes are present.

> probeStr :: (Name, Type) -> String
> probeStr (n, t) = "// Probe: " ++ n ++ ", type: " ++ show t
> 
> prePostCode :: [Name] -> [Name] -> [(Name, Type)] -> (String, String)
> prePostCode _ _ probeList =
>   ( unlines $ [ "// ---- This source is automatically generated by Atom ----"
>               , "#include <stdlib.h>"
>               , "#include <stdio.h>"
>               , "#include <unistd.h>"
>               , ""
>               , "bool g_sensor_ready;"
>               , "uint16_t g_sensor_value;"
>               , "void sensor_on(void);"
>               , "void sensor_off(void);"
>               , "void sensor_trigger(void);"
>               , ""
>               ] ++ map probeStr probeList
>   , unlines [ "int main(void) {"
>             , "    while (true) {"
>             , "        atom_tick();"
>             , "        usleep(1000);"
>             , "    }"
>             , "    return 0;"
>             , "}"
>             , "void sensor_on(void) {"
>             , "    printf(\"%lu: sensor_on()\\n\", __global_clock);"
>             , "}"
>             , ""
>             , "void sensor_off(void) {"
>             , "    printf(\"%lu: sensor_off()\\n\", __global_clock);"
>             , "}"
>             , ""
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
> 
> prePostHeader :: [Name] -> [Name] -> [(Name, Type)] -> (String, String)
> prePostHeader _ _ _ =
>   ( unlines [ "// ---- This header is automatically generated by Atom ----"
>             ]
>   , unlines [ "// ---- End automatically-generated header ----"
>             ])


I reordered things from the last example to give some simpler definitions first. First I add a probe into `tickSecond` for the value of `clock` (which thus far I have not done anything with):

> tickSecond :: Atom (V Word64)
> tickSecond = do
>   clock <- word64 "clock_sec" 0
>   probe "Clock" $ value clock
>   period 1000 $ exactPhase 0 $ atom "second" $ incr clock
>   return clock

And a probe into `checkSensor` for the value of `sensorValue`:

> checkSensor :: Word16 -> Atom () -> Atom ()
> checkSensor threshold overThresholdAction = atom "check_sensor" $ do
>   ready <- return $ bool' "g_sensor_ready"
>   sensorValue <- return $ word16' "g_sensor_value"
>   warmup <- timer "warmup"
>   triggered <- bool "triggered" False
>   sensorOn <- bool "sensor_on" False
>
>   probe "Sensor Value" $ value sensorValue
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

I can access all probes via [probes][hackage-probes], but given its type signature, some conversion is necessary:
```haskell
probes :: Atom [(String, UE)]
```

Primarily, that `UE` ('untyped expression') needs to be turned to something else to be useful. Luckily, [typeOf](http://hackage.haskell.org/package/atom-1.0.12/docs/Language-Atom-Expressions.html#v:typeOf) gives its type as a [Language.Atom.Expressions.Type](http://hackage.haskell.org/package/atom-1.0.12/docs/Language-Atom-Expressions.html#t:Type), and then the [Retype](http://hackage.haskell.org/package/atom-1.0.12/docs/Language-Atom-Expressions.html#v:Retype) constructor can convert it accordingly.

I'm not sure what else I can easily tell Atom to do with an expression, so I just pass it to [printIntegralE](http://hackage.haskell.org/package/atom-1.0.12/docs/Language-Atom-Unit.html#v:printIntegralE). I'm sure a suitably motivated individual could make more interesting functionality. I come up with this slightly hackish Haskell function for printing a probe's value:

*(Do you have a cleaner version? Show me.)*

> printProbe :: (String, UE) -> Atom ()
> printProbe (str, ue) = case typeOf ue of
>   Int8   -> ps (ru :: E Int8)
>   Int16  -> ps (ru :: E Int16)
>   Int32  -> ps (ru :: E Int32)
>   Int64  -> ps (ru :: E Int64)
>   Word8  -> ps (ru :: E Word8)
>   Word16 -> ps (ru :: E Word16)
>   Word32 -> ps (ru :: E Word32)
>   Word64 -> ps (ru :: E Word64)
>   where ps :: IntegralE a => E a -> Atom ()
>         ps = printIntegralE str
>         ru :: IntegralE a => E a
>         ru = Retype ue

Finally, in `example` I add in a rule `monitor` to print all the probe values. For sanity's sake, the period is 100 for a rate of every 1/10 second rather than at the base rate.

> example :: Atom ()
> example = do
> 
>   clock <- tickSecond
>
>   checkSensor 40000 $ do
>     printStrLn "Sensor value over threshold!"
> 
>   period 100 $ atom "monitor" $ do
>     mapM_ printProbe =<< probes
> 

In the end the monitor C code looks like this:
```c
/* atom_example2.monitor */
static void __r6() {
  bool __0 = true;
  uint16_t __1 = g_sensor_value;
  uint64_t __2 = state_example.atom_example2.clock_sec;
  if (__0) {
    printf("Sensor Value: %i\n", __1);
    printf("Clock: %i\n", __2);
  }
}
```

Fairly simple, but I can see how it would be useful.

[source code]: ./2015-02-20-atom-part-2-probes.lhs
[last post]: ./2015-02-17-atom-examples.html
[atom-hackage]: http://hackage.haskell.org/package/atom "atom: A DSL for embedded hard realtime applications. (hackage)"
[atom-github]: https://github.com/tomahawkins/atom "atom: A DSL for embedded hard realtime applications. (github)"
[hawkins]: http://tomahawkins.org/
[stackoverflow-haskell]: https://stackoverflow.com/questions/1263711/using-haskell-for-sizable-real-time-systems-how-if#answer-1582191 "Using Haskell for sizable real-time systems: how (if?)?"
[hackage-probe]: http://hackage.haskell.org/package/atom-1.0.12/docs/Language-Atom-Language.html#v:probe
[hackage-probes]: http://hackage.haskell.org/package/atom-1.0.12/docs/Language-Atom-Language.html#v:probes
