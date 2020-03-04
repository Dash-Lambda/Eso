# Contents
* [Meaning of Functional](#meaning-of-functional)
    - [First-Class Functions](#first-class-functions)
    - [Zero Side-Effects](#zero-side-effects)
    - [Referential Transparency](#referential-transparency)
* [State of the Project](#state-of-the-project)
    - [Supported Languages](#supported-languages)
    - [Current Features](#current-features)
    - [WIP](#wip)
    - [Languages Under Consideration](#languages-under-consideration)
* [Design of Eso](#design-of-eso)
    - [Language Components](#language-components)
    - [Parsers](#parsers)
    - [How the Interface Works](#how-the-interface-works)
        - [Persistent vs Non-Persistent Interfaces](#persistent-vs-non-persistent-interfaces)
        - [The Command Parser](#the-command-parser)
        - [States and Configurations](#states-and-configurations)
        - [Command Handlers](#command-handlers)
        - [Interpreter I/O](#interpreter-io)
        - [How Translators are Used](#how-translators-are-used)
    - [Environment Variables](#environment-variables)
    - [Config Environment](#config-environment)
* [Notes](#notes)
    - [BrainFuck Optimization Strategy](#brainFuck-optimization-strategy)
    - [User-Defined BrainFuck Translators](#user-defined-brainFuck-translators)
    - [On the Scala "Interpreter"](#on-the-scala-interpreter)
    - [FracTran Program Format](#fracTran-program-format)
    - [P'' Program Format](#p-program-format)
    - [On the Befunge-98 Interpreter](#on-the-befunge-98-interpreter)
    - [On the LazyK Interpreter](#on-the-lazyk-interpreter)
    - [How the Interface Code Works](#how-the-interface-code-works)
    - [Abstraction Eliminators](#abstraction-eliminators)
    - [Building](#building)

## Purpose of Eso
I develop and maintain this for two principle reasons:
* It's fun
* I learn a surprising amount

Eso started as a functional BrainFuck interpreter made out of curiosity about pure functional programming. In the course of turning Eso into what it is now, I've learned a shocking amount about language design, types, category theory, compiler theory, CPS, combinators, parsers, generative grammars, concurrency, effects, prime generators, and... quite a lot more.

You may notice that some things in Eso that could be implemented with a relatively simple library are not; usually that's on purpose, because writing the boilerplate and abstractions myself is... Well, it's fun and educational.

## Meaning of functional
Functional programming is a paradigm with three principle characteristics:

### First Class Functions
Functional programming gets its name from the fact that functions are treated as first-class values. This means you can use and manipulate functions just as you would any other variable or parameter.
### Zero Side Effects
A function defines a relationship between an input and an output. This means a function does not change anything outside of itself. One major example of this is error handling: In most languages, an error is thrown by a method and prevents a return value, and thus must be caught separately. This means code can have unexpected behavior that extends beyond the return, and it can be a pain to deal with. In functional programming, errors are usually handled by returning a wrapper that holds either a result or error information, which eliminates behavior that the caller does not expect.
### Referential Transparency
A function is also stable in that each input returns exactly one output. You should be able to replace any call to a function with the result and not change the behavior of the program, which means a function has no mutable state. As an extension of this, functional programming also makes heavy use of immutable data structures, which are data structures that cannot be changed in place.

Ultimately, what functional programming accomplishes is highly modular, testable, and type-safe code. Many functional languages also allow for far more elegant and concise code than is common of imperative and object oriented languages.

## State of the Project

### Supported Languages
Current native language support (mostly in chronological order):
1. [Brainfuck](https://esolangs.org/wiki/Brainfuck)
2. [Fluffle Puff](https://github.com/juju2143/flufflepuff)
3. [Ook](https://esolangs.org/wiki/Ook!)
4. [WhiteSpace](https://esolangs.org/wiki/Whitespace) ([as defined here](https://web.archive.org/web/20151108084710/http://compsoc.dur.ac.uk/whitespace/tutorial.html))
5. [WhiteSpace Assembly](https://github.com/Dash-Lambda/Eso/blob/master/WhiteSpaceSyntax.txt)
6. [FracTran](https://esolangs.org/wiki/Fractran)
7. [FracTran++](https://esolangs.org/wiki/Fractran%2B%2B)
8. [Thue](https://esolangs.org/wiki/Thue)
9. [P''](https://esolangs.org/wiki/P%E2%80%B2%E2%80%B2)
10. [///](https://esolangs.org/wiki////)
11. [Deadfish](https://esolangs.org/wiki/Deadfish)
12. [Emmental](https://esolangs.org/wiki/Emmental)
13. [Befunge-93](https://esolangs.org/wiki/Befunge#Befunge-93)
14. [Befunge-98](https://esolangs.org/wiki/Funge-98) (Handprint: 1165193033)
15. [Wierd](https://esolangs.org/wiki/Wierd) ([Compliant with wierd-new.c](https://github.com/graue/esofiles/blob/master/wierd/impl/wierd-new.c))
16. [Unlambda](https://esolangs.org/wiki/Unlambda)
17. [Grass](https://esolangs.org/wiki/Grass)
18. [Path](https://esolangs.org/wiki/PATH)
19. [SNUSP](https://esolangs.org/wiki/SNUSP)
20. [Metatape](https://esolangs.org/wiki/Metatape)
21. [Prelude](https://esolangs.org/wiki/Prelude)
22. [NULL](https://esolangs.org/wiki/NULL)
23. [Volatile](https://esolangs.org/wiki/Volatile)
24. [Glypho](https://esolangs.org/wiki/Glypho)
25. [Platts](https://esolangs.org/wiki/Platts)
26. [WordLang](https://github.com/WilliamRagstad/WordLang)
27. [LazyK](https://esolangs.org/wiki/Lazy_K)
28. [ALPL](https://esolangs.org/wiki/ALPL)
29. [LazyBird](https://esolangs.org/wiki/Lazy_Bird)
30. Scala

### Current features:
* Run programs from text files
* Feed input from text files
* Log output to text files
* Automatic file associations for known extensions
* Naive, optimizing, and compiling BrainFuck interpreters
* Translate to and from compatible languages
* Write WhiteSpace programs with a readable assembly language
* Transpile between certain languages
* Compile and run Scala source files
* Create and use user-defined BrainFuck languages
* User-configurable runtime parameters (logging, maximum output size, tape size, etc.)
* User-configurable bindings ("bind \<token\> \<command\>", "unbind \<token\>")
* Befunge-98 fingerprint support
    * BOOL
    * MODU
    * ROMA
    * NULL
    * HRTI
    * REFC
    * CPLI

### WIP:
* REPL mode
* Multiline bindings
* Automated testing
* Compiler memory (to avoid unnecessary recompiling)
* Additional components for supported languages
* Maybe language component-side error handling
* Debug features
* Potentially everything

### Languages Under Consideration
This is not an exhaustive list, but here are some of the languages I've considered, am considering, or am currently working on.
* [Unispace](https://esolangs.org/wiki/Unispace) (difficulty with finding particularly good documentation)
* [Funciton](https://esolangs.org/wiki/Funciton) (difficulty handling lazy input with its reversed string encoding)
* [Glass](https://esolangs.org/wiki/Glass) (specification is a bit too ambiguous about scope handling)
* [Marscapone](https://esolangs.org/wiki/Mascarpone)
* [INTERCAL](https://esolangs.org/wiki/INTERCAL) (I'll get there... Eventually...)
* [Malbolge](https://esolangs.org/wiki/Malbolge) (... ... ... ... We'll see...)

## Design of Eso
As I mentioned earlier, Eso started out as a simple, single functional-style BrainFuck interpreter. Since then it's gotten... Complicated. Though I prefer the word "sophisticated".

To clarify what is meant by "Functional Esoteric Language Interpreter": All of the language components (with the sole exception of the Scala component) are purely functional. The user interface is not purely functional, but its structure borrows a lot from functional style.

### Language Components
Languages supported by Eso can currently have 3 types of components:
* Interpreter: This is the basic requirement to support a language in Eso, and enables it to run programs in the language. An interpreter is a curried function with the (pseudocode) signature (configuration => (program => (input => output))), where the configuration is a collection of parameters for any optional features or behaviors the interpreter may have and the program is the source code. This means it defines a relationship between the configuration and the relationship between the program and the relationship between the input and the output... All that means is that when you call the interpreter you don't just get the output, you get another function that takes the input and returns the output.
* Translator: Some languages have derivatives (BrainFuck has many). A translator defines a relationship between the source code of two languages with one-to-one equivalence, which means you can translate the code freely between the languages without changing the structure of the program. These have a signature of (configuration => (program1 => program2)). Currently translators are used to support BrainFuck derivatives and enable the use of a readable assembly version of WhiteSpace.
* Transpiler: These define a relationship between non-equivalent languages. A transpiler is one-way, as it changes the structure of the program. These have a signature of (configuration => (program1 => program2)). Transpilers are currently used for compiling interpreters to translate the code into Scala.

### Parsers
One of the first things everyone would ask me when I started telling people about Eso is "what are you using for parsing?"

I didn't really understand the question at first, because each language's parser is different. I wrote the boilerplate code every time. But, as I wrote more and more parsers, I started to understand why they asked: There's a _lot_ of boilerplate.

So I started looking into parser libraries, then parser combinator libraries, then just decided I'd write my own damn parsing tools.

So, Eso has two general forms of parsers: Bespoke parsers, which are written by hand boilerplate and all, and abstracted parsers, which use Eso's set of parser tools to more cleanly and elegantly put together a parser. I only use bespoke parsers when there's a significant gain in performance (and I care enough about performance), or in any old parsers I overlooked when overhauling for the new tools.

The parser tools are a collection of function classes that take an input and return either a parse failure or a structure with the next parsed token, the remaining input, and information on where the token was found.

The parent class additionally implements methods to iterate through matches and a set of combinators and modifiers that currently consist of:
* `map`: Maps the output of the parser
* `withConditioning`: Modifies the input before it's parsed
* `>>`: Pipes the result of the left-hand parser into the right-hand one (one left result is a whole right input)
* `<+>`: Groups parsers such that the result comes from the earliest and longest match
* `|`: If the left-hand parser fails, the right-hand parser is used
* `<&>`: Turns the result into the product of the left-hand and right-hand parsers (the right-hand parses the remaining input after the left-hand)
* `<&`: Gives the left-hand result only if both parsers succeed (basically `<&>` but selecting the left)
* `&>`: Gives the right-hand result only if both parsers succeed (basically `<&>` but selecting the right)
* `~>`: Streams the output of the left-hand parser into the right-hand one (Like `>>` except across all matches)
* `*`: Returns a parser that parses the entire input (basically making parseAllValues the result)
* `+`: Like `*` except it fails if there are no matches

There are currently 6 primary parser classes:
* ChunkParser: Takes a function which returns an `Option`
* PartialParser: Similar to a ChunkParser except it takes a PartialFunction
* RecurParser: This one is for parsing to tree structures, currently used in Unlambda. It takes a `recur` function that decides when to go down a level, a `collect` function to turn a set of leaves into a node, and another parser for the non-branching tokens.
* RegexParser: This takes a regex and a function to turn a match into the output. This one lets me condense a lot of parsers into a single line, which makes me giddy.
* ArbitraryRecurParser: This one's called "arbitrary" because the depth isn't constant. It's given a function that can either parse the next token or tell it to go up or down a level.
* ElementwiseParser: This one parses a sequence element by element
* ScanParsers: These are a few parsers that take a function that transforms two collections at once. They're meant for parsing algorithms that transform the input like a zipper.

### How the Interface Works
It's always a challenge to handle UI functionally. Everything behind the scenes is fair game, but actually receiving input from and sending output to the user is by definition a side-effect.

The philosophy I took with Eso's interface is to focus on modularity without overcomplicating things with a doomed crusade for pure functional code. 

#### Persistent vs Non-Persistent Interfaces
Eso has two different interfaces, persistent and non-persistent.

The non-persistent interface is the entry point; it parses the input, executes it, then exits.
This interface is useful for quick actions and working with other tools, for example:
```
>eso transpile -sl BrainFuck -tl C++ -s lostKingdom.b -o lostKingdom.cpp -init 1000 -dyn true -indent true
Transpiled program saves to lostKingdom.cpp.
>gcc lostKingdom.cpp -o lostKingdom
>lostKingdom.exe
...
```

If the non-persistent interface receives either no arguments or the command `persistent`, it starts the persistent interface. This interface takes over the console, so you cannot use other tools in the same terminal while it's running. The advantage of this is that it can maintain environment variables, such as runtime parameters, bindings, and user-defined translators.

If you supply `persistent` with arguments, the persistent interface initializes the corresponding environment variables to the provided values.

#### The Command Parser
Eso parses each line of input into two components: A command and an argument list

The command is just a string, while the arguments are read as a list of name-value pairs in the form `command -name1 value1 -name2 value2 ...`, then stored in a hash map. Boolean options listed without a value default to true.

To make bindings relatively seamless, the bind/unbind commands as well as any active bindings are intercepted before reaching the parser. The bind and unbind commands are instead parsed as `bind <token> <command>` and `unbind <token>`, and any active bindings are replaced with the bound command then sent to the parser.

#### States and Configurations
Many of the languages supported by Eso have configurable options or parts of their environment, like optional dynamic tape size, random number generators, or even timers. Rather than using simple global values, which would severely impact modularity and disregard the functional style, Eso wraps all these into a data structure to pass down to components.

There are two structures used for this:
* States: These hold all the active language components, parameters, bindings, and anything else used by the interface.
* Configs: These hold the options, parameters, and environment variables used by the language components.

The interface maintains a State, and when needed uses that State to build a Config which it can pass to the language components.

Any language features which would otherwise break the functional style are pushed to the Config. For instance, there is a Befunge instruction which makes a random decision; instantiating a random number generator from within the interpreter would require either using the same seed every time or breaking referential transparency, so the Config carries its own random number generator, thus making the generator itself an input to the function.

#### Command Handlers
Eso originally used a single monolithic interface, but that grew cumbersome. The current design uses objects called "Handlers".

A Handler represents a single command, and it holds three things:
* Its name, used by the main interface to connect it to the parsed command.
* A description of its parameters, used by the "help" command.
* A method that executes the command.

A Handler is essentially a function with the signature (State => (Arguments => State)) (it isn't necessarily a pure function, but it's a simple way to look at it). The main interface parses its input, passes its state and any additional arguments to the corresponding Handler, then the Handler executes the command and returns the new state.

This approach makes it very easy to extend Eso with new features. Any new environment variables or components can be added to the State and Config structures without changing any other code, and new commands only require writing the new Handler and adding it to the interface's list of Handlers.

#### Interpreter I/O
As the interpreters are pure functions, internal interaction with the console is out. Instead, Eso leans heavily on something called lazy evaluation.

By default, Scala adopts eager evaluation; every expression is immediately evaluated to its result as it's encountered. My guess is that this is mostly due to Scala's multi-paradigm design, mixing imperative and functional styles.

One of the features common to many functional languages, and indeed available in Scala, is laziness; the ability to hold off evaluation of expressions until they are needed. In Scala, this culminates in one of my favorite data structures: The LazyList.

A LazyList is, as its name implies, a lazily-evaluated linked-list; each element is only evaluated as it is used. This allows you to do lots of things, like a list of *all* the prime numbers, or simple reductions of consecutive filters and mappings to a single traversal, and so on.

In Eso, both the input and output of an interpreter are LazyLists.

The interface builds a LazyList by reading one Char from the input for each element and passes it to the interpreter, where each time it pulls another element off the list it's effectively reading from the console. The interpreter in turn builds a LazyList by essentially running the program in chunks; the main routine is generally a recursive function that executes instructions until it reaches an output instruction, at which point it returns its current state plus the output. The interpreter sequentially calls this routine for each element, passing the state down the line; when an element is evaluated, the routine is called and steps forward.

The interface actually runs the program by traversing the output list and printing each element.

This preserves pure functionality because LazyLists are immutable; each element is only evaluated once, after that it doesn't change. The input list always represents a single constant input, it just figures out what that input is while it's being used. The interpreter always maps each unique input to a single output.

#### How Translators are Used
Aside from directly translating a program using the command, Eso uses translators under the hood to make the interface more flexible.

For the translate command, Eso doesn't just look for a translator between the two specified languages; it instead looks for the shortest chain of translators that can get it from the source language to the target language, then composes the chain into a single function. So, if you translate a program from Ook to FlufflePuff, Eso is actually translating Ook>BrainFuck>FlufflePuff.

When you use the run command, Eso searches for a chain from the source language to the nearest (i.e. shortest translation path) available interpreter. If it finds one, it automatically translates.

The transpile handler searches for the shortest chain from the source language to a known transpiler to the target language; bear in mind that it will only use a single transpiler.

Eso can do this with translators because they are guaranteed to be invertible and to preserve the structure of a program. In essence, no matter how many times you translate it, the program is guaranteed to be the same program. Transpilers can freely change how a program works (they just need to preserve the behavior), which means they're more flexible in terms of functionality but less flexible in terms of how they can be manipulated.

### Environment Variables
The environment maintained by Eso has collected quite a few parameters, detailed here:
* Booleans
    * appendInp: Toggle whether or not to append console input to file input. Primarily for self-interpreters who expect both the program and user input from the same sourc.
    * bfDiv: A flag dictating what to do when Funge-98 encounters division by zero. The spec allows it to either error out or return 0, most interpreters prompt the user at runtime but in Eso it's just an environment flag. True returns 0.
    * bfRetCode: True prints the Funge-98 return code at the end of execution, right above the "program complete" message if you have that on.
    * dfChar: DeadFish, which I believe is the first non-Turing-complete language in Eso, only prints numbers -it cannot print characters. Setting this flag to true interprets DeadFish output as characters anyway.
    * dyn: This flag controls dynamic tape size where applicable. True simulates an infinite memory tape, false, is fixed size. Currently used by BrainFuck, PATH, and SNUSP.
    * echoFileInp: Toggle whether or not to echo file input to the console. This will print file input as if it were user input.
    * fPtr: P\'\' doesn't have output commands, so I had to improvise -at the end of the program, the tape is printed. This flag controls the direction: True starts at the read head and goes right, false starts at the origin and goes left.
    * indent: For curly-bracket languages, this flag controls whether transpilers neatly indent everything or don't indent at all.
    * log: Toggle detailed logging, which just means Eso will tell you what it's doing and when it's done.
    * normLineBreaks: For some reason the definition of a new line varies between platforms, this flag turns all line breaks into `\n`
    * pNull: Toggle whether or not to print the null character for P\'\' output.
    * preludePar: The Prelude spec says the voices run simultaneously, this gives you the option between parallel and sequential.
    * printNum: Toggle whether or not to convert the character stream to integers. If true, the output is a list of the ASCII codes of the characters rather than the characters.
    * sHead: Toggle which end of the tape the head starts on for P\'\'. True means right.
    * time: Toggle whether or not to print how long a program took to run.
* Numbers
    * bfOpt: BrainFuck optimization level. 0 is no optimization (naive), 1 is optimizing, and 2 is compiling.
    * fileEOF: The ASCII code of the character Eso sticks at the end of file input. Most languages expect 0, but sometimes it needs to be something else. For instance, cgbfi.b expects a program terminated by `!`, and anything following is considered input.  
    * init: Initial length of memory tape, where applicable.
    * methSize: The maximum number of blocks allowed in a method when breaking up generated code to fit in the JVM's nonsensical limits.
    * charWidth: Bit-width of characters used by Metatape.
    * olen: Maximum number of characters to print. This is primarily useful for non-terminating programs. Some transpilers ignore this. 

Boolean values can be set to true with an argument matching the regex `(true|yes|t|y|[1-9])` and false matching `(false|no|f|n|0)`, both case-insensitive.

Numeric values may be set either with a numeric argument or with a character in single quotes `'c'` (in which case it will be set to that character's ASCII code).

### Config Environment
As mentioned earlier, one thing that was necessary to maintain a functional design was to make the environment an input, thus Eso hands off a Config object to language components.

The Config object currently holds:
* All of the main environment variables above
* A pseudorandom number generator
* A LazyList of times, handled the same way as user input.

## Notes

### BrainFuck Optimization Strategy
The optimizing BrainFuck interpreter translates the program into an intermediate language in a series of passes:
1. Clear loops (loops that set the current cell to 0) are replaced with a single instruction.
2. Repeated instructions are contracted into one instruction. For instance, >>>>> would be contracted to (>,5).
3. Unbroken sequences of shifts, increments/decrements, and clears are contracted into single 'bulk' operations.
4. Copy/multiply loops are replaced with a single instruction.
5. Brackets are assigned the index of their partner to eliminate scrubbing.

### User Defined BrainFuck Translators
The best way to define your own translator is to use the command `defineBFLang` and follow the prompts.

The translators are stored in a JSON, and the names of the translators are listed in an array names "names", thus you are not able to define a BrainFuck translator with the name "names".

### On the Scala "Interpreter"
The Scala 'interpreter' compiles the source code to an abstract syntax tree, defines a symbol with that tree, then tries to run the standard "main(args: Array[String]): Unit" method.

Its original purpose is to run programs generated by Eso's transpilers. Currently, the Scala 'interpreter' and the internal compiling interpreters are different; The internal compiling interpreters cast the code to a specific class, while the Scala 'interpreter' only assumes there's a main method. This ultimately means the internal ones are faster but the Scala language component is more versatile. 

### FracTran Program Format
The FracTran interpreter reads programs as an initial value followed by a list of fractions of the form "n/d", each term separated by a line break. The prime generator program looks like this:
```
2
17/91
78/85
19/51
23/38
29/33
77/29
95/23
77/19
1/17
11/13
13/11
15/2
1/7
55/1
```
FracTran++ has identical structure to (and is fully compatible with) FracTran, but with additional syntax detailed [here](https://github.com/Dash-Lambda/Eso/blob/master/FracTranpp_Syntax.txt).

### P'' Program Format
The P'' interpreter handles Böhm's extended instruction set, with the seven instructions (, ), r, r', L, R, and A. The first line is the ordered alphabet (the first character is the empty symbol), the second line is the initial tape state (blank for empty tape), and all following lines contain the program.

Hello world looks like this:
```
.Helo wrd!
.
r'L
r'r'L
rrA
r'r'r'L
rrrA
rrrrrA
rrrrA
rrrA
rrA
rrA
rA
A
```

### On the Befunge-98 Interpreter
This is by far the biggest, most complex, and most difficult to get right interpreter in Eso, as it will likely remain for some time.

For a while, there was a glaring issue in the Mycology report: Eso didn't report stack size correctly. There was a very simple reason for this; the Befunge-93 interpreter handles bottomless stacks with LazyLists, meaning the stack listerally is an endless list of 0s. Moving this over to Befunge-98 posed an issue, because Funge-98 adds the ability to query stack sizes, which is always infinite with the LazyList approach.

My first approach to resolve this was just to report the stack size as -1, but that obviously wasn't expected behavior. Eventually, I ended up writing a new collection type that keeps a finite Vector of elements but behaves like a bottomless list when elements are extracted. 

Another difficulty comes from the fact that Funge-98 allows the program to query a clock/timer.

\*long, shaky sigh\*

Ultimately, I settled on a design where clock reads are handled similarly to user input, where it's an immutable LazyList of clock reads that is evaluated as it is used.

Of course, the Funge-98 interpreter also does not support file I/O or the system.exec instruction, as this would break the functional style. Thankfully, these features are actually optional in the spec. 

Expect more fingerprints to become available over time. ... Slowly, probably. Whenever I get bored between new language components.

### On the LazyK Interpreter
The LazyK parser doesn't support mixing dialects in the same source file. That feature had only one mention in the entire spec, and the author put that mention there to say he didn't know how he felt about it -and implementing it with my current set of parser tools would mean bringing an ugly, inelegant monstrosity into this world where I otherwise have a parser so neat and tidy it makes me smile. Sorry, I don't plan to implement that unless my parser tools evolve to the point where it becomes a 3-4 line solution.

This interpreter is also a little unique in that it's the only interpreter in Eso that uses a mutable state. When an S combinator transforms (Sxyz) into (xz)(yz), the duplicated function z is wrapped into a special expression that collapses _both_ occurrences to the result when either one is evaluated. I did this because the performance gain is literally more than a thousand times.

The reason I had to use mutable states to accomplish this is that I have to perform the evaluation in the same level as the rest of the expression in order to not blow up the call stack. If I simply made the result a `lazy val` that made a new call to `eval`, that would introduce a recursion that can't be optimized away with trampolining. The moment I figure out a way to do this without mutable state, though, I'm changing it.

### How the Interface Code Works
You may notice odd-looking structures like this in the code for several interface handlers:
```scala
Trampoline.doOrElse(state){
  DoOrOp(args.get("s"), "Missing Source File", eio){src =>
    DoOrOp(getLang(args, "l", "s"), "Unrecognized Language or File Extension", eio){lang =>
      DoOrOp(findTranslator(state, lang, state.interpNames), "Language Not Recognized", eio){
        case (inam, t) =>
          DoOrErr(EsoFileReader.readFile(src, normLineFlag), eio){progRaw =>
            DoOrErr(t(progRaw), eio){prog =>
              TimeIt(state.interps(inam)(state.config)(prog)) match{
                case (i, dur) =>
                  DoOrErr(i, eio){r =>
                    DoOrErr(inputs, eio){inp =>
                      TimeIt{tryAll{printer(olim(r(inp)))}} match{
                        case (flg, rdr) => flg match{
                          case Failure(e) =>
                            if(timeFlg) eio.println(s"\nError: $e\nProgram failed in ${rdr}ms")
                            else eio.println(s"\nError: $e")
                          case Success(_) =>
                            if(timeFlg) eio.println(s"\nProgram completed in ${rdr}ms")
                            else eio.println()}}
                      Done{state}}}}}}}}}}
```
These actually represent most of the core logic. Eso uses `Try` monads for most of its error handling, as well as `Option`s for a lot of faillable tasks, so if the code were to use a normal imperative or structural style there would be a lot of redundant code for checking whether or not something failed and breaking the flow.

These structures just nest each step into the previous one it's dependent on. You can see that the first layer tries to get the source file, then the second layer tries to find the language to use, and so-on. Each layer tries to do something, and either passes the result to the next layer or short-circuits with a failure. This works out to be quite an elegant way to represent multi-step IO interactions.

You'll also notice that the whole thing is wrapped in a `Trampoline` method. If you were to just arbitrarily nest things like this, you would be pointlessly eating up the call stack -so instead, each layer is actually a wrapper that returns the next layer. So, during execution it's actually going into a layer then coming out of that layer with the next one, only ever going down into one layer at a time. This is called trampolining, and it's a common way to optimize recursive structures that could otherwise blow up the call stack.

Of course, Eso wouldn't need special handling of trampolines if it had full TCE, but that's a matter for future JVM versions to address.

###Abstraction Eliminators
For some combinator-calculus languages without support for lambda expressions, Eso provides abstraction eliminators. These are special transpilers that turn all lambda expressions into combinator expressions. For example, if you wanted to run abstraction elimination on an Unlambda program, you would type:
```
Eso> transpile -s example.txt -o example.unl -sl Lambda_Calculus
```

### Building
I use [SBT assembly](https://github.com/sbt/sbt-assembly). This repo should give you everything you need to build it, just put it in a folder, run SBT from that directory, let it do its thing, then run assembly.