# Functional Esoteric Interpreter
### Meaning of functional
Functional programming is a paradigm with three principle characteristics:

##### First-Class Functions
Functional programming gets its name from the fact that functions are treated as first-class values. This means you can use and manipulate functions just as you would any other variable or parameter.
##### Zero Side-Effects
A function defines a relationship between an input and an output. This means a function does not change anything outside of itself. One major example of this is error handling: In most languages, an error is thrown by a method and prevents a return value, and thus must be caught separately. This means code can have unexpected behavior that extends beyond the return, and it can be a pain to deal with. In functional programming, errors are usually handled by returning a wrapper that holds either a result or error information, which eliminates behavior that the caller does not expect.
##### Referential Transparency
A function is also stable in that each input returns exactly one output. You should be able to replace any call to a function with the result and not change the behavior of the program, which means a function has no mutable state. As an extension of this, functional programming also makes heavy use of immutable data structures, which are data structures that cannot be changed in place.

Ultimately, what functional programming accomplishes is highly modular, testable, and type-safe code. Many functional languages also allow for far more elegant and concise code than is common of imperative and object oriented languages.
### Usage of funtional programming in this project
All of the language components are purely functional. The user interface is not purely functional, but its structure borrows a lot from functional style.

Many of these languages support user I/O, which can be difficult because I/O is by definition a side-effect. People have found many ways around this, the most common being to simply *not* write the I/O component in functional style. This project handles I/O using lazily evaluated lists, which are immutable lists whose elements are only evaluated as they are used.

The interpreters take as input a sequence of characters and return a LazyList of characters. To print the output, the interface calls the interpreter then traverses the output list and prints each character. In effect, this means the interpreter operates in steps; as the list is traversed, the interpreter calls a function to find the next character in succession over and over until the program halts.

Input is a little bit of a cheat. The sequence of characters an interpreter intakes is a "Seq", which encompasses any ordered sequential collection type, including LazyLists. So, to get characters from the user, the LazyList is built by getting a character from the console for each element. Since they're evaluated on-demand, this allows real-time user input inside of a pure function. This preserves referential transparency because you will always get the same output for a given input, but bends the rules slightly by deciding what the input is as it's being used. This also technically eliminates side-effects because the interpreter doesn't have any knowledge of how its input is being built.
### State of the project
Current Native language support:
* [Brainfuck](https://esolangs.org/wiki/Brainfuck)
* [Fluffle Puff](https://github.com/juju2143/flufflepuff)
* [Ook](https://esolangs.org/wiki/Ook!)
* [WhiteSpace](https://esolangs.org/wiki/Whitespace) ([as defined here](https://web.archive.org/web/20151108084710/http://compsoc.dur.ac.uk/whitespace/tutorial.html))
* [WhiteSpace Assembly](https://github.com/Dash-Lambda/Eso/blob/master/WhiteSpaceSyntax.txt)
* [FracTran](https://esolangs.org/wiki/Fractran)
* [FracTran++](https://esolangs.org/wiki/Fractran%2B%2B)
* [Thue](https://esolangs.org/wiki/Thue)
* [P''](https://esolangs.org/wiki/P%E2%80%B2%E2%80%B2)
* [///](https://esolangs.org/wiki////)
* [Deadfish](https://esolangs.org/wiki/Deadfish)
* [Emmental](https://esolangs.org/wiki/Emmental)
* [Befunge-93](https://esolangs.org/wiki/Befunge#Befunge-93)
* [Befunge-98](https://esolangs.org/wiki/Funge-98)
* [Wierd](https://esolangs.org/wiki/Wierd) ([Compliant with wierd-new.c](https://github.com/graue/esofiles/blob/master/wierd/impl/wierd-new.c))
* [Unlambda](https://esolangs.org/wiki/Unlambda)
* [Grass](https://esolangs.org/wiki/Grass)
* [Path](https://esolangs.org/wiki/PATH)
* Scala

#### Language Components:
Languages supported by Eso can currently have 3 types of components:
* Interpreter: This is the basic requirement to support a language in Eso, and enables it to run programs in the language. An interpreter is a curried function with the (pseudocode) signature (configuration => (program => (input => output))), where the configuration is a collection of parameters for any optional features or behaviors the interpreter may have and the program is the source code. This means it defines a relationship between the configuration and the relationship between the program and the relationship between the input and the output... All that means is that when you call the interpreter you don't just get the output, you get another function that takes the input and returns the output.
* Translator: Some languages have derivatives (BrainFuck has many). A translator defines a relationship between the source code of two languages with one-to-one equivalence, which means you can translate the code freely between the languages without changing the structure of the program. These have a signature of (configuration => (program1 => program2)). Currently translators are used to support BrainFuck derivatives and enable the use of a readable assembly version of WhiteSpace.
* Transpiler: These define a relationship between non-equivalent languages. A transpiler is one-way, as it changes the structure of the program. These have a signature of (configuration => (program1 => program2)). Transpilers are currently used for compiling interpreters to translate the code into Scala.

#### Current features:
* Run programs from text files
* Feed input from text files
* Log output to text files
* Automatic file associations for known extensions
* Unoptimized, optimized, and compiled BrainFuck interpreters
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

##### WIP:
* REPL mode
* Multiline bindings
* Automated testing
* Compiler memory (to avoid unnecessary recompiling)
* Additional components for supported languages
* Maybe language component-side error handling
* Debug features
* Potentially everything

##### Languages Being Worked On
* [Unispace](https://esolangs.org/wiki/Unispace) (difficulty with finding particularly good documentation)
* [Funciton](https://esolangs.org/wiki/Funciton) (difficulty handling lazy input with its reversed string encoding)

### How the Interface and Command Parser Works
Eso has two different interfaces, persistent and non-persistent.

The non-persistent interface is the entry point. If it receives no arguments from the console, or if it receives the single argument "persistent", it starts the persistent interface; otherwise, it executes the command.
This interface is useful for quick actions and working with other tools, for example:
```
>eso transpile -sl BrainFuck -tl C++ -s lostKingdom.b -o lostKingdom.cpp -init 1000 -dyn true -indent true
Transpiled program saves to lostKingdom.cpp.
>gcc lostKingdom.cpp -o lostKingdom
>lostKingdom.exe
...
```

The persistent interface takes over the console, so you cannot use other tools while it's running. The advantage of this is that it can maintain environment variables, such as runtime parameters, bindings, and user-defined translators.

Both interfaces parse their arguments into a leading command followed by a list of options. Command-specific options can be seen with "help", and the non-persistent interface allows you to additionally pass name-value pairs to configure runtime parameters.

Internally, each interface has a "state" object that holds its current environment (parameters, interpreters, translators, transpilers, etc...) and a set of handlers, which are special function objects that are responsible for individual commands.

When the interface receives a command and parses it, it tries to match the command tag to the name of a handler. If it finds a matching handler, it passes the handler the current state and the parsed arguments, and the handler returns a new state; the persistent interface makes this new state the current state, while the non-persistent interface throws it away. If it doesn't find a handler, then it complains with an error message.

The idea behind the state/handler approach is pure modularity. It is very simple to add new functionality by writing a new handler and plugging it in or adding new parameters to the state; far simpler than it would be to modify a monolithic interface object directly. 

### How Translators are Used
Aside from directly translating a program using the command, Eso uses translators under the hood to make the interface more flexible.

For the translate command, Eso doesn't just look for a translator between the two specified languages; it instead looks for the shortest chain of translators that can get it from the source language to the target language, then composes the chain into a single function. So, if you translate a program from Ook to FlufflePuff, Eso is actually translating Ook>BrainFuck>FlufflePuff.

When you you a command, Eso searches for a chain from the source language to the nearest (i.e. shortest translation path) available interpreter. If it finds one, it automatically translates.

The transpile handler searches for the shortest chain from the source language to a known transpiler to the target language; bear in mind that it will only use a single transpiler.

Eso can do this with translators because they are guaranteed to be invertible and to preserve the structure of a program. In essence, no matter how many times you translate it, the program is guaranteed to be the same program. Transpilers can freely change how a program works (they just need to preserve the behavior), which means they're more flexible in terms of functionality but less flexible in terms of how they can be manipulated.

### BrainFuck Optimization Strategy
The optimizing BrainFuck interpreter translates the program into an intermediate language in a series of passes:
1. Clear loops (loops that set the current cell to 0) are replaced with a single instruction.
2. Repeated instructions are contracted into one instruction. For instance, >>>>> would be contracted to (>,5).
3. Unbroken sequences of shifts, increments/decrements, and clears are contracted into single 'bulk' operations.
4. Copy/multiply loops are replaced with a single instruction.
5. Brackets are assigned the index of their partner to eliminate scrubbing.

This is all done using LazyLists, so every pass is performed in a single traversal.

### User-Defined BrainFuck Translators
There are two ways to define your own BF language:
* Use the console prompt, which will ask you for the language name and syntax then handle the rest.
* Make and import a text file containing your language's information in this form:
```
#name=...
[=>...
]=>...
>=>...
<=>...
+=>...
-=>...
.=>...
,=>...
```

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

### FracTran++ Program Format
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
You'll notice in the Mycology report that there is one thing Eso's Funge-98 interpreter fails at: Reporting stack size.

The stack size is reported weird because Funge's stacks are bottomless. When you try to pop an empty stack, instead of throwing an error, it returns a 0; Eso's implementation handles this quite naturally with LazyLists, treating each stack on the stack-stack as an endless list of 0s. This means the size is always infinite, so Eso reports the size as -1.

I'm looking into solutions for this, but I don't currently see it as a large problem.

Expect more fingerprints to become available over time. ... Slowly, probably. Whenever I get bored between new language components.
