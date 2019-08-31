# Functional Esoteric Interpreter
### Meaning of functional
Functional programming is a paradigm with two principle characteristics:

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
* Scala

#### Language Components:
Languages supported by Eso can currently have 3 types of components:
* Interpreter: This is the basic requirement to support a language in Eso, and enables it to run programs in the language. An interpreter is a curried function with the (pseudocode) signature configuration => (program => (input => output)), where the configuration is a collection of parameters for any optional features or behaviors the interpreter may have and the program is the source code. This means it defines a relationship between the configuration and the relationship between the program and the relationship between the input and the output... All that means is that when you call the interpreter you don't just get the output, you get another function that takes the input and returns the output.
* Translator: Some languages have derivatives (BrainFuck has many). A translator defines a relationship between the source code of two languages with one-to-one equivalence, which means you can translate the code freely between the languages without changing the structure of the program. These have a signature of configuration => (program1 => program2). Currently translators are used to support BrainFuck derivatives and enable the use of a readable assembly version of WhiteSpace.
* Generator: These define a relationship between non-equivalent languages. A generator is one-way, as it changes the structure of the program. These have a signature of configuration => (program1 => program2). Generators are currently used for compiling interpreters to translate the code into Scala.

#### Current features:
* Run program from text file
* Log output to text file
* Unoptimized, optimized, and compiled BrainFuck interpreters
* Translate to and from compatible languages
* Write WhiteSpace programs with a readable assembly language
* Generate Scala code from BrainFuck programs
* Compile and run Scala source files
* Create and use user-defined BrainFuck languages
* User-configurable runtime parameters (logging, maximum output size, tape size, etc.)

##### WIP:
* Multiline bindings
* Compiler memory (to avoid unnecessary recompiling)
* Unispace interpreter
* Additional languages and components
* Maybe language component-side error handling
* Debug features
* Potentially everything

### Optimization Strategy
The optimizing BrainFuck interpreter translates the program into an intermediate language in a series of passes:
1. Clear loops (loops that set the current cell to 0) are replaced with a single instruction.
2. Repeated instructions are contracted into one instruction. For instance, >>>>> would be contracted to (>,5).
3. Unbroken sequences of shifts and increments/decrements are contracted into single 'bulk' operations.
4. Copy/multiply loops are replaced with a single instruction.
5. Brackets are assigned the index of their partner to eliminate scrubbing.

This is all done using LazyLists, so every pass is performed in a single traversal.
### User-Defined BrainFuck Translators
There are two ways to define your own BF language:
* Use the console prompt, which will ask you for the language name and syntax then handle the rest.
* Make and import a text file containing your language's information in this form:
```
name=...
[=...
]=...
>=...
<=...
+=...
-=...
.=...
,=...
```

### On the Scala "Interpreter"
The main purpose of the Scala interpreter is to run Scala source files generated by Eso. It assumes the source file is a Function2[BlockingQueue[Option[Try[Char]]], Seq[Char], Runnable] definition of the form "new Function2[BlockingQueue[Option[Try[Char]]], Seq[Char], Runnable]{...}", and returns the result of the defined function. It can run arbitrary Scala code as long as it's in that form.

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
FracTran++ has identical structure to (and it fully compatible with) FracTran, but with additional syntax detailed [here](https://github.com/Dash-Lambda/Eso/blob/master/FracTranpp_Syntax.txt).

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

This is very much a work in progress.
