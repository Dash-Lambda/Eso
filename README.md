# Functional Esoteric Interpreter
### Meaning of functional
Functional programming is a paradigm characterized by zero side-effects. The idea is to build your program around evaluating functions, where the functions take an input and return an output without modifying anything else. The biggest benefits of functional programming are safety and modularity, and in many cases concision.

### Usage of funtional programming in this project
The interpreters and translators are -mostly- purely functional, the UI is not. Functional programming is great for backend/core code, but it's not ideal for interactive elements. The only deviation from functional style is the optional console logging during runtime, which is for interactive programs.

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
* Interpreters: An interpreter executes a program; or, thinking functionally, it defines a relationship between a program string and its result string. An interpreter is the bare minimum for a language to be supported.
* Translators: A translator defines a relationship between exactly equivalent languages. Two languages are exactly equivalent if they share identical structure and are executed in identical ways, meaning a translator merely changes the syntax. This also means translators are two-way. A translator can be used to add support for derivative languages, as well as to define intelligible languages to make unintelligible languages (like WhiteSpace) easier to use.
* Generators: A code generator defines a relationship between equivalent programs in non-equivalent languages. The primary purpose of a generator is to turn slow interpreted code into code that can be compiled, as demonstrated by the compiling BrainFuck interpreter. Even though the original and generated programs are equivalent, meaning they perform the same computation, their structure is not necessarily equivalent, meaning generators are only one-way.

#### Current features:
* Run program from text file
* Unoptimized, optimized, and compiled BrainFuck interpreters
* Long and SafeLong based WhiteSpace interpreters
* Translate to and from compatible languages
* Write WhiteSpace programs with a readable assembly language
* Generate Scala code from BrainFuck programs
* Compile and run Scala source files
* Create and use user-defined BrainFuck languages
* User-configurable runtime parameters (logging, maximum output size, tape size, etc.)
* Debug mode
* Optionally slow down execution in debug mode in some interpreters

##### WIP:
* Compiler memory (to avoid unnecessary recompiling)
* Dynamic tape size for compiled BrainFuck interpreter
* Unispace interpreter
* Additional languages and interpreters
* Modularization
* Potentially everything

### Optimization Strategy
The first major difference in the optimizing BrainFuck interpreter is its program and data tapes. Instead of keeping two lists for each one and modifying them at each step, it uses a single static program tape and a single data tape, keeping track of its position in each with a counter. Both are stored using Vectors.

The optimizer performs a series of passes over the program:
1. Filter out all non-BrainFuck characters and replace all clear loops with a single instruction '_'.
2. Contract all sequences of repeated instructions with a single instruction. For instance, +++++ becomes (+,5).
3. Collect all unbroken sequences of pointer movements and increments/decrements with single 'bulk' operations that perform the entire sequence in a single large step. Bulk operations are represented by 'u'.
4. Replace all copy/multiplication loops with a single instruction. This amounts to finding all blocks of the form "[u]" where u does not shift the pointer and decrements the current value by 1, and replacing them with a single 'l' which performs the bulk operation once while multiplying all the increments/decrements by the current value.
5. Pair every bracket with the index of its corresponding bracket. This eliminates the need to scrub through the program looking for the next bracket on every jump or skip.

### User-Defined BrainFuck Translators
There are two ways to define your own BF language:
* Use the console prompt, which will ask you for the language name and syntax then handle the rest.
* Make a text file containing your language's information in this form:
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
The main purpose of the Scala interpreter is to run Scala source files generated by Eso's compiler. It assumes the source file is a Function0[String] definition of the form "new Function0[String]{...}", and returns the result of the defined function. It can run arbitrary Scala code as long as it's in that form.

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
