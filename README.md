# Functional Esoteric Interpreter

This is a purely (or mostly purely) functional interpreter for esoteric programming languages written in Scala.

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
* Scala

#### Current features:
* Run program from text file
* Unoptimized, optimized, and compiled BrainFuck interpreters
* Translate to and from supported BrainFuck languages
* Compile BrainFuck programs to Scala source files
* Compile and run Scala source files
* Create and use user-defined BrainFuck languages
* User-configurable runtime parameters (logging, maximum output size, tape size, etc.)
* Convert difficult-to-read code (a la WhiteSpace) to and from readable syntax with assemblers
* Debug mode to show interpreter state during runtime

##### WIP:
* Dynamic tape size for compiled BrainFuck interpreter
* Unispace interpreter
* Streamline WhiteSpace interpreter versions (make it generic)
* Additional languages and interpreters
* Modularization
* Potentially everything

### Optimization Strategy
The first major difference in the optimizing BrainFuck interpreter is its program and data tapes. Instead of keeping two lists for each one and modifying them at each step, it uses a single static program tape and a single data tape, keeping track of its position in each with a counter. Both are stored using Vectors.

The optimizer performs a series of 5 passes over the program:
1. Contract all repeated operations into pairs specifying the operation and the number of repititions.
2. Replace all clear loops ([-]) and scan loops ([>], [<], [>>], etc.) with single instructions.
3. Replace all unbroken sequences of shifts and arithmetic with single bulk operations. A bulk operation specifies a collection of offsets and increments to update cells relative to the point, and a final shift to move the pointer. This eliminates all >, <, +, and - operations, replacing them with a single type of operation tagged 'u' (for "update").
4. Replace all copy loops with single operations. This is done by searching for sequences of the form [u] where the final shift is 0 and the cumulative increment on the current cell is -1, and replacing the loop with a signle operation tagged 'l' (for "loop"). An l operation refers to the same bulk operation as the u it replaces, but when executed it will multiply all the increments by the value in the current cell.
5. Find matching brackets and assign their value as the index of their counterpart. This eliminates the need to scrub through the program for the matching bracket at runtime.

### User-Defined Translators
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

This is very much a work in progress.
