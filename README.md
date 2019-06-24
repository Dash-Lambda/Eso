# Functional Esoteric Interpreter

This is a purely (or mostly purely) functional interpreter for esoteric programming languages written in Scala.

### Meaning of functional
Functional programming is a paradigm characterized by zero side-effects. The idea is to build your program around evaluating functions, where the functions take an input and return an output without modifying anything else. The biggest benefits of functional programming are safety and modularity, and in many cases concision.

### Usage of funtional programming in this project
The interpreters and translators are purely functional, the UI is not. Functional programming is great for backend/core code, but it's not ideal for interactive elements.

### State of the project
Current Native language support:
* [Brainfuck](https://esolangs.org/wiki/Brainfuck)
* [Fluffle Puff](https://github.com/juju2143/flufflepuff)
* [Ook](https://esolangs.org/wiki/Ook!)
* [WhiteSpace](https://esolangs.org/wiki/Whitespace) ([as defined here](https://web.archive.org/web/20151108084710/http://compsoc.dur.ac.uk/whitespace/tutorial.html))

Current features:
* Run program from text file
* Optimized and unoptimized BrainFuck interpreters
* Translate to and from supported BrainFuck languages
* User-defined BrainFuck languages
* User-configurable runtime parameters (logging, maximum output size, tape size, etc.)
* Convert difficult-to-read code (a la WhiteSpace) to and from readable syntax with assemblers
* Debug mode to show interpreter state during runtime

WIP:
* Streamline WhiteSpace interpreter versions (make it generic)
* User command binding
* Additional languages and interpreters
* Potentially everything

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
