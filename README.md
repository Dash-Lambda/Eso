# Functional Esoteric Interpreter

This is a purely (or mostly purely) functional interpreter for esoteric programming languages written in Scala.

### Meaning of functional
Functional programming is a paradigm characterized by zero side-effects. The idea is to build your program around evaluating functions, where the functions take an input and return an output without modifying anything else. The biggest benefits of functional programming are safety and modularity.

### Usage of funtional programming in this project
The interpreters and translators are purely functional, the UI is not. Functional programming is great for backend/core code, but it's not ideal for interactive elements.

### State of the project
Current Native language support:
* Brainfuck (https://esolangs.org/wiki/Brainfuck)
* Fluffle Puff (https://github.com/juju2143/flufflepuff)
* Ook (https://esolangs.org/wiki/Ook!)

Current features:
* Run program from text file
* Optimized and unoptimized BrainFuck interpreters
* Translate to and from supported languages
* User-defined languages

WIP features:
* Additional languages and interpreters (currently trying to figure out the Whitespace language)
* Potentially everything

There are two ways to define your own language:
* Use the console prompt, which will ask you for the language name and syntax and handle the rest.
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
