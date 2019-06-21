# Functional Esoteric Interpreter

This is a purely (or mostly purely) functional interpreter for esoteric programming languages written in Scala.

Current Native language support:
* Brainfuck (https://esolangs.org/wiki/Brainfuck)
* Fluffle Puff (https://github.com/juju2143/flufflepuff)
* Ook (https://esolangs.org/wiki/Ook!)

Current features:
* Run program from text file
* Translate to and from supported languages
* User-defined languages

WIP features:
* Additional languages and interpreters
* Proper UI

To Make your own BF-based languages, make a text file containing your languagess information in this form:

name=<name>
[=<something>
]=<something>
+=<something>
-=<something>
<=<something>
>=<something>
.=<something>
,=<something>

By default the UI looks for a file named "CustomLangs.txt", but the load command accepts an optional file name to point it somewhere else.

This is very much a work in progress.
