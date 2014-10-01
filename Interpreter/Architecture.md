How is the interpreter organized?
---------------------------------

The interpreter works with a few data types:

ADT	: a simple, stupid value. (Nats are encoded as these too)
Float	: a floating point value
VCall	: a name that should still be resolved using the context
App Lambda Arg	: application of a function on the given argument

These have to be evaluated in their own environment, consisting of:
-> The world. All code, in a FQN -> Module manner
-> The current module, noted as an FQN
-> A symboltable with the local binding (the input arguments)

These context can be built up locally, and then passed on in the reader monad.

Notice that all values are typed


Implementation
--------------

InterpreterDef	: all data and type declarations
Interpreter	: hub to the actual implementation files.
Interpreter.Application	: application of an argument against a value.
Interpreter.Utils	: some usefull functions to use in the interpreter

Interpreter.Tools	: info command, which gives nice docs to print.
