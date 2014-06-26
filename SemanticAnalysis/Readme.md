Semantic analysis performs some checks as to validate the code:

- Building of symbol tables per module
	-> Created locally (assumes correct types)
	-> New types (classes etc)
	-> Imported functions and types
	-> Exported functions (and types, if a type is exported)
- Building type tables
	-> Supertype/Subtype relationships
- Building symbol tables inside expressions
	-> Maps (name -> (Type, docstring, stringrep)) and with parent
	-> Will later be extended with let-in and lambda's
- Typechecks of the expressions
	-> Correct (multi)-declarations
	-> Picking the right function
- Simple precondition propagator
	-> With warnings only
- Simple law check
	-> Exhaustive with non-recursive/non-infinite datatypes
	-> Random values in other cases

The type declarations work as some kind of checkpoint. Infers are not supported in this version.
