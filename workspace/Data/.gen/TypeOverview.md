# Type overview

Type | Declared in | Kind | Docstring
---- | ----------- | ---- | ---------
**Any**  | Any | ````*````  | The supertype of every single type. Each type has ````Any```` implicitly as supertype.
**Eq**  | Category.Eq | ````*````  | The category which defines _equality_ ````==```` and _inequality_ ````!=````
**Associative** ````a````  | Category.Function | ````(* ~> *)````  | Functions for which the order of evaluation does not matter.
**Commutative** ````a````  ````b````  | Category.Function | ````(* ~> (* ~> *))````  | Functions for which the arguments can be swapped.
**Curry** ````a````  ````b````  | Category.Function | ````(* ~> (* ~> *))````  | Arbitrary functions from ````a```` to ````b````.
**Mappable** ````b:Eq````  | Category.Mappable | ````(* ~> *)````  | A _container_ on which ````map```` is defined. Also known as ````Functor```` in most other functional programming languages.
**X** ````a````  | Category.Mappable | ````(* ~> *)````  | TODO remove
**Y** ````b````  | Category.Mappable | ````(* ~> *)````  | TODO remove
**Z**  | Category.Mappable | ````*````  | TODO remove
**Bool**  | Data.Bool | ````*````  | The ````Bool```` datatype represents truth values of logic.

## Supertypes 

Type | Is subtype of
---- | -------------
Any | Any
Eq | Any
Associative````a0````  | (a0 -> (a0 -> a0))
Commutative````a0````  ````a1````  | (a0 -> (a0 -> a1))
Curry````a0````  ````a1````  | (a0 -> a1)
Mappable````a0````  | Any
X````a0````  | Any
Y````a0````  | Any, (X a0)
Z | Any, (Y Bool)
Bool | Any



### Supertypes of Any

Is type | #Frees | Requirements | Binding
------- | ------ | ------------ | -------
Any | 0 |  | {}

### Supertypes of Eq

Is type | #Frees | Requirements | Binding
------- | ------ | ------------ | -------
Any | 0 |  | {}

### Supertypes of Associative

Is type | #Frees | Requirements | Binding
------- | ------ | ------------ | -------
(a0 -> (a0 -> a0)) | 1 | ````a0````  | {}

### Supertypes of Commutative

Is type | #Frees | Requirements | Binding
------- | ------ | ------------ | -------
(a0 -> (a0 -> a1)) | 2 | ````a0````  ````a1````  | {}

### Supertypes of Curry

Is type | #Frees | Requirements | Binding
------- | ------ | ------------ | -------
(a0 -> a1) | 2 | ````a0````  ````a1````  | {}

### Supertypes of Mappable

Is type | #Frees | Requirements | Binding
------- | ------ | ------------ | -------
Any | 1 | ````a0````  | {}

### Supertypes of X

Is type | #Frees | Requirements | Binding
------- | ------ | ------------ | -------
Any | 1 | ````a0````  | {}

### Supertypes of Y

Is type | #Frees | Requirements | Binding
------- | ------ | ------------ | -------
Any | 1 | ````a0````  | {}
(X a0) | 1 | ````a0````  | {"a0" --> a0}

### Supertypes of Z

Is type | #Frees | Requirements | Binding
------- | ------ | ------------ | -------
Any | 0 |  | {}
(X a0) | 1 | ````a0````  | {"a0" --> a0}
(Y Bool) | 0 |  | {"a0" --> pietervdvn:Data:Data.Bool.Bool}

### Supertypes of Bool

Is type | #Frees | Requirements | Binding
------- | ------ | ------------ | -------
Any | 0 |  | {}



> This page was automatically generated.
> 
> 
> Do not edit it, as regeneration will overwrite your changes.
> 
> 
> Back to [index](Index.md)