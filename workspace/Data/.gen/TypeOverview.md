# Type overview

Type | Declared in | Kind | Docstring
---- | ----------- | ---- | ---------
**Any**  | Any | ````*````  | The supertype of every single type. Each type has ````Any```` implicitly as supertype.
**Eq**  | Category.Eq | ````*````  | The category which defines _equality_ ````==```` and _inequality_ ````!=````
**Associative** ````a````  | Category.Function | ````(* ~> *)````  | Functions for which the order of evaluation does not matter.
**Commutative** ````a````  ````b````  | Category.Function | ````(* ~> (* ~> *))````  | Functions for which the arguments can be swapped.
**Curry** ````a````  ````b````  | Category.Function | ````(* ~> (* ~> *))````  | Arbitrary functions from ````a```` to ````b````.
**Mappable** ````b````  | Category.Mappable | ````(* ~> *)````  | A _container_ on which ````map```` is defined. Also known as ````Functor```` in most other functional programming languages.
**X** ````a:Eq````  | Category.Mappable | ````(* ~> *)````  | TODO remove
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
X````a0:Eq````  | Any
Y````a0````  | Any, (X a0)
Z | Any, (Y Bool)
Bool | Any



### Supertypes of Any

Is type | #Frees | Requirements | Binding | Via
------- | ------ | ------------ | ------- | ---
Any | 0 |  | {} | _Native_ 

### Supertypes of Eq

Is type | #Frees | Requirements | Binding | Via
------- | ------ | ------------ | ------- | ---
Any | 0 |  | {} | _Native_ 

### Supertypes of Associative

Is type | #Frees | Requirements | Binding | Via
------- | ------ | ------------ | ------- | ---
(a0 -> (a0 -> a0)) | 1 | ````a0````  | {} | _Native_ 

### Supertypes of Commutative

Is type | #Frees | Requirements | Binding | Via
------- | ------ | ------------ | ------- | ---
(a0 -> (a0 -> a1)) | 2 | ````a0````  ````a1````  | {} | _Native_ 

### Supertypes of Curry

Is type | #Frees | Requirements | Binding | Via
------- | ------ | ------------ | ------- | ---
(a0 -> a1) | 2 | ````a0````  ````a1````  | {} | _Native_ 

### Supertypes of Mappable

Is type | #Frees | Requirements | Binding | Via
------- | ------ | ------------ | ------- | ---
Any | 1 | ````a0````  | {} | _Native_ 

### Supertypes of X

Is type | #Frees | Requirements | Binding | Via
------- | ------ | ------------ | ------- | ---
Any | 1 | ````a0```` : {Eq} | {} | _Native_ 

### Supertypes of Y

Is type | #Frees | Requirements | Binding | Via
------- | ------ | ------------ | ------- | ---
Any | 1 | ````a0````  | {} | _Native_ 
(X a0) | 1 | ````a0````  | {"a0" --> a0} | _Native_ 

### Supertypes of Z

Is type | #Frees | Requirements | Binding | Via
------- | ------ | ------------ | ------- | ---
Any | 0 |  | {} | _Native_ 
(Y Bool) | 0 |  | {"a0" --> pietervdvn:Data:Data.Bool.Bool} | _Native_ 

### Supertypes of Bool

Is type | #Frees | Requirements | Binding | Via
------- | ------ | ------------ | ------- | ---
Any | 0 |  | {} | _Native_ 



> This page was automatically generated.
> 
> 
> Do not edit it, as regeneration will overwrite your changes.
> 
> 
> Back to [index](Index.md)