> Back to [index](Index.md)

# Type overview

Type | Declared in | Kind | Docstring
---- | ----------- | ---- | ---------
**Any**  | Any | ````*````  | The supertype of every single type. Each type has ````Any```` implicitly as supertype.![Supertype.jpg](../doc/res/Supertype.jpg)
**BIInt**  | Builtins | ````*````  | A int! This is a temporary representation, which will be replaced by a truly builtin one
**Dummy**  | Prelude | ````*````  | public import Category.Eq
**Eq**  | Prelude | ````*````  | public import Category.Eq
**X** (````a0```` :````Eq```` ) | Prelude | ````(* ~> *)````  | public import Category.Eq
**Y** ````a0````  ````a1````  | Prelude | ````(* ~> (* ~> *))````  | public import Category.Eq
**Z** ````a0````  | Prelude | ````(* ~> *)````  | public import Category.Eq
**Eq**  | Category.Eq | ````*````  | The category which defines _equality_ ````==```` and _inequality_ ````!=````
**Associative** ````a0````  | Category.Function | ````(* ~> *)````  | Functions for which the order of evaluation does not matter.
**Commutative** ````a0````  ````a1````  | Category.Function | ````(* ~> (* ~> *))````  | Functions for which the arguments can be swapped.
**Curry** ````a0````  ````a1````  | Category.Function | ````(* ~> (* ~> *))````  | Arbitrary functions from ````a```` to ````b````.
**Mappable** ````a0````  | Category.Mappable | ````(* ~> *)````  | A _container_ on which ````map```` is defined. Also known as ````Functor```` in most other functional programming languages.
**Monoid**  | Category.Monoid | ````*````  | A ````Monoid```` is an type which has _neutral element_ and an _addition_.
**Product**  | Category.Monoid | ````*````  | An instance of ````Monoid````, with ````*```` defined as append and ````1```` as neutral element.
**Sum**  | Category.Monoid | ````*````  | An instance of ````Monoid````, with ````+```` defined as append and ````0```` as neutral element.
**Ord**  | Category.Ord | ````*````  | The category which defines _lower then_ and _higher then_.
**Collection** ````a0````  | Collection.Collection | ````(* ~> *)````  | A ````Collection```` is a data structure which contains zero or more elements.
**Dict** (````a0```` :````Eq```` ) ````a1````  | Collection.Dict | ````(* ~> (* ~> *))````  | A ````Collection```` which maps a key onto a value.
**List** ````a0````  | Collection.List | ````(* ~> *)````  | A ````Collection```` which preserves order and allows duplicate elements.
**Maybe** ````a0````  | Collection.Maybe | ````(* ~> *)````  | A collection which contains at most one value.
**Set** (````a0```` :````Eq```` ) | Collection.Set | ````(* ~> *)````  | A ````Collection```` without order and duplicates.
**Tuple** ````a0````  ````a1````  | Collection.Tuple | ````(* ~> (* ~> *))````  | The data structure representing tuples.
**Bool**  | Data.Bool | ````*````  | The ````Bool```` datatype represents truth values of logic.
**Int**  | Num.Nat | ````*````  | An _integer_
**Int'**  | Num.Nat | ````*````  | An _integer_ which is not zero
**IntInf**  | Num.Nat | ````*````  | An _integer_ or positive or negative _Infinity_ (````Inf````).
**IntInf'**  | Num.Nat | ````*````  | An _integer_ which is not zero, or positive or negative _Infinity_ (````Inf````).
**Nat**  | Num.Nat | ````*````  | A natural number
**Nat'**  | Num.Nat | ````*````  | A natural number, which is not zero.
**NatInf**  | Num.Nat | ````*````  | A natural number or _infinity_ (````Inf````).
**NatInf'**  | Num.Nat | ````*````  | A natural number (which is not zero) or _infinity_ (````Inf````).


## Supertypetables per type

### How to read a 'Supertypetable of T a0 a1'

````T a0 a1````  is the type given in **Is Type**  , if the **requirements**  on the free type variables are met. This table contains always the same number of frees, but a certain supertype can demand extra requirements.

The **Via**  column codes via what type this specific supertype was added. This means that, if ````List````  has supertype ````Collection```` , and ````Collection````  has supertype ````Mappable```` , that ````List```` has the suppertype````Mappable```` , which has been added via````Collection```` . _Native_ denotes that this supertype was added via the code.

A **Binding** might have happened on this supertype. E.g ````List (k,v)```` has the supertype````Dict k v```` .````Dict a0 a1```` has the supertype````Collection a0 a1```` .So if we want to add the supertype````Collection a0 a1```` to````List (k,v)```` , we have to substitute````a0 --> k, a1 --> v```` in the````Collection a0 a1```` example, if we want it to be correct.The **Orig Type** show this type before the substitution.

### Supertypes of BIInt 

Is type | Requirements | Via | Orig type | Origsuper -> actualsuper binding | Via -> Current binding
------- | ------------ | --- | --------- | -------------------------------- | ----------------------
.  |  | _Native_  | .  | {} | _Native_ 


### Supertypes of Dummy 

Is type | Requirements | Via | Orig type | Origsuper -> actualsuper binding | Via -> Current binding
------- | ------------ | --- | --------- | -------------------------------- | ----------------------
.  |  | _Native_  | .  | {} | _Native_ 


### Supertypes of Eq 

Is type | Requirements | Via | Orig type | Origsuper -> actualsuper binding | Via -> Current binding
------- | ------------ | --- | --------- | -------------------------------- | ----------------------
.  |  | _Native_  | .  | {} | _Native_ 


### Supertypes of X (````a0```` :````Eq```` )

Is type | Requirements | Via | Orig type | Origsuper -> actualsuper binding | Via -> Current binding
------- | ------------ | --- | --------- | -------------------------------- | ----------------------
.  | (````a0```` :````Eq```` ) | _Native_  | .  | {} | _Native_ 


### Supertypes of Y ````a0````  ````a1```` 

Is type | Requirements | Via | Orig type | Origsuper -> actualsuper binding | Via -> Current binding
------- | ------------ | --- | --------- | -------------------------------- | ----------------------
(X a1) | (````a1```` :````Eq```` ) | _Native_  | (X a1) | {"a0" --> a1} | _Native_ 


### Supertypes of Z ````a0```` 

Is type | Requirements | Via | Orig type | Origsuper -> actualsuper binding | Via -> Current binding
------- | ------------ | --- | --------- | -------------------------------- | ----------------------
.  |  | _Native_  | .  | {} | _Native_ 
((Y Dummy) a0) |  | _Native_  | ((Y Dummy) a0) | {"a0" --> pietervdvn:Data:Prelude.Dummy "a1" --> a0} | _Native_ 


### Supertypes of Eq 

Is type | Requirements | Via | Orig type | Origsuper -> actualsuper binding | Via -> Current binding
------- | ------------ | --- | --------- | -------------------------------- | ----------------------
.  |  | _Native_  | .  | {} | _Native_ 


### Supertypes of Associative ````a0```` 

Is type | Requirements | Via | Orig type | Origsuper -> actualsuper binding | Via -> Current binding
------- | ------------ | --- | --------- | -------------------------------- | ----------------------
(a0 -> (a0 -> a0)) | ````a0````  | _Native_  | (a0 -> (a0 -> a0)) | {} | _Native_ 


### Supertypes of Commutative ````a0````  ````a1```` 

Is type | Requirements | Via | Orig type | Origsuper -> actualsuper binding | Via -> Current binding
------- | ------------ | --- | --------- | -------------------------------- | ----------------------
(a0 -> (a0 -> a1)) | ````a0````  ````a1````  | _Native_  | (a0 -> (a0 -> a1)) | {} | _Native_ 


### Supertypes of Curry ````a0````  ````a1```` 

Is type | Requirements | Via | Orig type | Origsuper -> actualsuper binding | Via -> Current binding
------- | ------------ | --- | --------- | -------------------------------- | ----------------------
(a0 -> a1) | ````a0````  ````a1````  | _Native_  | (a0 -> a1) | {} | _Native_ 


### Supertypes of Mappable ````a0```` 

Is type | Requirements | Via | Orig type | Origsuper -> actualsuper binding | Via -> Current binding
------- | ------------ | --- | --------- | -------------------------------- | ----------------------
.  |  | _Native_  | .  | {} | _Native_ 


### Supertypes of Monoid 

Is type | Requirements | Via | Orig type | Origsuper -> actualsuper binding | Via -> Current binding
------- | ------------ | --- | --------- | -------------------------------- | ----------------------
.  |  | _Native_  | .  | {} | _Native_ 


### Supertypes of Product 

Is type | Requirements | Via | Orig type | Origsuper -> actualsuper binding | Via -> Current binding
------- | ------------ | --- | --------- | -------------------------------- | ----------------------
.  |  | _Native_  | .  | {} | _Native_ 
Monoid |  | _Native_  | Monoid | {} | _Native_ 


### Supertypes of Sum 

Is type | Requirements | Via | Orig type | Origsuper -> actualsuper binding | Via -> Current binding
------- | ------------ | --- | --------- | -------------------------------- | ----------------------
.  |  | _Native_  | .  | {} | _Native_ 
Monoid |  | _Native_  | Monoid | {} | _Native_ 


### Supertypes of Ord 

Is type | Requirements | Via | Orig type | Origsuper -> actualsuper binding | Via -> Current binding
------- | ------------ | --- | --------- | -------------------------------- | ----------------------
.  |  | _Native_  | .  | {} | _Native_ 


### Supertypes of Collection ````a0```` 

Is type | Requirements | Via | Orig type | Origsuper -> actualsuper binding | Via -> Current binding
------- | ------------ | --- | --------- | -------------------------------- | ----------------------
.  |  | _Native_  | .  | {} | _Native_ 
Eq | (````a0```` :````Eq```` ) | _Native_  | Eq | {} | _Native_ 
Monoid |  | _Native_  | Monoid | {} | _Native_ 
(Mappable a0) |  | _Native_  | (Mappable a0) | {"a0" --> a0} | _Native_ 


### Supertypes of Dict (````a0```` :````Eq```` ) ````a1```` 

Is type | Requirements | Via | Orig type | Origsuper -> actualsuper binding | Via -> Current binding
------- | ------------ | --- | --------- | -------------------------------- | ----------------------
Monoid |  | _Native_  | Monoid | {} | _Native_ 
(Mappable a1) |  | _Native_  | (Mappable a1) | {"a0" --> a1} | _Native_ 
(Collection (a0, a1)) | (````a0```` :````Eq```` ) | _Native_  | (Collection (a0, a1)) | {"a0" --> (a0, a1)} | _Native_ 


### Supertypes of List ````a0```` 

Is type | Requirements | Via | Orig type | Origsuper -> actualsuper binding | Via -> Current binding
------- | ------------ | --- | --------- | -------------------------------- | ----------------------
.  |  | _Native_  | .  | {} | _Native_ 
((Dict k1) v1) | (````k1```` :````Eq```` ) (````a0```` :````(k1, v1)```` ) | _Native_  | ((Dict k1) v1) | {"a0" --> k1 "a1" --> v1} | _Native_ 


### Supertypes of Maybe ````a0```` 

Is type | Requirements | Via | Orig type | Origsuper -> actualsuper binding | Via -> Current binding
------- | ------------ | --- | --------- | -------------------------------- | ----------------------
.  |  | _Native_  | .  | {} | _Native_ 


### Supertypes of Set (````a0```` :````Eq```` )

Is type | Requirements | Via | Orig type | Origsuper -> actualsuper binding | Via -> Current binding
------- | ------------ | --- | --------- | -------------------------------- | ----------------------
(Collection a0) | (````a0```` :````Eq```` ) | _Native_  | (Collection a0) | {"a0" --> a0} | _Native_ 


### Supertypes of Tuple ````a0````  ````a1```` 

Is type | Requirements | Via | Orig type | Origsuper -> actualsuper binding | Via -> Current binding
------- | ------------ | --- | --------- | -------------------------------- | ----------------------
.  |  | _Native_  | .  | {} | _Native_ 
Eq | (````a0```` :````Eq```` ) (````a1```` :````Eq```` ) | _Native_  | Eq | {} | _Native_ 
(Mappable a1) |  | _Native_  | (Mappable a1) | {"a0" --> a1} | _Native_ 


### Supertypes of Bool 

Is type | Requirements | Via | Orig type | Origsuper -> actualsuper binding | Via -> Current binding
------- | ------------ | --- | --------- | -------------------------------- | ----------------------
.  |  | _Native_  | .  | {} | _Native_ 


### Supertypes of Int 

Is type | Requirements | Via | Orig type | Origsuper -> actualsuper binding | Via -> Current binding
------- | ------------ | --- | --------- | -------------------------------- | ----------------------
IntInf |  | _Native_  | IntInf | {} | _Native_ 


### Supertypes of Int' 

Is type | Requirements | Via | Orig type | Origsuper -> actualsuper binding | Via -> Current binding
------- | ------------ | --- | --------- | -------------------------------- | ----------------------
Int |  | _Native_  | Int | {} | _Native_ 
IntInf' |  | _Native_  | IntInf' | {} | _Native_ 


### Supertypes of IntInf 

Is type | Requirements | Via | Orig type | Origsuper -> actualsuper binding | Via -> Current binding
------- | ------------ | --- | --------- | -------------------------------- | ----------------------
BIInt |  | _Native_  | BIInt | {} | _Native_ 
Eq |  | _Native_  | Eq | {} | _Native_ 


### Supertypes of IntInf' 

Is type | Requirements | Via | Orig type | Origsuper -> actualsuper binding | Via -> Current binding
------- | ------------ | --- | --------- | -------------------------------- | ----------------------
IntInf |  | _Native_  | IntInf | {} | _Native_ 


### Supertypes of Nat 

Is type | Requirements | Via | Orig type | Origsuper -> actualsuper binding | Via -> Current binding
------- | ------------ | --- | --------- | -------------------------------- | ----------------------
Int |  | _Native_  | Int | {} | _Native_ 
NatInf |  | _Native_  | NatInf | {} | _Native_ 


### Supertypes of Nat' 

Is type | Requirements | Via | Orig type | Origsuper -> actualsuper binding | Via -> Current binding
------- | ------------ | --- | --------- | -------------------------------- | ----------------------
Int' |  | _Native_  | Int' | {} | _Native_ 
Nat |  | _Native_  | Nat | {} | _Native_ 


### Supertypes of NatInf 

Is type | Requirements | Via | Orig type | Origsuper -> actualsuper binding | Via -> Current binding
------- | ------------ | --- | --------- | -------------------------------- | ----------------------
IntInf |  | _Native_  | IntInf | {} | _Native_ 


### Supertypes of NatInf' 

Is type | Requirements | Via | Orig type | Origsuper -> actualsuper binding | Via -> Current binding
------- | ------------ | --- | --------- | -------------------------------- | ----------------------
IntInf' |  | _Native_  | IntInf' | {} | _Native_ 
NatInf |  | _Native_  | NatInf | {} | _Native_ 




> This page was automatically generated.
> 
> 
> Do not edit it, as regeneration will overwrite your changes.
> 
> 
> Back to [index](Index.md)
> <div id="clicker">click</div>