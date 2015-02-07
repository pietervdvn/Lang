# Type overview

Type | Declared in | Kind | Docstring
---- | ----------- | ---- | ---------
**Any**  | Any | ````*````  | The supertype of every single type. Each type has ````Any```` implicitly as supertype.
**BIInt**  | Builtins | ````*````  | A int! This is a temporary representation, which will be replaced by a truly builtin one
**Eq**  | Category.Eq | ````*````  | The category which defines _equality_ ````==```` and _inequality_ ````!=````
**Associative** ````a````  | Category.Function | ````(* ~> *)````  | Functions for which the order of evaluation does not matter.
**Commutative** ````a````  ````b````  | Category.Function | ````(* ~> (* ~> *))````  | Functions for which the arguments can be swapped.
**Curry** ````a````  ````b````  | Category.Function | ````(* ~> (* ~> *))````  | Arbitrary functions from ````a```` to ````b````.
**Mappable** ````a````  | Category.Mappable | ````(* ~> *)````  | A _container_ on which ````map```` is defined. Also known as ````Functor```` in most other functional programming languages.
**Monoid**  | Category.Monoid | ````*````  | A ````Monoid```` is an type which has _neutral element_ and an _addition_.
**Product**  | Category.Monoid | ````*````  | An instance of ````Monoid````, with ````*```` defined as append and ````1```` as neutral element.
**Sum**  | Category.Monoid | ````*````  | An instance of ````Monoid````, with ````+```` defined as append and ````0```` as neutral element.
**Ord**  | Category.Ord | ````*````  | The category which defines _lower then_ and _higher then_.
**Collection** ````a````  | Collection.Collection | ````(* ~> *)````  | A ````Collection```` is a data structure which contains zero or more elements.
**Dict** ````k:Eq````  ````v````  | Collection.Dict | ````(* ~> (* ~> *))````  | A ````Collection```` which maps a key onto a value.
**List** ````a````  | Collection.List | ````(* ~> *)````  | A ````Collection```` which preserves order and allows duplicate elements.
**Maybe** ````a````  | Collection.Maybe | ````(* ~> *)````  | A collection which contains at most one value.
**More** ````a````  | Collection.More | ````(* ~> *)````  | A ````Collection```` which contains at least one element.
**Set** ````a:Eq````  | Collection.Set | ````(* ~> *)````  | A ````Collection```` without order and duplicates.
**Tuple** ````a````  ````b````  | Collection.Tuple | ````(* ~> (* ~> *))````  | The data structure representing tuples.
**Void**  | Collection.Void | ````*````  | Type representing _no data_.
**Bool**  | Data.Bool | ````*````  | The ````Bool```` datatype represents truth values of logic.
**Disjunct** ````n:Eq````  | Graph.Graph | ````(* ~> *)````  | Set of disjunct sets
**Graph** ````n:Eq,Ord````  ````a````  | Graph.Graph | ````(* ~> (* ~> *))````  | A generic, undirected graph (which might contain cycles).
**Weighted** ````graph:Graph````  ````n:Eq,Ord````  ````w:Eq,Monoid,Ord````  ````a````  | Graph.Graph | ````((* ~> (* ~> *)) ~> (* ~> (* ~> (* ~> *))))````  | A generic, undirected graph (which might contain cycles) and has a (symmetric) weight on each vertex.
**Int**  | Num.Nat | ````*````  | An _integer_
**Int'**  | Num.Nat | ````*````  | An _integer_ which is not zero
**IntInf**  | Num.Nat | ````*````  | An _integer_ or positive or negative _Infinity_ (````Inf````).
**IntInf'**  | Num.Nat | ````*````  | An _integer_ which is not zero, or positive or negative _Infinity_ (````Inf````).
**Nat**  | Num.Nat | ````*````  | A natural number
**Nat'**  | Num.Nat | ````*````  | A natural number, which is not zero.
**NatInf**  | Num.Nat | ````*````  | A natural number or _infinity_ (````Inf````).
**NatInf'**  | Num.Nat | ````*````  | A natural number (which is not zero) or _infinity_ (````Inf````).
**Flip** ````x````  ````b````  ````a````  | Type.Function | ````(* ~> (* ~> (* ~> *)))````  | The 'Flip' type takes a type function (````* -> * -> *````) and flips it arguments. E.g. Flip Dict v k = Dict k v

## Supertypes 

Type | Is subtype of
---- | -------------
BIInt | Any
Eq | Any
Associative````a````  | (a -> (a -> a))
Commutative````a````  ````b````  | (a -> (a -> b))
Curry````a````  ````b````  | (a -> b)
Mappable````a````  | Any
Monoid | Any
Product | Monoid
Sum | Monoid
Ord | Any
Collection | Mappable
Collection````a:Eq````  | Eq
Collection````a````  | Monoid
Dict````k````  | Mappable
Dict````k````  ````v````  | Monoid
Dict````k:Eq````  ````v````  | (Collection ((Tuple k) v))
List | Mappable, Collection
List````a````  | Monoid
List````a:((Tuple k) v)````  | ((Dict k) v)
Maybe````a````  | Any
More | Collection
More````a````  | Any
Set````a:Eq````  | (Collection a)
Tuple````a````  | Mappable
Tuple````a````  ````b````  | Any
Tuple````a:Eq````  ````b:Eq````  | Eq
Void | Any
Bool | Any
Disjunct````n:Eq````  | (Set (Set n))
Graph````n````  ````a````  | Any
Weighted````graph:Graph````  ````n:Ord, Eq````  ````w:Monoid, Ord, Eq````  | (graph n)
Int | IntInf
Int' | Int, IntInf'
IntInf | BIInt, Eq
IntInf' | IntInf
Nat | Int, NatInf
Nat' | Int', Nat
NatInf | IntInf
NatInf' | IntInf', NatInf
Flip````x````  ````b````  ````a````  | ((x a) b)



### Supertypes of Any

No supertypes

### Supertypes of BIInt

Is type | #Frees | Requirements
------- | ------ | ------------
Any | 0 | 

### Supertypes of Eq

Is type | #Frees | Requirements
------- | ------ | ------------
Any | 0 | 

### Supertypes of Associative

Is type | #Frees | Requirements
------- | ------ | ------------
(a -> (a -> a)) | 1 | ````a```` 

### Supertypes of Commutative

Is type | #Frees | Requirements
------- | ------ | ------------
(a -> (a -> b)) | 2 | ````a````  ````b```` 

### Supertypes of Curry

Is type | #Frees | Requirements
------- | ------ | ------------
(a -> b) | 2 | ````a````  ````b```` 

### Supertypes of Mappable

Is type | #Frees | Requirements
------- | ------ | ------------
Any | 1 | ````a```` 

### Supertypes of Monoid

Is type | #Frees | Requirements
------- | ------ | ------------
Any | 0 | 

### Supertypes of Product

Is type | #Frees | Requirements
------- | ------ | ------------
Monoid | 0 | 

### Supertypes of Sum

Is type | #Frees | Requirements
------- | ------ | ------------
Monoid | 0 | 

### Supertypes of Ord

Is type | #Frees | Requirements
------- | ------ | ------------
Any | 0 | 

### Supertypes of Collection

Is type | #Frees | Requirements
------- | ------ | ------------
Eq | 1 | ````a```` : {Eq}
Mappable | 0 | 
Monoid | 1 | ````a```` 

### Supertypes of Dict

Is type | #Frees | Requirements
------- | ------ | ------------
Mappable | 1 | ````k```` 
Monoid | 2 | ````k````  ````v```` 
(Collection ((Tuple k) v)) | 2 | ````k```` : {Eq} ````v```` 

### Supertypes of List

Is type | #Frees | Requirements
------- | ------ | ------------
Mappable | 0 | 
Monoid | 1 | ````a```` 
Collection | 0 | 
((Dict k) v) | 1 | ````a```` : {((Tuple k) v)}

### Supertypes of Maybe

Is type | #Frees | Requirements
------- | ------ | ------------
Any | 1 | ````a```` 

### Supertypes of More

Is type | #Frees | Requirements
------- | ------ | ------------
Any | 1 | ````a```` 
Collection | 0 | 

### Supertypes of Set

Is type | #Frees | Requirements
------- | ------ | ------------
(Collection a) | 1 | ````a```` : {Eq}

### Supertypes of Tuple

Is type | #Frees | Requirements
------- | ------ | ------------
Any | 2 | ````a````  ````b```` 
Eq | 2 | ````a```` : {Eq} ````b```` : {Eq}
Mappable | 1 | ````a```` 

### Supertypes of Void

Is type | #Frees | Requirements
------- | ------ | ------------
Any | 0 | 

### Supertypes of Bool

Is type | #Frees | Requirements
------- | ------ | ------------
Any | 0 | 

### Supertypes of Disjunct

Is type | #Frees | Requirements
------- | ------ | ------------
(Set (Set n)) | 1 | ````n```` : {Eq}

### Supertypes of Graph

Is type | #Frees | Requirements
------- | ------ | ------------
Any | 2 | ````n````  ````a```` 

### Supertypes of Weighted

Is type | #Frees | Requirements
------- | ------ | ------------
(Graph n) | 3 | ````graph```` : {Graph} ````n```` : {Eq, Ord} ````w```` : {Eq, Monoid, Ord}
(graph n) | 3 | ````graph```` : {Graph} ````n```` : {Eq, Ord} ````w```` : {Eq, Monoid, Ord}

### Supertypes of Int

Is type | #Frees | Requirements
------- | ------ | ------------
IntInf | 0 | 

### Supertypes of Int'

Is type | #Frees | Requirements
------- | ------ | ------------
Int | 0 | 
IntInf' | 0 | 

### Supertypes of IntInf

Is type | #Frees | Requirements
------- | ------ | ------------
BIInt | 0 | 
Eq | 0 | 

### Supertypes of IntInf'

Is type | #Frees | Requirements
------- | ------ | ------------
IntInf | 0 | 

### Supertypes of Nat

Is type | #Frees | Requirements
------- | ------ | ------------
Int | 0 | 
NatInf | 0 | 

### Supertypes of Nat'

Is type | #Frees | Requirements
------- | ------ | ------------
Int' | 0 | 
Nat | 0 | 

### Supertypes of NatInf

Is type | #Frees | Requirements
------- | ------ | ------------
IntInf | 0 | 

### Supertypes of NatInf'

Is type | #Frees | Requirements
------- | ------ | ------------
IntInf' | 0 | 
NatInf | 0 | 

### Supertypes of Flip

Is type | #Frees | Requirements
------- | ------ | ------------
((x a) b) | 3 | ````x````  ````b````  ````a```` 



> This page was automatically generated.
> 
> 
> Do not edit it, as regeneration will overwrite your changes.
> 
> 
> Back to [index](Index.md)