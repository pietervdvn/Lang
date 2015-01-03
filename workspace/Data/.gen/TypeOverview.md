# Type overview

Type | Declared in | Kind | Requirements | Docstring
---- | ----------- | ---- | ------------ | ---------
BIInt | pietervdvn:Data:Builtins | * | **TODO**  | A int!
Eq | pietervdvn:Data:Category.Eq | * | **TODO**  | When a type is instance of ''Eq'' it means data of this type can be compared for equivalence.
Associative | pietervdvn:Data:Category.Function | (* ~> *) | **TODO**  | 
An associative function is a function for which you can swap the arguments, and get the same function.

Commutative | pietervdvn:Data:Category.Function | (* ~> *) | **TODO**  | 
A commutative function

Mappable | pietervdvn:Data:Category.Mappable | (* ~> *) | **TODO**  | A functor is a container which has a 'map'-function
Monoid | pietervdvn:Data:Category.Monoid | * | **TODO**  | A monoid is a type on which an _addition_ operation is defined together with a neutral element for this operator.
Product | pietervdvn:Data:Category.Monoid | * | **TODO**  | Product is an instance of monoid, with (+) defined as append
Sum | pietervdvn:Data:Category.Monoid | * | **TODO**  | Sum is an instance of monoid, with (+) defined as append
Collection | pietervdvn:Data:Collection.Collection | (* ~> *) | **TODO**  | A collection represents, well, a collection of data. Each set, list, map, bag, ... is instance of this class
Dict | pietervdvn:Data:Collection.Dict | (* ~> (* ~> *)) | **TODO**  | A ''Dict'' (dictionary) is any type that maps a certain key onto a certain value.
List | pietervdvn:Data:Collection.List | (* ~> *) | **TODO**  | The class which defines a list
Maybe | pietervdvn:Data:Collection.Maybe | (* ~> *) | **TODO**  | The Maybe datatype definition.
Set | pietervdvn:Data:Collection.Set | (* ~> *) | **TODO**  | A set is a unordered collection, where each element is saved exactly once
Bool | pietervdvn:Data:Data.Bool | * | **TODO**  | The ''Bool'' datatype represents truth values of logic.
Int | pietervdvn:Data:Num.Nat | * | **TODO**  | 
Int' | pietervdvn:Data:Num.Nat | * | **TODO**  | 
IntInf | pietervdvn:Data:Num.Nat | * | **TODO**  | 
IntInf' | pietervdvn:Data:Num.Nat | * | **TODO**  | 
Nat | pietervdvn:Data:Num.Nat | * | **TODO**  | 
Nat' | pietervdvn:Data:Num.Nat | * | **TODO**  | 
NatInf | pietervdvn:Data:Num.Nat | * | **TODO**  | 
NatInf' | pietervdvn:Data:Num.Nat | * | **TODO**  | 
Flip | pietervdvn:Data:Type.Function | (* ~> (* ~> (* ~> *))) | **TODO**  | 



> This page was automatically generated on 2015-01-03 19:16:08
> 
> 
> Do not edit it, as re-generation will overwrite your changes.