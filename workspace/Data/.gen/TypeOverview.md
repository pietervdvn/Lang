# Type overview

Type | Declared in | Kind | Requirements | Docstring
---- | ----------- | ---- | ------------ | ---------
**BIInt**  | Builtins | * |  | A int!
**Eq**  | Category.Eq | * |  | When a type is instance of ````Eq```` it means data of this type can be compared for equivalence.
**Associative** a | Category.Function | (* ~> *) |  | An associative function is a function for which you can swap the arguments, and get the same function.
**Commutative** a | Category.Function | (* ~> *) |  | A commutative function
**Mappable** a | Category.Mappable | (* ~> *) |  | A functor is a container which has a 'map'-function
**Monoid**  | Category.Monoid | * |  | A monoid is a type on which an _addition_ operation is defined together with a neutral element for this operator.
**Product**  | Category.Monoid | * |  | Product is an instance of monoid, with (+) defined as append
**Sum**  | Category.Monoid | * |  | Sum is an instance of monoid, with (+) defined as append
**Collection** a | Collection.Collection | (* ~> *) |  | A collection represents, well, a collection of data. Each set, list, map, bag, ... is instance of this class
**Dict** k v | Collection.Dict | (* ~> (* ~> *)) | k:Eq | A ````Dict```` (dictionary) is any type that maps a certain key onto a certain value.
**List** a | Collection.List | (* ~> *) |  | The class which defines a list
**Maybe** a | Collection.Maybe | (* ~> *) |  | The Maybe datatype definition.
**Set** a | Collection.Set | (* ~> *) | a:Eq | A set is a unordered collection, where each element is saved exactly once
**Bool**  | Data.Bool | * |  | The ````Bool```` datatype represents truth values of logic.
**Int**  | Num.Nat | * |  | 
**Int'**  | Num.Nat | * |  | 
**IntInf**  | Num.Nat | * |  | 
**IntInf'**  | Num.Nat | * |  | 
**Nat**  | Num.Nat | * |  | 
**Nat'**  | Num.Nat | * |  | 
**NatInf**  | Num.Nat | * |  | 
**NatInf'**  | Num.Nat | * |  | 
**Flip** x b a | Type.Function | (* ~> (* ~> (* ~> *))) |  | 



> This page was automatically generated on 2015-01-03 21:32:51
> 
> 
> Do not edit it, as re-generation will overwrite your changes.