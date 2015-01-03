# Type overview

Type | Declared in | Kind | Requirements | Docstring
---- | ----------- | ---- | ------------ | ---------
**BIInt**  | pietervdvn:Data:Builtins | * |  | A int!
**Eq**  | pietervdvn:Data:Category.Eq | * |  | When a type is instance of ''Eq'' it means data of this type can be compared for equivalence.
**Associative** a | pietervdvn:Data:Category.Function | (* ~> *) |  | An associative function is a function for which you can swap the arguments, and get the same function.
**Commutative** a | pietervdvn:Data:Category.Function | (* ~> *) |  | A commutative function
**Mappable** a | pietervdvn:Data:Category.Mappable | (* ~> *) |  | A functor is a container which has a 'map'-function
**Monoid**  | pietervdvn:Data:Category.Monoid | * |  | A monoid is a type on which an _addition_ operation is defined together with a neutral element for this operator.
**Product**  | pietervdvn:Data:Category.Monoid | * |  | Product is an instance of monoid, with (+) defined as append
**Sum**  | pietervdvn:Data:Category.Monoid | * |  | Sum is an instance of monoid, with (+) defined as append
**Collection** a | pietervdvn:Data:Collection.Collection | (* ~> *) |  | A collection represents, well, a collection of data. Each set, list, map, bag, ... is instance of this class
**Dict** k v | pietervdvn:Data:Collection.Dict | (* ~> (* ~> *)) | k:Eq;  | A ''Dict'' (dictionary) is any type that maps a certain key onto a certain value.
**List** a | pietervdvn:Data:Collection.List | (* ~> *) |  | The class which defines a list
**Maybe** a | pietervdvn:Data:Collection.Maybe | (* ~> *) |  | The Maybe datatype definition.
**Set** a | pietervdvn:Data:Collection.Set | (* ~> *) | a:Eq | A set is a unordered collection, where each element is saved exactly once
**Bool**  | pietervdvn:Data:Data.Bool | * |  | The ''Bool'' datatype represents truth values of logic.
**Int**  | pietervdvn:Data:Num.Nat | * |  | 
**Int'**  | pietervdvn:Data:Num.Nat | * |  | 
**IntInf**  | pietervdvn:Data:Num.Nat | * |  | 
**IntInf'**  | pietervdvn:Data:Num.Nat | * |  | 
**Nat**  | pietervdvn:Data:Num.Nat | * |  | 
**Nat'**  | pietervdvn:Data:Num.Nat | * |  | 
**NatInf**  | pietervdvn:Data:Num.Nat | * |  | 
**NatInf'**  | pietervdvn:Data:Num.Nat | * |  | 
**Flip** x b a | pietervdvn:Data:Type.Function | (* ~> (* ~> (* ~> *))) | ; ;  | 



> This page was automatically generated on 2015-01-03 20:59:57
> 
> 
> Do not edit it, as re-generation will overwrite your changes.