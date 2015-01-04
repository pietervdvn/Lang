# Type overview

Type | Declared in | Kind | Requirements | Docstring
---- | ----------- | ---- | ------------ | ---------
**BIInt**  | Builtins | * |  | A int!
**T**  | Prelude | * |  | 
**Eq**  | Category.Eq | * |  | The category which declares equality ````==```` and inequality ````!=````
**Associative** a | Category.Function | (* ~> *) |  | Functions for which the order of evaluation does not matter.
**Commutative** a | Category.Function | (* ~> *) |  | Functions for which the arguments can be swapped.
**Mappable** a | Category.Mappable | (* ~> *) |  | A ````container```` on which ````map```` is defined. Aka ````functor````
**Monoid**  | Category.Monoid | * |  | A monoid is an type on which a neutral (identity) element and an _addition_ operator is defined.
**Product**  | Category.Monoid | * |  | Product is an instance of monoid, with (*) defined as append
**Sum**  | Category.Monoid | * |  | Sum is an instance of monoid, with (+) defined as append
**Collection** a | Collection.Collection | (* ~> *) |  | A ````Collection```` is a data structure which contains zero or more elements.
**Dict** k v | Collection.Dict | (* ~> (* ~> *)) | k:Eq | A ````Collection```` which maps a key onto a value.
**List** a | Collection.List | (* ~> *) |  | A ````Collection```` which preserves order and allows duplicate elements.
**Maybe** a | Collection.Maybe | (* ~> *) |  | A collection which contains at most one value.
**Set** a | Collection.Set | (* ~> *) | a:Eq | A ````Collection```` without order and duplicates.
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



> This page was automatically generated on 2015-01-04 17:35:48
> 
> 
> Do not edit it, as re-generation will overwrite your changes.