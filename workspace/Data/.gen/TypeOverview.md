# Type overview

Type | Declared in | Kind | Docstring
---- | ----------- | ---- | ---------
**BIInt**  | Builtins | * | A int! This is a temporary representations, which will be replaced by a truly builtin one
**Eq**  | Category.Eq | * | The category which defines equality ````==```` and inequality ````!=````
**Associative** ````a````  | Category.Function | (* ~> *) | Functions for which the order of evaluation does not matter.
**Commutative** ````a````  | Category.Function | (* ~> *) | Functions for which the arguments can be swapped.
**Mappable** ````a````  | Category.Mappable | (* ~> *) | A _container_ on which ````map```` is defined. Also known as ````Functor```` in most other functional programming languages.
**Monoid**  | Category.Monoid | * | A ````Monoid```` is an type which has _neutral element_ and an _addition_.
**Product**  | Category.Monoid | * | An instance of ````Monoid````, with ````*```` defined as append and ````1```` as neutral element.
**Sum**  | Category.Monoid | * | An instance of ````Monoid````, with ````+```` defined as append and ````0```` as neutral element.
**Ord**  | Category.Ord | * | The category which defines _lower then_ and _higher then_.
**Collection** ````a````  | Collection.Collection | (* ~> *) | A ````Collection```` is a data structure which contains zero or more elements.
**Dict** ````k:Eq````  ````v````  | Collection.Dict | (* ~> (* ~> *)) | A ````Collection```` which maps a key onto a value.
**List** ````a````  | Collection.List | (* ~> *) | A ````Collection```` which preserves order and allows duplicate elements.
**Maybe** ````a````  | Collection.Maybe | (* ~> *) | A collection which contains at most one value.
**Set** ````a:Eq````  | Collection.Set | (* ~> *) | A ````Collection```` without order and duplicates.
**Bool**  | Data.Bool | * | The ````Bool```` datatype represents truth values of logic.
**Int**  | Num.Nat | * | 
**Int'**  | Num.Nat | * | 
**IntInf**  | Num.Nat | * | 
**IntInf'**  | Num.Nat | * | 
**Nat**  | Num.Nat | * | 
**Nat'**  | Num.Nat | * | 
**NatInf**  | Num.Nat | * | 
**NatInf'**  | Num.Nat | * | 
**Flip** ````x````  ````b````  ````a````  | Type.Function | (* ~> (* ~> (* ~> *))) | 



> This page was automatically generated on 2015-01-04 21:36:53
> 
> 
> Do not edit it, as re-generation will overwrite your changes.