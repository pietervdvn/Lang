# Type overview

Type | Declared in | Kind | Docstring
---- | ----------- | ---- | ---------
**BIInt**  | Builtins | ````*````  | A int! This is a temporary representation, which will be replaced by a truly builtin one
**Eq**  | Category.Eq | ````*````  | The category which defines _equality_ ````==```` and _inequality_ ````!=````
**Associative** ````a````  | Category.Function | ````(* ~> *)````  | Functions for which the order of evaluation does not matter.
**Commutative** ````a````  ````b````  | Category.Function | ````(* ~> (* ~> *))````  | Functions for which the arguments can be swapped.
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
**Bool**  | Data.Bool | ````*````  | The ````Bool```` datatype represents truth values of logic.
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
Associative````a````  | (a -> a -> a)
Commutative````a````  ````b````  | (a -> a -> b)
Product | Monoid
Sum | Monoid
Collection | Mappable
Collection````a````  | Monoid
Dict````k````  ````v````  | Mappable, Monoid
Dict````k:Eq````  ````v````  | (Set k)
List | Mappable
List````a````  | Monoid, (Collection a)
More | Collection
Int | IntInf
Int' | Int, IntInf'
IntInf | BIInt
IntInf' | IntInf
Nat | NatInf
Nat' | Nat, NatInf'
NatInf | IntInf
NatInf' | NatInf
Flip````x````  ````b````  ````a````  | (x a b)





> This page was automatically generated on 2015-01-06 21:13:30 UTC (2015-01-06 22:13:30 CET)
> 
> 
> Do not edit it, as regeneration will overwrite your changes.
> 
> 
> Back to [index](Index.md)