	> Back to [index](Index.md)

# Type overview

Type | Declared in | Kind | Docstring
---- | ----------- | ---- | ---------
**Any**  | Any | ````*````  | The supertype of every single type. Each type has ````Any```` implicitly as supertype.![Supertype.jpg](../doc/res/Supertype.jpg)
**BIInt**  | Builtins | ````*````  | A int! This is a temporary representation, which will be replaced by a truly builtin one
**Eq**  | Category.Eq | ````*````  | The category which defines _equality_ ````==```` and _inequality_ ````!=````
**Associative** ````a````  | Category.Function | ````(* ~> *)````  | Functions for which the order of evaluation does not matter.
**Commutative** ````a````  ````b````  | Category.Function | ````(* ~> (* ~> *))````  | Functions for which the arguments can be swapped.
**Curry** ````a````  ````b````  | Category.Function | ````(* ~> (* ~> *))````  | Arbitrary functions from ````a```` to ````b````.
**Mappable** ````b````  | Category.Mappable | ````(* ~> *)````  | A _container_ on which ````map```` is defined. Also known as ````Functor```` in most other functional programming languages.
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
**PrivateKey**  | Crypto.PubPrivAlgo | ````*````  | The type representing the _private_ key of a public-key encryption scheme
**PubPrivAlgo** ````a:PrivateKey````  ````b:PublicKey````  | Crypto.PubPrivAlgo | ````(* ~> (* ~> *))````  | The type representing a public-key encryption scheme, with fixed keys
**PublicKey**  | Crypto.PubPrivAlgo | ````*````  | The type representing the _public_ key of a public-key encryption scheme
**RSA**  | Crypto.PubPrivAlgo | ````*````  | The type representing a public-key encryption scheme, with fixed keys
**RSAPrivKey**  | Crypto.PubPrivAlgo | ````*````  | The type representing a public-key encryption scheme, with fixed keys
**RSAPubKey**  | Crypto.PubPrivAlgo | ````*````  | The type representing a public-key encryption scheme, with fixed keys
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


## Supertypetables per type

### How to read a 'Supertypetable of T a0 a1'

````T a0 a1```` is the type given in **Is Type** , if the **requirements** on the free type variables are met. This table contains always the same number of frees, but a certain supertype can demand extra requirements.

The **Via**  column codes via what type this specific supertype was added. This means that, if ````List````  has supertype ````Collection```` , and ````Collection````  has supertype ````Mappable```` , that ````List```` has the suppertype````Mappable```` , which has been added via````Collection```` . _Native_ denotes that this supertype was added via the code.

A **Binding** might have happened on this supertype. E.g ````List (k,v)```` has the supertype````Dict k v```` .````Dict a0 a1```` has the supertype````Collection a0 a1```` .So if we want to add the supertype````Collection a0 a1```` to````List (k,v)```` , we have to substitute````a0 --> k, a1 --> v```` in the````Collection a0 a1```` example, if we want it to be correct.The **Orig Type** show this type before the substitution.

### Supertypes of BIInt

Is type | Requirements | Via | Orig type | Orig type reqs | Binding
------- | ------------ | --- | --------- | -------------- | -------
.  |  | _Native_  | .  | Todo | {}


### Supertypes of Eq

Is type | Requirements | Via | Orig type | Orig type reqs | Binding
------- | ------------ | --- | --------- | -------------- | -------
.  |  | _Native_  | .  | Todo | {}


### Supertypes of Associative a0

Is type | Requirements | Via | Orig type | Orig type reqs | Binding
------- | ------------ | --- | --------- | -------------- | -------
(a0 -> (a0 -> a0)) | ````a0````  | _Native_  | (a0 -> (a0 -> a0)) | Todo | {}


### Supertypes of Commutative a0 a1

Is type | Requirements | Via | Orig type | Orig type reqs | Binding
------- | ------------ | --- | --------- | -------------- | -------
(a0 -> (a0 -> a1)) | ````a0````  ````a1````  | _Native_  | (a0 -> (a0 -> a1)) | Todo | {}


### Supertypes of Curry a0 a1

Is type | Requirements | Via | Orig type | Orig type reqs | Binding
------- | ------------ | --- | --------- | -------------- | -------
(a0 -> a1) | ````a0````  ````a1````  | _Native_  | (a0 -> a1) | Todo | {}


### Supertypes of Mappable a0

Is type | Requirements | Via | Orig type | Orig type reqs | Binding
------- | ------------ | --- | --------- | -------------- | -------
.  | ````a0````  | _Native_  | .  | Todo | {}


### Supertypes of Monoid

Is type | Requirements | Via | Orig type | Orig type reqs | Binding
------- | ------------ | --- | --------- | -------------- | -------
.  |  | _Native_  | .  | Todo | {}


### Supertypes of Product

Is type | Requirements | Via | Orig type | Orig type reqs | Binding
------- | ------------ | --- | --------- | -------------- | -------
.  |  | _Native_  | .  | Todo | {}
Monoid |  | _Native_  | Monoid | Todo | {}


### Supertypes of Sum

Is type | Requirements | Via | Orig type | Orig type reqs | Binding
------- | ------------ | --- | --------- | -------------- | -------
.  |  | _Native_  | .  | Todo | {}
Monoid |  | _Native_  | Monoid | Todo | {}


### Supertypes of Ord

Is type | Requirements | Via | Orig type | Orig type reqs | Binding
------- | ------------ | --- | --------- | -------------- | -------
.  |  | _Native_  | .  | Todo | {}


### Supertypes of Collection a0

Is type | Requirements | Via | Orig type | Orig type reqs | Binding
------- | ------------ | --- | --------- | -------------- | -------
.  | ````a0````  | _Native_  | .  | Todo | {}
Eq | ````a0```` : {````Eq```` } | _Native_  | Eq | Todo | {}
Monoid | ````a0````  | _Native_  | Monoid | Todo | {}
(Mappable a0) | ````a0````  | _Native_  | (Mappable a0) | Todo | {"a0" --> a0}


### Supertypes of Dict a0 a1

Is type | Requirements | Via | Orig type | Orig type reqs | Binding
------- | ------------ | --- | --------- | -------------- | -------
.  | ````a0````  ````a1````  | Monoid | .  | Todo | {}
Eq | ````a0```` : {````Eq```` } ````a1````  | (Collection (a0, a1)) | Eq | Todo | {"a0" --> (a0, a1)}
Monoid | ````a0````  ````a1````  | _Native_  | Monoid | Todo | {}
(Mappable a1) | ````a0````  ````a1````  | _Native_  | (Mappable a1) | Todo | {"a0" --> a1}
(Mappable (a0, a1)) | ````a0```` : {````Eq```` } ````a1````  | (Collection (a0, a1)) | (Mappable a0) | Todo | {"a0" --> (a0, a1)}
(Collection (a0, a1)) | ````a0```` : {````Eq```` } ````a1````  | _Native_  | (Collection (a0, a1)) | Todo | {"a0" --> (a0, a1)}


### Supertypes of List a0

Is type | Requirements | Via | Orig type | Orig type reqs | Binding
------- | ------------ | --- | --------- | -------------- | -------
.  | ````a0````  | Monoid | .  | Todo | {}
Eq | ````a0````  | (Collection a0) | Eq | Todo | {"a0" --> a0}
Monoid | ````a0````  | _Native_  | Monoid | Todo | {}
(Mappable a0) | ````a0````  | _Native_  | (Mappable a0) | Todo | {"a0" --> a0}
(Mappable v1) | ````a0```` : {````(k1, v1)```` } | ((Dict k1) v1) | (Mappable a1) | Todo | {"a0" --> k1 "a1" --> v1}
(Mappable [v1]) | ````a0```` : {````(k1, v1)```` } | ((Dict k1) [v1]) | (Mappable a1) | Todo | {"a0" --> k1 "a1" --> [v1]}
(Mappable (k1, v1)) | ````a0```` : {````(k1, v1)```` } | ((Dict k1) v1) | (Mappable (a0, a1)) | Todo | {"a0" --> k1 "a1" --> v1}
(Mappable (k1, [v1])) | ````a0```` : {````(k1, v1)```` } | ((Dict k1) [v1]) | (Mappable (a0, a1)) | Todo | {"a0" --> k1 "a1" --> [v1]}
(Collection a0) | ````a0````  | _Native_  | (Collection a0) | Todo | {"a0" --> a0}
(Collection (k1, v1)) | ````a0```` : {````(k1, v1)```` } | ((Dict k1) v1) | (Collection (a0, a1)) | Todo | {"a0" --> k1 "a1" --> v1}
(Collection (k1, [v1])) | ````a0```` : {````(k1, v1)```` } | ((Dict k1) [v1]) | (Collection (a0, a1)) | Todo | {"a0" --> k1 "a1" --> [v1]}
{a0} | ````a0````  | _Native_  | {a0} | Todo | {"a0" --> a0}
((Dict k1) v1) | ````a0```` : {````(k1, v1)```` } | _Native_  | ((Dict k1) v1) | Todo | {"a0" --> k1 "a1" --> v1}
((Dict k1) [v1]) | ````a0```` : {````(k1, v1)```` } | _Native_  | ((Dict k1) [v1]) | Todo | {"a0" --> k1 "a1" --> [v1]}


### Supertypes of Maybe a0

Is type | Requirements | Via | Orig type | Orig type reqs | Binding
------- | ------------ | --- | --------- | -------------- | -------
.  | ````a0````  | _Native_  | .  | Todo | {}


### Supertypes of More a0

Is type | Requirements | Via | Orig type | Orig type reqs | Binding
------- | ------------ | --- | --------- | -------------- | -------
.  | ````a0````  | _Native_  | .  | Todo | {}
Eq | ````a0````  | (Collection a0) | Eq | Todo | {"a0" --> a0}
Monoid | ````a0````  | (Collection a0) | Monoid | Todo | {"a0" --> a0}
(Mappable a0) | ````a0````  | (Collection a0) | (Mappable a0) | Todo | {"a0" --> a0}
(Collection a0) | ````a0````  | _Native_  | (Collection a0) | Todo | {"a0" --> a0}


### Supertypes of Set a0

Is type | Requirements | Via | Orig type | Orig type reqs | Binding
------- | ------------ | --- | --------- | -------------- | -------
.  | ````a0```` : {````Eq```` } | (Collection a0) | .  | Todo | {"a0" --> a0}
Eq | ````a0```` : {````Eq```` } | (Collection a0) | Eq | Todo | {"a0" --> a0}
Monoid | ````a0```` : {````Eq```` } | (Collection a0) | Monoid | Todo | {"a0" --> a0}
(Mappable a0) | ````a0```` : {````Eq```` } | (Collection a0) | (Mappable a0) | Todo | {"a0" --> a0}
(Collection a0) | ````a0```` : {````Eq```` } | _Native_  | (Collection a0) | Todo | {"a0" --> a0}


### Supertypes of Tuple a0 a1

Is type | Requirements | Via | Orig type | Orig type reqs | Binding
------- | ------------ | --- | --------- | -------------- | -------
.  | ````a0````  ````a1````  | _Native_  | .  | Todo | {}
Eq | ````a0```` : {````Eq```` } ````a1```` : {````Eq```` } | _Native_  | Eq | Todo | {}
(Mappable a1) | ````a0````  ````a1````  | _Native_  | (Mappable a1) | Todo | {"a0" --> a1}


### Supertypes of Void

Is type | Requirements | Via | Orig type | Orig type reqs | Binding
------- | ------------ | --- | --------- | -------------- | -------
.  |  | _Native_  | .  | Todo | {}


### Supertypes of PrivateKey

Is type | Requirements | Via | Orig type | Orig type reqs | Binding
------- | ------------ | --- | --------- | -------------- | -------
.  |  | _Native_  | .  | Todo | {}


### Supertypes of PubPrivAlgo a0 a1

Is type | Requirements | Via | Orig type | Orig type reqs | Binding
------- | ------------ | --- | --------- | -------------- | -------
.  | ````a0```` : {````PrivateKey```` } ````a1```` : {````PublicKey```` } | _Native_  | .  | Todo | {}


### Supertypes of PublicKey

Is type | Requirements | Via | Orig type | Orig type reqs | Binding
------- | ------------ | --- | --------- | -------------- | -------
.  |  | _Native_  | .  | Todo | {}


### Supertypes of RSA

Is type | Requirements | Via | Orig type | Orig type reqs | Binding
------- | ------------ | --- | --------- | -------------- | -------
.  |  | ((PubPrivAlgo RSAPrivKey) RSAPubKey) | .  | Todo | {"a0" --> pietervdvn:Data:Crypto.PubPrivAlgo.RSAPrivKey "a1" --> pietervdvn:Data:Crypto.PubPrivAlgo.RSAPubKey}
((PubPrivAlgo RSAPrivKey) RSAPubKey) |  | _Native_  | ((PubPrivAlgo RSAPrivKey) RSAPubKey) | Todo | {"a0" --> pietervdvn:Data:Crypto.PubPrivAlgo.RSAPrivKey "a1" --> pietervdvn:Data:Crypto.PubPrivAlgo.RSAPubKey}


### Supertypes of RSAPrivKey

Is type | Requirements | Via | Orig type | Orig type reqs | Binding
------- | ------------ | --- | --------- | -------------- | -------
.  |  | _Native_  | .  | Todo | {}
PrivateKey |  | _Native_  | PrivateKey | Todo | {}


### Supertypes of RSAPubKey

Is type | Requirements | Via | Orig type | Orig type reqs | Binding
------- | ------------ | --- | --------- | -------------- | -------
.  |  | _Native_  | .  | Todo | {}
PublicKey |  | _Native_  | PublicKey | Todo | {}


### Supertypes of Bool

Is type | Requirements | Via | Orig type | Orig type reqs | Binding
------- | ------------ | --- | --------- | -------------- | -------
.  |  | _Native_  | .  | Todo | {}


### Supertypes of Disjunct a0

Is type | Requirements | Via | Orig type | Orig type reqs | Binding
------- | ------------ | --- | --------- | -------------- | -------
.  | ````a0```` : {````Eq```` } | {Set a0} | .  | Todo | {"a0" --> {a0}}
Eq | ````a0```` : {````Eq```` } | {Set a0} | Eq | Todo | {"a0" --> {a0}}
Monoid | ````a0```` : {````Eq```` } | {Set a0} | Monoid | Todo | {"a0" --> {a0}}
(Mappable {a0}) | ````a0```` : {````Eq```` } | {Set a0} | (Mappable a0) | Todo | {"a0" --> {a0}}
(Collection {a0}) | ````a0```` : {````Eq```` } | {Set a0} | (Collection a0) | Todo | {"a0" --> {a0}}
{Set a0} | ````a0```` : {````Eq```` } | _Native_  | {Set a0} | Todo | {"a0" --> {a0}}


### Supertypes of Graph a0 a1

Is type | Requirements | Via | Orig type | Orig type reqs | Binding
------- | ------------ | --- | --------- | -------------- | -------
.  | ````a0```` : {````Eq```` , ````Ord```` } ````a1````  | _Native_  | .  | Todo | {}


### Supertypes of Weighted a0 a1 a2 a3

Is type | Requirements | Via | Orig type | Orig type reqs | Binding
------- | ------------ | --- | --------- | -------------- | -------
.  | ````a0```` : {````Graph```` } ````a1```` : {````Eq```` , ````Ord```` } ````a2```` : {````Eq```` , ````Monoid```` , ````Ord```` } ````a3````  | ((Graph a1) a3) | .  | Todo | {"a0" --> a1 "a1" --> a3}
((Graph a1) a3) | ````a0```` : {````Graph```` } ````a1```` : {````Eq```` , ````Ord```` } ````a2```` : {````Eq```` , ````Monoid```` , ````Ord```` } ````a3````  | _Native_  | ((a0 a1) a3) | Todo | {"a0" --> a1 "a1" --> a3}
((a0 a1) a3) | ````a0```` : {````Graph```` } ````a1```` : {````Eq```` , ````Ord```` } ````a2```` : {````Eq```` , ````Monoid```` , ````Ord```` } ````a3````  | _Native_  | ((a0 a1) a3) | Todo | {"a0" --> a1 "a1" --> a3}


### Supertypes of Int

Is type | Requirements | Via | Orig type | Orig type reqs | Binding
------- | ------------ | --- | --------- | -------------- | -------
.  |  | IntInf | .  | Todo | {}
BIInt |  | IntInf | BIInt | Todo | {}
Eq |  | IntInf | Eq | Todo | {}
IntInf |  | _Native_  | IntInf | Todo | {}


### Supertypes of Int'

Is type | Requirements | Via | Orig type | Orig type reqs | Binding
------- | ------------ | --- | --------- | -------------- | -------
.  |  | Int | .  | Todo | {}
BIInt |  | Int | BIInt | Todo | {}
Eq |  | Int | Eq | Todo | {}
Int |  | _Native_  | Int | Todo | {}
IntInf |  | Int | IntInf | Todo | {}
IntInf' |  | _Native_  | IntInf' | Todo | {}


### Supertypes of IntInf

Is type | Requirements | Via | Orig type | Orig type reqs | Binding
------- | ------------ | --- | --------- | -------------- | -------
.  |  | BIInt | .  | Todo | {}
BIInt |  | _Native_  | BIInt | Todo | {}
Eq |  | _Native_  | Eq | Todo | {}


### Supertypes of IntInf'

Is type | Requirements | Via | Orig type | Orig type reqs | Binding
------- | ------------ | --- | --------- | -------------- | -------
.  |  | IntInf | .  | Todo | {}
BIInt |  | IntInf | BIInt | Todo | {}
Eq |  | IntInf | Eq | Todo | {}
IntInf |  | _Native_  | IntInf | Todo | {}


### Supertypes of Nat

Is type | Requirements | Via | Orig type | Orig type reqs | Binding
------- | ------------ | --- | --------- | -------------- | -------
.  |  | Int | .  | Todo | {}
BIInt |  | Int | BIInt | Todo | {}
Eq |  | Int | Eq | Todo | {}
Int |  | _Native_  | Int | Todo | {}
IntInf |  | Int | IntInf | Todo | {}
NatInf |  | _Native_  | NatInf | Todo | {}


### Supertypes of Nat'

Is type | Requirements | Via | Orig type | Orig type reqs | Binding
------- | ------------ | --- | --------- | -------------- | -------
.  |  | Int' | .  | Todo | {}
BIInt |  | Int' | BIInt | Todo | {}
Eq |  | Int' | Eq | Todo | {}
Int |  | Int' | Int | Todo | {}
Int' |  | _Native_  | Int' | Todo | {}
IntInf |  | Int' | IntInf | Todo | {}
IntInf' |  | Int' | IntInf' | Todo | {}
Nat |  | _Native_  | Nat | Todo | {}
NatInf |  | Nat | NatInf | Todo | {}


### Supertypes of NatInf

Is type | Requirements | Via | Orig type | Orig type reqs | Binding
------- | ------------ | --- | --------- | -------------- | -------
.  |  | IntInf | .  | Todo | {}
BIInt |  | IntInf | BIInt | Todo | {}
Eq |  | IntInf | Eq | Todo | {}
IntInf |  | _Native_  | IntInf | Todo | {}


### Supertypes of NatInf'

Is type | Requirements | Via | Orig type | Orig type reqs | Binding
------- | ------------ | --- | --------- | -------------- | -------
.  |  | IntInf' | .  | Todo | {}
BIInt |  | IntInf' | BIInt | Todo | {}
Eq |  | IntInf' | Eq | Todo | {}
IntInf |  | IntInf' | IntInf | Todo | {}
IntInf' |  | _Native_  | IntInf' | Todo | {}
NatInf |  | _Native_  | NatInf | Todo | {}


### Supertypes of Flip a0 a1 a2

Is type | Requirements | Via | Orig type | Orig type reqs | Binding
------- | ------------ | --- | --------- | -------------- | -------
((a0 a2) a1) | ````a0````  ````a1````  ````a2````  | _Native_  | ((a0 a2) a1) | Todo | {"a0" --> a2 "a1" --> a1}




> This page was automatically generated.
>
>
> Do not edit it, as regeneration will overwrite your changes.
>
>
> Back to [index](Index.md)
