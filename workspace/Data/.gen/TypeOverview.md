> Back to [index](Index.md)

# Type overview

Type | Declared in | Kind | Docstring
---- | ----------- | ---- | ---------
**Any**  | Any | ````*````  | The supertype of every single type. Each type has ````Any```` implicitly as supertype.![Supertype.jpg](../doc/res/Supertype.jpg)
**BIInt**  | Builtins | ````*````  | A int! This is a temporary representation, which will be replaced by a truly builtin one
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
**More** ````a0````  | Collection.More | ````(* ~> *)````  | A ````Collection```` which contains at least one element.
**Set** (````a0```` :````Eq```` ) | Collection.Set | ````(* ~> *)````  | A ````Collection```` without order and duplicates.
**Tuple** ````a0````  ````a1````  | Collection.Tuple | ````(* ~> (* ~> *))````  | The data structure representing tuples.
**Void**  | Collection.Void | ````*````  | Type representing _no data_.
**PrivateKey**  | Crypto.PubPrivAlgo | ````*````  | The type representing the _private_ key of a public-key encryption scheme
**PubPrivAlgo** (````a0```` :````PrivateKey```` ) (````a1```` :````PublicKey```` ) | Crypto.PubPrivAlgo | ````(* ~> (* ~> *))````  | The type representing a public-key encryption scheme, with fixed keys
**PublicKey**  | Crypto.PubPrivAlgo | ````*````  | The type representing the _public_ key of a public-key encryption scheme
**RSA**  | Crypto.PubPrivAlgo | ````*````  | The type representing a public-key encryption scheme, with fixed keys
**RSAPrivKey**  | Crypto.PubPrivAlgo | ````*````  | The type representing a public-key encryption scheme, with fixed keys
**RSAPubKey**  | Crypto.PubPrivAlgo | ````*````  | The type representing a public-key encryption scheme, with fixed keys
**Bool**  | Data.Bool | ````*````  | The ````Bool```` datatype represents truth values of logic.
**Disjunct** (````a0```` :````Eq```` ) | Graph.Graph | ````(* ~> *)````  | Set of disjunct sets
**Graph** (````a0```` :````Eq```` , ````Ord```` ) ````a1````  | Graph.Graph | ````(* ~> (* ~> *))````  | A generic, undirected graph (which might contain cycles).
**Weighted** (````a0```` :````Graph```` ) (````a1```` :````Eq```` , ````Ord```` ) (````a2```` :````Eq```` , ````Monoid```` , ````Ord```` ) ````a3````  | Graph.Graph | ````((* ~> (* ~> *)) ~> (* ~> (* ~> (* ~> *))))````  | A generic, undirected graph (which might contain cycles) and has a (symmetric) weight on each vertex.
**Int**  | Num.Nat | ````*````  | An _integer_
**Int'**  | Num.Nat | ````*````  | An _integer_ which is not zero
**IntInf**  | Num.Nat | ````*````  | An _integer_ or positive or negative _Infinity_ (````Inf````).
**IntInf'**  | Num.Nat | ````*````  | An _integer_ which is not zero, or positive or negative _Infinity_ (````Inf````).
**Nat**  | Num.Nat | ````*````  | A natural number
**Nat'**  | Num.Nat | ````*````  | A natural number, which is not zero.
**NatInf**  | Num.Nat | ````*````  | A natural number or _infinity_ (````Inf````).
**NatInf'**  | Num.Nat | ````*````  | A natural number (which is not zero) or _infinity_ (````Inf````).
**Flip** ````a0````  ````a1````  ````a2````  | Type.Function | ````(* ~> (* ~> (* ~> *)))````  | The 'Flip' type takes a type function (````* -> * -> *````) and flips it arguments. E.g. Flip Dict v k = Dict k v


## Supertypetables per type

### How to read a 'Supertypetable of T a0 a1'

````T a0 a1````  is the type given in **Is Type**  , if the **requirements**  on the free type variables are met. This table contains always the same number of frees, but a certain supertype can demand extra requirements.

The **Via**  column codes via what type this specific supertype was added. This means that, if ````List````  has supertype ````Collection```` , and ````Collection````  has supertype ````Mappable```` , that ````List```` has the suppertype````Mappable```` , which has been added via````Collection```` . _Native_ denotes that this supertype was added via the code.

A **Binding** might have happened on this supertype. E.g ````List (k,v)```` has the supertype````Dict k v```` .````Dict a0 a1```` has the supertype````Collection a0 a1```` .So if we want to add the supertype````Collection a0 a1```` to````List (k,v)```` , we have to substitute````a0 --> k, a1 --> v```` in the````Collection a0 a1```` example, if we want it to be correct.The **Orig Type** show this type before the substitution.

### Supertypes of BIInt 

Is type | Requirements | Via | Orig type | Binding
------- | ------------ | --- | --------- | -------
.  |  | _Native_  | .  | {}


### Supertypes of Eq 

Is type | Requirements | Via | Orig type | Binding
------- | ------------ | --- | --------- | -------
.  |  | _Native_  | .  | {}


### Supertypes of Associative a0

Is type | Requirements | Via | Orig type | Binding
------- | ------------ | --- | --------- | -------
(a0 -> (a0 -> a0)) | ````a0````  | _Native_  | (a0 -> (a0 -> a0)) | {}


### Supertypes of Commutative a0 a1

Is type | Requirements | Via | Orig type | Binding
------- | ------------ | --- | --------- | -------
(a0 -> (a0 -> a1)) | ````a0````  ````a1````  | _Native_  | (a0 -> (a0 -> a1)) | {}


### Supertypes of Curry a0 a1

Is type | Requirements | Via | Orig type | Binding
------- | ------------ | --- | --------- | -------
(a0 -> a1) | ````a0````  ````a1````  | _Native_  | (a0 -> a1) | {}


### Supertypes of Mappable a0

Is type | Requirements | Via | Orig type | Binding
------- | ------------ | --- | --------- | -------
.  | ````a0````  | _Native_  | .  | {}


### Supertypes of Monoid 

Is type | Requirements | Via | Orig type | Binding
------- | ------------ | --- | --------- | -------
.  |  | _Native_  | .  | {}


### Supertypes of Product 

Is type | Requirements | Via | Orig type | Binding
------- | ------------ | --- | --------- | -------
.  |  | _Native_  | .  | {}
Monoid |  | _Native_  | Monoid | {}


### Supertypes of Sum 

Is type | Requirements | Via | Orig type | Binding
------- | ------------ | --- | --------- | -------
.  |  | _Native_  | .  | {}
Monoid |  | _Native_  | Monoid | {}


### Supertypes of Ord 

Is type | Requirements | Via | Orig type | Binding
------- | ------------ | --- | --------- | -------
.  |  | _Native_  | .  | {}


### Supertypes of Collection a0

Is type | Requirements | Via | Orig type | Binding
------- | ------------ | --- | --------- | -------
.  | ````a0````  | _Native_  | .  | {}
Eq | ````a0```` : {````Eq```` } | _Native_  | Eq | {}
Monoid | ````a0````  | _Native_  | Monoid | {}
(Mappable a0) | ````a0````  | _Native_  | (Mappable a0) | {"a0" --> a0}


### Supertypes of Dict a0 a1

Is type | Requirements | Via | Orig type | Binding
------- | ------------ | --- | --------- | -------
.  | ````a0````  ````a1````  | Monoid | .  | {}
Eq | ````a1````  | (Collection (a0, a1)) | Eq | {"a0" --> (a0, a1)}
Monoid | ````a0````  ````a1````  | _Native_  | Monoid | {}
(Mappable a1) | ````a0````  ````a1````  | _Native_  | (Mappable a1) | {"a0" --> a1}
(Mappable (a0, a1)) | ````a1````  | (Collection (a0, a1)) | (Mappable a0) | {"a0" --> (a0, a1)}
(Collection (a0, a1)) | ````a0```` : {````Eq```` } ````a1````  | _Native_  | (Collection (a0, a1)) | {"a0" --> (a0, a1)}


### Supertypes of List a0

Is type | Requirements | Via | Orig type | Binding
------- | ------------ | --- | --------- | -------
.  | ````a0````  | Monoid | .  | {}
Eq | ````a0````  | (Collection a0) | Eq | {"a0" --> a0}
Monoid | ````a0````  | _Native_  | Monoid | {}
(Mappable a0) | ````a0````  | _Native_  | (Mappable a0) | {"a0" --> a0}
(Mappable v1) | ````k1```` : {````(k1, v1)```` } | ((Dict k1) v1) | (Mappable a1) | {"a0" --> k1 "a1" --> v1}
(Mappable [v1]) | ````k1```` : {````(k1, v1)```` } | ((Dict k1) [v1]) | (Mappable a1) | {"a0" --> k1 "a1" --> [v1]}
(Mappable (k1, v1)) | ````k1```` : {````(k1, v1)```` } | ((Dict k1) v1) | (Mappable (a0, a1)) | {"a0" --> k1 "a1" --> v1}
(Mappable (k1, [v1])) | ````k1```` : {````(k1, v1)```` } | ((Dict k1) [v1]) | (Mappable (a0, a1)) | {"a0" --> k1 "a1" --> [v1]}
(Collection a0) | ````a0````  | _Native_  | (Collection a0) | {"a0" --> a0}
(Collection (k1, v1)) | ````k1```` : {````(k1, v1)```` } | ((Dict k1) v1) | (Collection (a0, a1)) | {"a0" --> k1 "a1" --> v1}
(Collection (k1, [v1])) | ````k1```` : {````(k1, v1)```` } | ((Dict k1) [v1]) | (Collection (a0, a1)) | {"a0" --> k1 "a1" --> [v1]}
{a0} | ````a0````  | _Native_  | {a0} | {"a0" --> a0}
((Dict k1) v1) | ````a0```` : {````(k1, v1)```` } | _Native_  | ((Dict k1) v1) | {"a0" --> k1 "a1" --> v1}
((Dict k1) [v1]) | ````a0```` : {````(k1, v1)```` } | _Native_  | ((Dict k1) [v1]) | {"a0" --> k1 "a1" --> [v1]}


### Supertypes of Maybe a0

Is type | Requirements | Via | Orig type | Binding
------- | ------------ | --- | --------- | -------
.  | ````a0````  | _Native_  | .  | {}


### Supertypes of More a0

Is type | Requirements | Via | Orig type | Binding
------- | ------------ | --- | --------- | -------
.  | ````a0````  | _Native_  | .  | {}
Eq | ````a0````  | (Collection a0) | Eq | {"a0" --> a0}
Monoid | ````a0````  | (Collection a0) | Monoid | {"a0" --> a0}
(Mappable a0) | ````a0````  | (Collection a0) | (Mappable a0) | {"a0" --> a0}
(Collection a0) | ````a0````  | _Native_  | (Collection a0) | {"a0" --> a0}


### Supertypes of Set a0

Is type | Requirements | Via | Orig type | Binding
------- | ------------ | --- | --------- | -------
.  | ````a0```` : {````Eq```` } | (Collection a0) | .  | {"a0" --> a0}
Eq | ````a0```` : {````Eq```` } | (Collection a0) | Eq | {"a0" --> a0}
Monoid | ````a0```` : {````Eq```` } | (Collection a0) | Monoid | {"a0" --> a0}
(Mappable a0) | ````a0```` : {````Eq```` } | (Collection a0) | (Mappable a0) | {"a0" --> a0}
(Collection a0) | ````a0```` : {````Eq```` } | _Native_  | (Collection a0) | {"a0" --> a0}


### Supertypes of Tuple a0 a1

Is type | Requirements | Via | Orig type | Binding
------- | ------------ | --- | --------- | -------
.  | ````a0````  ````a1````  | _Native_  | .  | {}
Eq | ````a0```` : {````Eq```` } ````a1```` : {````Eq```` } | _Native_  | Eq | {}
(Mappable a1) | ````a0````  ````a1````  | _Native_  | (Mappable a1) | {"a0" --> a1}


### Supertypes of Void 

Is type | Requirements | Via | Orig type | Binding
------- | ------------ | --- | --------- | -------
.  |  | _Native_  | .  | {}


### Supertypes of PrivateKey 

Is type | Requirements | Via | Orig type | Binding
------- | ------------ | --- | --------- | -------
.  |  | _Native_  | .  | {}


### Supertypes of PubPrivAlgo a0 a1

Is type | Requirements | Via | Orig type | Binding
------- | ------------ | --- | --------- | -------
.  | ````a0```` : {````PrivateKey```` } ````a1```` : {````PublicKey```` } | _Native_  | .  | {}


### Supertypes of PublicKey 

Is type | Requirements | Via | Orig type | Binding
------- | ------------ | --- | --------- | -------
.  |  | _Native_  | .  | {}


### Supertypes of RSA 

Is type | Requirements | Via | Orig type | Binding
------- | ------------ | --- | --------- | -------
.  |  | ((PubPrivAlgo RSAPrivKey) RSAPubKey) | .  | {"a0" --> pietervdvn:Data:Crypto.PubPrivAlgo.RSAPrivKey "a1" --> pietervdvn:Data:Crypto.PubPrivAlgo.RSAPubKey}
((PubPrivAlgo RSAPrivKey) RSAPubKey) |  | _Native_  | ((PubPrivAlgo RSAPrivKey) RSAPubKey) | {"a0" --> pietervdvn:Data:Crypto.PubPrivAlgo.RSAPrivKey "a1" --> pietervdvn:Data:Crypto.PubPrivAlgo.RSAPubKey}


### Supertypes of RSAPrivKey 

Is type | Requirements | Via | Orig type | Binding
------- | ------------ | --- | --------- | -------
.  |  | _Native_  | .  | {}
PrivateKey |  | _Native_  | PrivateKey | {}


### Supertypes of RSAPubKey 

Is type | Requirements | Via | Orig type | Binding
------- | ------------ | --- | --------- | -------
.  |  | _Native_  | .  | {}
PublicKey |  | _Native_  | PublicKey | {}


### Supertypes of Bool 

Is type | Requirements | Via | Orig type | Binding
------- | ------------ | --- | --------- | -------
.  |  | _Native_  | .  | {}


### Supertypes of Disjunct a0

Is type | Requirements | Via | Orig type | Binding
------- | ------------ | --- | --------- | -------
.  |  | {Set a0} | .  | {"a0" --> {a0}}
Eq |  | {Set a0} | Eq | {"a0" --> {a0}}
Monoid |  | {Set a0} | Monoid | {"a0" --> {a0}}
(Mappable {a0}) |  | {Set a0} | (Mappable a0) | {"a0" --> {a0}}
(Collection {a0}) |  | {Set a0} | (Collection a0) | {"a0" --> {a0}}
{Set a0} | ````a0```` : {````Eq```` } | _Native_  | {Set a0} | {"a0" --> {a0}}


### Supertypes of Graph a0 a1

Is type | Requirements | Via | Orig type | Binding
------- | ------------ | --- | --------- | -------
.  | ````a0```` : {````Eq```` , ````Ord```` } ````a1````  | _Native_  | .  | {}


### Supertypes of Weighted a0 a1 a2 a3

Is type | Requirements | Via | Orig type | Binding
------- | ------------ | --- | --------- | -------
.  | ````a1```` : {````Graph```` } ````a3```` : {````Eq```` , ````Ord```` } ````a2```` : {````Eq```` , ````Monoid```` , ````Ord```` } ````a3````  | ((Graph a1) a3) | .  | {"a0" --> a1 "a1" --> a3}
((Graph a1) a3) | ````a0```` : {````Graph```` } ````a1```` : {````Eq```` , ````Ord```` } ````a2```` : {````Eq```` , ````Monoid```` , ````Ord```` } ````a3````  | _Native_  | ((a0 a1) a3) | {"a0" --> a1 "a1" --> a3}
((a0 a1) a3) | ````a0```` : {````Graph```` } ````a1```` : {````Eq```` , ````Ord```` } ````a2```` : {````Eq```` , ````Monoid```` , ````Ord```` } ````a3````  | _Native_  | ((a0 a1) a3) | {"a0" --> a1 "a1" --> a3}


### Supertypes of Int 

Is type | Requirements | Via | Orig type | Binding
------- | ------------ | --- | --------- | -------
.  |  | IntInf | .  | {}
BIInt |  | IntInf | BIInt | {}
Eq |  | IntInf | Eq | {}
IntInf |  | _Native_  | IntInf | {}


### Supertypes of Int' 

Is type | Requirements | Via | Orig type | Binding
------- | ------------ | --- | --------- | -------
.  |  | Int | .  | {}
BIInt |  | Int | BIInt | {}
Eq |  | Int | Eq | {}
Int |  | _Native_  | Int | {}
IntInf |  | Int | IntInf | {}
IntInf' |  | _Native_  | IntInf' | {}


### Supertypes of IntInf 

Is type | Requirements | Via | Orig type | Binding
------- | ------------ | --- | --------- | -------
.  |  | BIInt | .  | {}
BIInt |  | _Native_  | BIInt | {}
Eq |  | _Native_  | Eq | {}


### Supertypes of IntInf' 

Is type | Requirements | Via | Orig type | Binding
------- | ------------ | --- | --------- | -------
.  |  | IntInf | .  | {}
BIInt |  | IntInf | BIInt | {}
Eq |  | IntInf | Eq | {}
IntInf |  | _Native_  | IntInf | {}


### Supertypes of Nat 

Is type | Requirements | Via | Orig type | Binding
------- | ------------ | --- | --------- | -------
.  |  | Int | .  | {}
BIInt |  | Int | BIInt | {}
Eq |  | Int | Eq | {}
Int |  | _Native_  | Int | {}
IntInf |  | Int | IntInf | {}
NatInf |  | _Native_  | NatInf | {}


### Supertypes of Nat' 

Is type | Requirements | Via | Orig type | Binding
------- | ------------ | --- | --------- | -------
.  |  | Int' | .  | {}
BIInt |  | Int' | BIInt | {}
Eq |  | Int' | Eq | {}
Int |  | Int' | Int | {}
Int' |  | _Native_  | Int' | {}
IntInf |  | Int' | IntInf | {}
IntInf' |  | Int' | IntInf' | {}
Nat |  | _Native_  | Nat | {}
NatInf |  | Nat | NatInf | {}


### Supertypes of NatInf 

Is type | Requirements | Via | Orig type | Binding
------- | ------------ | --- | --------- | -------
.  |  | IntInf | .  | {}
BIInt |  | IntInf | BIInt | {}
Eq |  | IntInf | Eq | {}
IntInf |  | _Native_  | IntInf | {}


### Supertypes of NatInf' 

Is type | Requirements | Via | Orig type | Binding
------- | ------------ | --- | --------- | -------
.  |  | IntInf' | .  | {}
BIInt |  | IntInf' | BIInt | {}
Eq |  | IntInf' | Eq | {}
IntInf |  | IntInf' | IntInf | {}
IntInf' |  | _Native_  | IntInf' | {}
NatInf |  | _Native_  | NatInf | {}


### Supertypes of Flip a0 a1 a2

Is type | Requirements | Via | Orig type | Binding
------- | ------------ | --- | --------- | -------
((a0 a2) a1) | ````a0````  ````a1````  ````a2````  | _Native_  | ((a0 a2) a1) | {"a0" --> a2 "a1" --> a1}




> This page was automatically generated.
> 
> 
> Do not edit it, as regeneration will overwrite your changes.
> 
> 
> Back to [index](Index.md)