# Type overview

Type | Declared in | Kind | Docstring
---- | ----------- | ---- | ---------
**Any**  | Any | ````*````  | The supertype of every single type. Each type has ````Any```` implicitly as supertype.
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
**List** ````y:Monoid````  | Collection.List | ````(* ~> *)````  | A ````Collection```` which preserves order and allows duplicate elements.
**MyList** ````x:Eq````  | Collection.List | ````(* ~> *)````  | A ````Collection```` which preserves order and allows duplicate elements.
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

## Supertypes 

Type | Is subtype of
---- | -------------
BIInt | Any
Eq | Any
Associative````a0````  | (a0 -> (a0 -> a0))
Commutative````a0````  ````a1````  | (a0 -> (a0 -> a1))
Curry````a0````  ````a1````  | (a0 -> a1)
Mappable````a0````  | Any
Monoid | Any
Product | Any, Monoid
Sum | Any, Monoid
Ord | Any
Collection````a0````  | Any, Monoid, (Mappable a0)
Collection````a0:Eq````  | Eq
Dict````a0````  ````a1````  | Monoid, (Mappable a1)
Dict````a0:Eq````  ````a1````  | (Collection ((Tuple a0) a1))
List````a0````  | Monoid, (Mappable a0)
List````a0:Monoid````  | (Collection a0)
List````a0:Eq````  | (Set a0)
List````a0:((Tuple k1) v1)````  | ((Dict k1) v1), ((Dict k1) (List v1))
MyList````a0:Eq````  | (List a0)
Maybe````a0````  | Any
More````a0````  | Any, (Collection a0)
Set````a0:Eq````  | (Collection a0)
Tuple````a0````  ````a1````  | Any, (Mappable a1)
Tuple````a0:Eq````  ````a1:Eq````  | Eq
Void | Any
PrivateKey | Any
PubPrivAlgo````a0:PrivateKey````  ````a1:PublicKey````  | Any
PublicKey | Any
RSA | ((PubPrivAlgo RSAPrivKey) RSAPubKey)
RSAPrivKey | Any, PrivateKey
RSAPubKey | Any, PublicKey
Bool | Any
Disjunct````a0:Eq````  | (Set (Set a0))
Graph````a0:Eq, Ord````  ````a1````  | Any
Weighted````a0:Graph````  ````a1:Ord, Eq````  ````a2:Monoid, Ord, Eq````  ````a3````  | ((a0 a1) a3)
Int | IntInf
Int' | Int, IntInf'
IntInf | BIInt, Eq
IntInf' | IntInf
Nat | Int, NatInf
Nat' | Int', Nat
NatInf | IntInf
NatInf' | IntInf', NatInf
Flip````a0````  ````a1````  ````a2````  | ((a0 a2) a1)



### Supertypes of BIInt

Is type | #Frees | Requirements | Binding | Via
------- | ------ | ------------ | ------- | ---
Any | 0 |  | {} | _Native_ 

### Supertypes of Eq

Is type | #Frees | Requirements | Binding | Via
------- | ------ | ------------ | ------- | ---
Any | 0 |  | {} | _Native_ 

### Supertypes of Associative

Is type | #Frees | Requirements | Binding | Via
------- | ------ | ------------ | ------- | ---
(a0 -> (a0 -> a0)) | 1 | ````a0````  | {} | _Native_ 

### Supertypes of Commutative

Is type | #Frees | Requirements | Binding | Via
------- | ------ | ------------ | ------- | ---
(a0 -> (a0 -> a1)) | 2 | ````a0````  ````a1````  | {} | _Native_ 

### Supertypes of Curry

Is type | #Frees | Requirements | Binding | Via
------- | ------ | ------------ | ------- | ---
(a0 -> a1) | 2 | ````a0````  ````a1````  | {} | _Native_ 

### Supertypes of Mappable

Is type | #Frees | Requirements | Binding | Via
------- | ------ | ------------ | ------- | ---
Any | 1 | ````a0````  | {} | _Native_ 

### Supertypes of Monoid

Is type | #Frees | Requirements | Binding | Via
------- | ------ | ------------ | ------- | ---
Any | 0 |  | {} | _Native_ 

### Supertypes of Product

Is type | #Frees | Requirements | Binding | Via
------- | ------ | ------------ | ------- | ---
Any | 0 |  | {} | _Native_ 
Monoid | 0 |  | {} | _Native_ 

### Supertypes of Sum

Is type | #Frees | Requirements | Binding | Via
------- | ------ | ------------ | ------- | ---
Any | 0 |  | {} | _Native_ 
Monoid | 0 |  | {} | _Native_ 

### Supertypes of Ord

Is type | #Frees | Requirements | Binding | Via
------- | ------ | ------------ | ------- | ---
Any | 0 |  | {} | _Native_ 

### Supertypes of Collection

Is type | #Frees | Requirements | Binding | Via
------- | ------ | ------------ | ------- | ---
Any | 1 | ````a0````  | {} | _Native_ 
Eq | 1 | ````a0```` : {Eq} | {} | _Native_ 
Monoid | 1 | ````a0````  | {} | _Native_ 
(Mappable a0) | 1 | ````a0````  | {"a0" --> a0} | _Native_ 

### Supertypes of Dict

Is type | #Frees | Requirements | Binding | Via
------- | ------ | ------------ | ------- | ---
Any | 0 |  | {} | Monoid
Eq | 0 |  | {"a0" --> ((pietervdvn:Data:Collection.Tuple.Tuple a0) a1)} | (Collection ((Tuple a0) a1))
Monoid | 2 | ````a0````  ````a1````  | {} | _Native_ 
(Mappable a1) | 2 | ````a0````  ````a1````  | {"a0" --> a1} | _Native_ 
(Mappable ((Tuple a0) a1)) | 0 |  | {"a0" --> ((pietervdvn:Data:Collection.Tuple.Tuple a0) a1)} | (Collection ((Tuple a0) a1))
(Collection ((Tuple a0) a1)) | 2 | ````a0```` : {Eq} ````a1````  | {"a0" --> ((pietervdvn:Data:Collection.Tuple.Tuple a0) a1)} | _Native_ 

### Supertypes of List

Is type | #Frees | Requirements | Binding | Via
------- | ------ | ------------ | ------- | ---
Any | 0 |  | {} | Monoid
Eq | 0 |  | {"a0" --> a0} | (Collection a0)
Monoid | 1 | ````a0````  | {} | _Native_ 
(Mappable a0) | 1 | ````a0````  | {"a0" --> a0} | _Native_ 
(Mappable v1) | 0 |  | {"a0" --> k1 "a1" --> v1} | ((Dict k1) v1)
(Mappable (List v1)) | 0 |  | {"a0" --> k1 "a1" --> (pietervdvn:Data:Collection.List.List v1)} | ((Dict k1) (List v1))
(Mappable ((Tuple k1) v1)) | 0 |  | {"a0" --> k1 "a1" --> v1} | ((Dict k1) v1)
(Mappable ((Tuple k1) (List v1))) | 0 |  | {"a0" --> k1 "a1" --> (pietervdvn:Data:Collection.List.List v1)} | ((Dict k1) (List v1))
(Collection a0) | 1 | ````a0```` : {Monoid} | {"a0" --> a0} | _Native_ 
(Collection ((Tuple k1) v1)) | 0 |  | {"a0" --> k1 "a1" --> v1} | ((Dict k1) v1)
(Collection ((Tuple k1) (List v1))) | 0 |  | {"a0" --> k1 "a1" --> (pietervdvn:Data:Collection.List.List v1)} | ((Dict k1) (List v1))
(Set a0) | 1 | ````a0```` : {Eq} | {"a0" --> a0} | _Native_ 
((Dict k1) v1) | 1 | ````a0```` : {((Tuple k1) v1)} | {"a0" --> k1 "a1" --> v1} | _Native_ 
((Dict k1) (List v1)) | 1 | ````a0```` : {((Tuple k1) v1)} | {"a0" --> k1 "a1" --> (pietervdvn:Data:Collection.List.List v1)} | _Native_ 

### Supertypes of MyList

Is type | #Frees | Requirements | Binding | Via
------- | ------ | ------------ | ------- | ---
Any | 0 |  | {"a0" --> a0} | (List a0)
Eq | 0 |  | {"a0" --> a0} | (List a0)
Monoid | 0 |  | {"a0" --> a0} | (List a0)
(Mappable a0) | 0 |  | {"a0" --> a0} | (List a0)
(Mappable v1) | 0 |  | {"a0" --> k1} | (List a0)
(Mappable (List v1)) | 0 |  | {"a0" --> k1} | (List a0)
(Mappable ((Tuple k1) v1)) | 0 |  | {"a0" --> k1} | (List a0)
(Mappable ((Tuple k1) (List v1))) | 0 |  | {"a0" --> k1} | (List a0)
(Collection a0) | 0 |  | {"a0" --> a0} | (List a0)
(Collection ((Tuple k1) v1)) | 0 |  | {"a0" --> k1} | (List a0)
(Collection ((Tuple k1) (List v1))) | 0 |  | {"a0" --> k1} | (List a0)
(List a0) | 1 | ````a0```` : {Eq} | {"a0" --> a0} | _Native_ 
(Set a0) | 0 |  | {"a0" --> a0} | (List a0)
((Dict k1) v1) | 0 |  | {"a0" --> k1} | (List a0)
((Dict k1) (List v1)) | 0 |  | {"a0" --> k1} | (List a0)

### Supertypes of Maybe

Is type | #Frees | Requirements | Binding | Via
------- | ------ | ------------ | ------- | ---
Any | 1 | ````a0````  | {} | _Native_ 

### Supertypes of More

Is type | #Frees | Requirements | Binding | Via
------- | ------ | ------------ | ------- | ---
Any | 1 | ````a0````  | {} | _Native_ 
Eq | 0 |  | {"a0" --> a0} | (Collection a0)
Monoid | 0 |  | {"a0" --> a0} | (Collection a0)
(Mappable a0) | 0 |  | {"a0" --> a0} | (Collection a0)
(Collection a0) | 1 | ````a0````  | {"a0" --> a0} | _Native_ 

### Supertypes of Set

Is type | #Frees | Requirements | Binding | Via
------- | ------ | ------------ | ------- | ---
Any | 0 |  | {"a0" --> a0} | (Collection a0)
Eq | 0 |  | {"a0" --> a0} | (Collection a0)
Monoid | 0 |  | {"a0" --> a0} | (Collection a0)
(Mappable a0) | 0 |  | {"a0" --> a0} | (Collection a0)
(Collection a0) | 1 | ````a0```` : {Eq} | {"a0" --> a0} | _Native_ 

### Supertypes of Tuple

Is type | #Frees | Requirements | Binding | Via
------- | ------ | ------------ | ------- | ---
Any | 2 | ````a0````  ````a1````  | {} | _Native_ 
Eq | 2 | ````a0```` : {Eq} ````a1```` : {Eq} | {} | _Native_ 
(Mappable a1) | 2 | ````a0````  ````a1````  | {"a0" --> a1} | _Native_ 

### Supertypes of Void

Is type | #Frees | Requirements | Binding | Via
------- | ------ | ------------ | ------- | ---
Any | 0 |  | {} | _Native_ 

### Supertypes of PrivateKey

Is type | #Frees | Requirements | Binding | Via
------- | ------ | ------------ | ------- | ---
Any | 0 |  | {} | _Native_ 

### Supertypes of PubPrivAlgo

Is type | #Frees | Requirements | Binding | Via
------- | ------ | ------------ | ------- | ---
Any | 2 | ````a0```` : {PrivateKey} ````a1```` : {PublicKey} | {} | _Native_ 

### Supertypes of PublicKey

Is type | #Frees | Requirements | Binding | Via
------- | ------ | ------------ | ------- | ---
Any | 0 |  | {} | _Native_ 

### Supertypes of RSA

Is type | #Frees | Requirements | Binding | Via
------- | ------ | ------------ | ------- | ---
Any | 0 |  | {"a0" --> pietervdvn:Data:Crypto.PubPrivAlgo.RSAPrivKey "a1" --> pietervdvn:Data:Crypto.PubPrivAlgo.RSAPubKey} | ((PubPrivAlgo RSAPrivKey) RSAPubKey)
((PubPrivAlgo RSAPrivKey) RSAPubKey) | 0 |  | {"a0" --> pietervdvn:Data:Crypto.PubPrivAlgo.RSAPrivKey "a1" --> pietervdvn:Data:Crypto.PubPrivAlgo.RSAPubKey} | _Native_ 

### Supertypes of RSAPrivKey

Is type | #Frees | Requirements | Binding | Via
------- | ------ | ------------ | ------- | ---
Any | 0 |  | {} | _Native_ 
PrivateKey | 0 |  | {} | _Native_ 

### Supertypes of RSAPubKey

Is type | #Frees | Requirements | Binding | Via
------- | ------ | ------------ | ------- | ---
Any | 0 |  | {} | _Native_ 
PublicKey | 0 |  | {} | _Native_ 

### Supertypes of Bool

Is type | #Frees | Requirements | Binding | Via
------- | ------ | ------------ | ------- | ---
Any | 0 |  | {} | _Native_ 

### Supertypes of Disjunct

Is type | #Frees | Requirements | Binding | Via
------- | ------ | ------------ | ------- | ---
Any | 0 |  | {"a0" --> (pietervdvn:Data:Collection.Set.Set a0)} | (Set (Set a0))
Eq | 0 |  | {"a0" --> (pietervdvn:Data:Collection.Set.Set a0)} | (Set (Set a0))
Monoid | 0 |  | {"a0" --> (pietervdvn:Data:Collection.Set.Set a0)} | (Set (Set a0))
(Mappable (Set a0)) | 0 |  | {"a0" --> (pietervdvn:Data:Collection.Set.Set a0)} | (Set (Set a0))
(Collection (Set a0)) | 0 |  | {"a0" --> (pietervdvn:Data:Collection.Set.Set a0)} | (Set (Set a0))
(Set (Set a0)) | 1 | ````a0```` : {Eq} | {"a0" --> (pietervdvn:Data:Collection.Set.Set a0)} | _Native_ 

### Supertypes of Graph

Is type | #Frees | Requirements | Binding | Via
------- | ------ | ------------ | ------- | ---
Any | 2 | ````a0```` : {Eq, Ord} ````a1````  | {} | _Native_ 

### Supertypes of Weighted

Is type | #Frees | Requirements | Binding | Via
------- | ------ | ------------ | ------- | ---
Any | 0 |  | {"a0" --> a1 "a1" --> a3} | ((Graph a1) a3)
((Graph a1) a3) | 4 | ````a0```` : {Graph} ````a1```` : {Eq, Ord} ````a2```` : {Eq, Monoid, Ord} ````a3````  | {"a0" --> a1 "a1" --> a3} | _Native_ 
((a0 a1) a3) | 4 | ````a0```` : {Graph} ````a1```` : {Eq, Ord} ````a2```` : {Eq, Monoid, Ord} ````a3````  | {"a0" --> a1 "a1" --> a3} | _Native_ 

### Supertypes of Int

Is type | #Frees | Requirements | Binding | Via
------- | ------ | ------------ | ------- | ---
Any | 0 |  | {} | IntInf
BIInt | 0 |  | {} | IntInf
Eq | 0 |  | {} | IntInf
IntInf | 0 |  | {} | _Native_ 

### Supertypes of Int'

Is type | #Frees | Requirements | Binding | Via
------- | ------ | ------------ | ------- | ---
Any | 0 |  | {} | Int
BIInt | 0 |  | {} | Int
Eq | 0 |  | {} | Int
Int | 0 |  | {} | _Native_ 
IntInf | 0 |  | {} | Int
IntInf' | 0 |  | {} | _Native_ 

### Supertypes of IntInf

Is type | #Frees | Requirements | Binding | Via
------- | ------ | ------------ | ------- | ---
Any | 0 |  | {} | BIInt
BIInt | 0 |  | {} | _Native_ 
Eq | 0 |  | {} | _Native_ 

### Supertypes of IntInf'

Is type | #Frees | Requirements | Binding | Via
------- | ------ | ------------ | ------- | ---
Any | 0 |  | {} | IntInf
BIInt | 0 |  | {} | IntInf
Eq | 0 |  | {} | IntInf
IntInf | 0 |  | {} | _Native_ 

### Supertypes of Nat

Is type | #Frees | Requirements | Binding | Via
------- | ------ | ------------ | ------- | ---
Any | 0 |  | {} | Int
BIInt | 0 |  | {} | Int
Eq | 0 |  | {} | Int
Int | 0 |  | {} | _Native_ 
IntInf | 0 |  | {} | Int
NatInf | 0 |  | {} | _Native_ 

### Supertypes of Nat'

Is type | #Frees | Requirements | Binding | Via
------- | ------ | ------------ | ------- | ---
Any | 0 |  | {} | Int'
BIInt | 0 |  | {} | Int'
Eq | 0 |  | {} | Int'
Int | 0 |  | {} | Int'
Int' | 0 |  | {} | _Native_ 
IntInf | 0 |  | {} | Int'
IntInf' | 0 |  | {} | Int'
Nat | 0 |  | {} | _Native_ 
NatInf | 0 |  | {} | Nat

### Supertypes of NatInf

Is type | #Frees | Requirements | Binding | Via
------- | ------ | ------------ | ------- | ---
Any | 0 |  | {} | IntInf
BIInt | 0 |  | {} | IntInf
Eq | 0 |  | {} | IntInf
IntInf | 0 |  | {} | _Native_ 

### Supertypes of NatInf'

Is type | #Frees | Requirements | Binding | Via
------- | ------ | ------------ | ------- | ---
Any | 0 |  | {} | IntInf'
BIInt | 0 |  | {} | IntInf'
Eq | 0 |  | {} | IntInf'
IntInf | 0 |  | {} | IntInf'
IntInf' | 0 |  | {} | _Native_ 
NatInf | 0 |  | {} | _Native_ 

### Supertypes of Flip

Is type | #Frees | Requirements | Binding | Via
------- | ------ | ------------ | ------- | ---
((a0 a2) a1) | 3 | ````a0````  ````a1````  ````a2````  | {"a0" --> a2 "a1" --> a1} | _Native_ 



> This page was automatically generated.
> 
> 
> Do not edit it, as regeneration will overwrite your changes.
> 
> 
> Back to [index](Index.md)