> Back to [index](Index.md)

# Type overview

Type | Declared in | Kind | Docstring
---- | ----------- | ---- | ---------
**Any**  | Any | ````*````  | The supertype of every single type. Each type has ````Any```` implicitly as supertype.![Supertype.jpg](../doc/res/Supertype.jpg)
**Eq**  | Prelude | ````*````  | Contains a lot of usefull functions, types, classes.
**X** (````a0```` :````Eq```` ) | Prelude | ````(* ~> *)````  | Contains a lot of usefull functions, types, classes.
**Y** ````a0````  | Prelude | ````(* ~> *)````  | Contains a lot of usefull functions, types, classes.
**Z** ````a0````  | Prelude | ````(* ~> *)````  | Contains a lot of usefull functions, types, classes.


## Supertypetables per type

### How to read a 'Supertypetable of T a0 a1'

````T a0 a1````  is the type given in **Is Type**  , if the **requirements**  on the free type variables are met. This table contains always the same number of frees, but a certain supertype can demand extra requirements.

The **Via**  column codes via what type this specific supertype was added. This means that, if ````List````  has supertype ````Collection```` , and ````Collection````  has supertype ````Mappable```` , that ````List```` has the suppertype````Mappable```` , which has been added via````Collection```` . _Native_ denotes that this supertype was added via the code.

A **Binding** might have happened on this supertype. E.g ````List (k,v)```` has the supertype````Dict k v```` .````Dict a0 a1```` has the supertype````Collection a0 a1```` .So if we want to add the supertype````Collection a0 a1```` to````List (k,v)```` , we have to substitute````a0 --> k, a1 --> v```` in the````Collection a0 a1```` example, if we want it to be correct.The **Orig Type** show this type before the substitution.

### Supertypes of Eq 

Is type | Requirements | Via | Orig type | Origsuper -> actualsuper binding | Via -> Current binding
------- | ------------ | --- | --------- | -------------------------------- | ----------------------
.  |  | _Native_  | .  | {} | _Native_ 


### Supertypes of X (````a0```` :````Eq```` )

Is type | Requirements | Via | Orig type | Origsuper -> actualsuper binding | Via -> Current binding
------- | ------------ | --- | --------- | -------------------------------- | ----------------------
.  | (````a0```` :````Eq```` ) | _Native_  | .  | {} | _Native_ 


### Supertypes of Y ````a0```` 

Is type | Requirements | Via | Orig type | Origsuper -> actualsuper binding | Via -> Current binding
------- | ------------ | --- | --------- | -------------------------------- | ----------------------
.  | (````a0```` :````Eq```` ) | (X a0) | .  | {} | {}
(X a0) | (````a0```` :````Eq```` ) | _Native_  | (X a0) | {"a0" --> a0} | _Native_ 


### Supertypes of Z ````a0```` 

Is type | Requirements | Via | Orig type | Origsuper -> actualsuper binding | Via -> Current binding
------- | ------------ | --- | --------- | -------------------------------- | ----------------------
.  |  | _Native_  | .  | {} | _Native_ 
(X a0) | (````a0```` :````Eq```` ) | (Y a0) | (X a0) | {} | {}
(Y a0) |  | _Native_  | (Y a0) | {"a0" --> a0} | _Native_ 




> This page was automatically generated.
> 
> 
> Do not edit it, as regeneration will overwrite your changes.
> 
> 
> Back to [index](Index.md)
> <div id="clicker">click</div>