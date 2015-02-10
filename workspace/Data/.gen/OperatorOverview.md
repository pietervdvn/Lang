# Operator overview

# Precedences overview

The higher the operator stands in the table (the lower the number), the more range it will have. The lower it stands, the tighter the operator binds. The lower the operator stands, the earlier it will be evaluated

To test precedence, invoke ````--p <expression>```` in the interpreter, which converts expression to prefix notation.

Precedence | Operators | Associativity
---------- | --------- | -------------
0 | ``!=``, ``==`` | left
1 | ``&&``, ``||`` | left
2 | ``!`` | prefix
3 | Other operators | left
4 | Function application | left



Operator | Precedence | Associativity
-------- | ---------- | -------------
``!`` | 2 | prefix
``!=`` | 0 | left
``&&`` | 1 | left
``==`` | 0 | left
``||`` | 1 | left





> This page was automatically generated.
> 
> 
> Do not edit it, as regeneration will overwrite your changes.
> 
> 
> Back to [index](Index.md)