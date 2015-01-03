# Operator overview

# Precedences overview

The higher the operator stand in the table (the lower the number), the more range it will have. The lower it stands, the tighter the operator binds. The lower the operator stands, the earlier it will be evaluated

To test precedence, invoke ````--p <expression>```` in the interpreter, which converts expression to prefix notation.

Precedence | Operators | Associativity
---------- | --------- | -------------
1 | ``&&``, ``||`` | left
2 | ``==``, ``!=`` | left
3 | ``!`` | prefix
4 | Other operators | left



Operator | Precedence | Associativity
-------- | ---------- | -------------
``!`` | 3 | prefix
``!=`` | 2 | left
``&&`` | 1 | left
``==`` | 2 | left
``||`` | 1 | left





> This page was automatically generated on 2015-01-03 19:25:09
> 
> 
> Do not edit it, as re-generation will overwrite your changes.