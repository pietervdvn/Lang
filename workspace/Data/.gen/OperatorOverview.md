# Operator overview

# Precedences overview

The higher the operator stands in the table (the lower the number), the more range it will have. The lower it stands, the tighter the operator binds. The lower the operator stands, the earlier it will be evaluated

To test precedence, invoke ````--p <expression>```` in the interpreter, which converts expression to prefix notation.

Precedence | Operators | Associativity
---------- | --------- | -------------
1 | ``:`` | left (default)
2 | ``?`` | right
3 | ``&&``, ``||`` | left
4 | ``==``, ``!=`` | left
5 | ``!`` | prefix
6 | ``<``, ``>`` | left
7 | ``+``, ``-`` | left
8 | ``*``, ``%``, ``/``, ``//`` | left
9 | ``^`` | left
10 | Other operators | left



Operator | Precedence | Associativity
-------- | ---------- | -------------
``!`` | 5 | prefix
``!=`` | 4 | left
``%`` | 8 | left (default)
``&&`` | 3 | left
``*`` | 8 | left
``+`` | 7 | left
``-`` | 7 | left (default)
``/`` | 8 | left (default)
``//`` | 8 | left (default)
``:`` | 1 | left (default)
``<`` | 6 | left
``==`` | 4 | left
``>`` | 6 | left (default)
``?`` | 2 | right
``^`` | 9 | left
``||`` | 3 | left





> This page was automatically generated on 2015-01-05 21:27:57 UTC (2015-01-05 22:27:57 CET)
> 
> 
> Do not edit it, as regeneration will overwrite your changes.
> 
> 
> Back to [index](Index.md)