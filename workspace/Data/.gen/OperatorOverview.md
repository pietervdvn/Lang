> Back to [index](Index.md)

# Operator overview

# Precedences overview

The higher the operator stands in the table (the lower the number), the more range it will have. The lower it stands, the tighter the operator binds. The lower the operator stands, the earlier it will be evaluated

To test precedence, invoke ````--p <expression>```` in the interpreter, which converts expression to prefix notation.

Precedence | Operators | Associativity
---------- | --------- | -------------
0 | ``!`` | prefix
1 | ``!=``, ``==`` | left
2 | ``%``, ``*``, ``/``, ``//`` | left (default)
3 | ``&&``, ``||`` | left
4 | ``+``, ``-`` | left
5 | ``:`` | left (default)
6 | ``<``, ``>`` | left
7 | ``?`` | right
8 | ``^`` | left
9 | Other operators | left
10 | Function application | left




Operator | Precedence | Associativity
-------- | ---------- | -------------
``!`` | 0 | prefix
``!=`` | 1 | left
``%`` | 2 | left (default)
``&&`` | 3 | left
``*`` | 2 | left
``+`` | 4 | left
``-`` | 4 | left (default)
``/`` | 2 | left (default)
``//`` | 2 | left (default)
``:`` | 5 | left (default)
``<`` | 6 | left
``==`` | 1 | left
``>`` | 6 | left (default)
``?`` | 7 | right
``^`` | 8 | left
``||`` | 3 | left






> This page was automatically generated.
> 
> 
> Do not edit it, as regeneration will overwrite your changes.
> 
> 
> Back to [index](Index.md)
> <div id="clicker">click</div>