# Precedences overview

The higher the operator stand in the table (the lower the number), the more range it will have. The lower it stands, the tighter the operator binds. The lower the operator stands, the earlier it will be evaluated

To test precedence, invoke ````--p <expression>```` in the interpreter, which converts expression to prefix notation.

Precedence | Operators | Associativity
---------- | --------- | -------------
1 | ``:`` | right
2 | ``&&``, ``||`` | left
3 | ``==``, ``!=`` | left
4 | ``+``, ``-`` | left
5 | ``!`` | prefix
6 | ``*``, ``/``, ``%`` | left
7 | ``²``, ``³`` | postfix
8 | Other operators | left

Operator | Precedence | Associativity
-------- | ---------- | -------------
``!`` | 5 | prefix
``!=`` | 3 | left
``%`` | 6 | left
``&&`` | 2 | left
``*`` | 6 | left
``+`` | 4 | left
``-`` | 4 | left
``/`` | 6 | left
``:`` | 1 | right
``==`` | 3 | left
``||`` | 2 | left
``²`` | 7 | postfix
``³`` | 7 | postfix
