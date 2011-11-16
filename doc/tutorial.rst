
Tutorial
********

Basic Types
===========

``Int``
-------

``Int`` is a type for integers. There are no upper limit and no lower limit for
a value.

``Bool``
--------

``Bool`` is a type for ``true`` and ``false``.

``String``
----------

A double quoted string like ``"foobarbazquux"`` is an instance of ``String``.

``Nil``
-------

``Nil`` is a type of ``nil``.

true or false
-------------

``nil`` and ``false`` are false. Anything else are true.

Structured Programming
======================

Loops
-----

There are three statements for looping; ``while``, ``iterate`` and ``every``.

``while`` Statement
'''''''''''''''''''

``while`` statemet repeats inside code while given expression is true::

  i = 0
  while i < 42
    puts(i)
    i += 1
  end

The above code prints 42 numbers from zero (to 41).

``iterate`` Statement
'''''''''''''''''''''

``iterate`` statement picks up values in an array or a dictionary, and
assigns it to variables after the keyword ``as`` for each iteration::

  foo = [42, 26]
  iterate foo as bar
    puts(bar)
  end

The above code prints ``42`` and ``26``.

``iterate`` statement picks up both of a key and a value of a dictionary for one
iteration. So you must give two variables after ``as`` separated with "``,``"::

  foo = { 42: 26 }
  iterate foo as key, value
    puts(key)
    puts(value)
  end

The above code prints ``42`` and ``26``.

Function
========

Function Definition
-------------------

A function definition has ``def`` and its name following optional formal
arguments enclosing parenthesises ("``(``" and "``)``"), like::

  def add(bar, baz)
    return bar + baz
  end

``return`` statement returns a value to a caller.

Function Calling
----------------

A function calling consists of its name and actual arguments enclosing
parenthesises, like::

  add(42, 26)

Exception
=========

``try`` Statement
-----------------

``except`` Clause
-----------------

``finally`` Clause
------------------

.. vim: tabstop=2 shiftwidth=2 expandtab softtabstop=2 filetype=rst
