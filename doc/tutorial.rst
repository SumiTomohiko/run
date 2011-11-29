
Tutorial
********

Hello, world! (First Step into run)
===================================

Giving a script path to ``run`` command executes it::

  $ echo 'print("Hello, world!")' > foo.run
  $ run foo.run
  Hello, world!

Comment
=======

Single Line Comment with ``#``
------------------------------

A part from ``#`` to next newline is a comment. ``run`` ignores there::

  print("Hello, world!") # Here is a comment.

Multi Line (Smiling) Comment
----------------------------

A smiling mark ``(:`` tells a start of a multiline comment. An end mark of a
multiline comment is ``:)``::

  (:
   : Here is a comment.
   :)
  print("Hello, world!")

A multiline comment can have nested multiline comments::

  (:
   : (:
   :  :
   :  :)
   : Here is still a comment.
   :)
  print("Hello, world!")

Variable and Assignment
=======================

Assignment of run is on ALGOL style (using ``=``)::

  foo = 42

You do not need any declarations. First assignment makes a variable.

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

Heredocument
'''''''''''''

run supports heredocuments::

  print(<<EOF)
  foobarbazquux
  EOF

Above code prints ``foobarbazquux``.

``Nil``
-------

``Nil`` is a type of ``nil``.

``Array``
---------

Enclosing comma-separated-values with ``[`` and ``]`` makes an array::

  foo = [42, 1.21]

Each element in an array can be read by the following way::

  print(foo[0]) # => 42

You can know length of an array with ``size`` attribute::

  print(foo.size) # => 2

``Dict``
--------

Enclosing comma-separated-pairs with ``{`` and ``}`` makes a dictionary. One
pair consists of two values separated with ``:``::

  foo = { 42: 26 }

Each element in a dictionary can be read by the way like arrays::

  print(foo[42]) # => 26

``Function`` and ``UserFunction``
---------------------------------

In run, ``Function`` is a builtin function. On the other hand, ``UserFunction``
is a function declared in a script.

``ProcessStatus``
-----------------

``ProcessStatus`` object represents status of a process. You can take this with
``$?`` after executing a process. In usual shells (like bash and csh), ``$?`` is
an integer, but this is concrete. run hopes more abstract process status. This
is a reason why ``ProcessStatus`` exists.

If you give a ``ProcessStatus`` object in an expression which is evalulated as
boolean (like a condition expression in ``if`` statement), ``ProcessStatus`` of
zero is true, any others are false.

true or false
-------------

``nil``, ``false`` and ``ProcessStatus`` of ``0`` are false. Anything else are
true.

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

``every`` Statement
'''''''''''''''''''

``every`` statement can be used in iterating files. ``every`` statement accepts
one or more file name patterns after ``every`` keyword. ``every`` statement
searches files which are matched with this patterns, and assigns each name to
a variable after ``as`` keyword::

  every * as f
    puts(f)
  end

Above code prints files in a current directory.

Branch (``if`` Statement)
-------------------------

You will understand all of ``if`` statement with below code::

  if score < 60
    print("NG")
  elif score < 80
    print("Good")
  elif score < 90
    print("Excellent")
  else
    print("Amazing")
  end

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

run supports exceptions like Java and Python, etc.

``try`` Statement
-----------------

``try`` statement declares range of catching an exception. ``try`` statement
must have one or more ``except`` clauses, one ``finally`` clause or both::

  try
    # Code expecting exceptions
  except Exception as e
    # e is an exception.
  finally
    # Cleanup
  end

``except`` Clause
-----------------

One ``except`` clause has an exception class to catch, and a variable name to
store an exception after ``as`` keyword.

``finally`` Clause
------------------

run executes code in ``finally`` clause always.

``raise`` Statement
-------------------

You can raise an exception with ``raise`` statement.

Traceback
---------

If an exception is not catched, run show a traceback::

  $ cat foo.run

  def foo()
    raise Exception.new(42)
  end

  foo()
  $ run foo.run
  Traceback (most recent call last):
    File "foo.run", line 6, in <module>
    File "foo.run", line 3, in foo
  Exception: 42

Command Executing
=================

Basic
-----

Executing a command is simple::

  echo "Hello, world!"

Pipeline
--------

Joining some commands with ``|`` becomes a pipeline::

  echo "Hello, world!" | cat -n

Redirection
-----------

``<`` with a path after a command is redirection to standard input. ``>`` with
a path after a command is redirection to standard output::

  grep -v "^#" < /etc/rc.conf > settings

If you hope to write both of stdout and stderr to a file, you can use ``=>``::

  foo => foo.log

Process Status
--------------

``$?`` is a status of last executed process.

Command Expression
------------------

Enclosing a pipeline (including single command) with ``$(`` and ``)`` is a
command expression. This will be a string of stdout of specified process::

  foo = $(echo "Hello, world!")

``foo`` will be ``"Hello, world!"``.

.. vim: tabstop=2 shiftwidth=2 expandtab softtabstop=2 filetype=rst
