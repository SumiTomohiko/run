
About Parsing
=============

run accepts command executing statements as well as ``while`` statements and so
on. This function has one problem. run must determine which are commands, and
which are not commands. For example,

::

  foo = 42

may be an assignment expression, but also a command calling of ``foo`` with two
arguments such as ``=`` and ``42``. run decides this with the following steps.

1. If one line is acceptable as a non-command, run decides that it is not
   a command.
2. A first token of a line is one of ``break``, ``def``, ``elif``, ``else``,
   ``end``, ``every``, ``if``, ``next``, ``return`` and ``while``, run decides
   that it is not a command.
3. Any other lines are commands.

So run can know that ``foo = 42`` is an expression. But sometimes run cannot
accept any commands. For example,

::

  puts(42)

Even if you gave this line as a command executing of a command ``puts(42)``, run
accepts this as a function call of ``puts`` with one argument ``42``.

run's grammar does not allow expressions as statements but assignments and
callings, so

::

  test

is a command executing of ``test``.

.. vim: tabstop=2 shiftwidth=2 expandtab softtabstop=2 filetype=rst
