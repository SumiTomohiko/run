
Script Language run
*******************

Objective
=========

An objective of run is replacing schell scripts.

Samples
=======

Hello, world!
-------------

::

  puts("Hello, world!")

Pipeline
--------

::

  ls -t | head

Communication between two processes, ``ping`` and ``pong``
----------------------------------------------------------

::

  ./ping <-> ./pong

Iterate all files under the current directory
---------------------------------------------

::

  every * as f
    puts(f)
  end

Features
========

* External command executing with easy syntax
* Powerfull redirecting
* Basic Types

  * ``Nil``
  * ``Bool``
  * ``Int``
  * ``Float``
  * ``String``
  * ``Array``
  * ``Dictionary`` (Hash)
  * ``Function`` / ``UserFunction``
  * ``Method``
  * ``ProcessStatus``
  * Some exceptions

* Structured programming with ``if``, ``while``, ``iterate`` and ``every``
  statements
* Arithmetic operations
* UTF-8 (only) acceptable
* Written in `Objective Caml <http://caml.inria.fr/ocaml/index.en.html>`_
* :doc:`copying`

Documentations
==============

* :doc:`tutorial`
* :doc:`install`
* :doc:`parsing`

Author
======

An author of run is `Ranko Kadonote <http://neko-daisuki.ddo.jp/~SumiTomohiko/from-python-import-fun/index.html>`_.

.. vim: tabstop=2 shiftwidth=2 expandtab softtabstop=2 filetype=rst
