.. _build_tests:

==============================
Testing the MARBL Build System
==============================

There are two build test scripts in ``$MARBLROOT/tests/bld_tests``: ``bld_lib.py`` and ``bld_exe.py``.
The former just builds ``libmarbl.a`` while the latter also builds the standalone driver that runs the :ref:`unit tests <unit_tests>` and :ref:`regression tests <regression_tests>`.
For known super computers, these scripts build with all defined compilers.
For unknown computers, these scripts check to see what supported compilers are in ``$PATH`` and test with each of them.

.. code-block:: none

  $ ./bld_lib.py
  (bld_lib): No machine specified and [machine name] is not recognized
  (bld_lib): This test will assume you are not running on a supported cluster
  (bld_lib): Override with the --mach option if this is not correct
  (bld_lib): Found the following compilers in $PATH: ['gnu', 'pgi']
