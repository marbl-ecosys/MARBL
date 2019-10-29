.. _standalone_implementation:

===========================
The MARBL Standalone Driver
===========================

The MARBL stand alone driver currently exists solely as a way to test the MARBL code base.
Testing is broken into three categories:

#. Does the Fortran code :ref:`build <build_tests>` correctly?
   We test both the MARBL library and the test framework driver.
   These tests can be found in ``$MARBLROOT/tests/bld_tests``.
#. :ref:`Unit testing <unit_tests>`: do specific subroutines return the correct value?
   These tests can be found in ``$MARBLROOT/tests/unit_tests``.
#. :ref:`Regression testing <regression_tests>`: do specific call sequences continue to return the same value?
   These tests can be found in ``$MARBLROOT/tests/regression_tests``.

All testing can be run via python scripts that import code from ``$MARBLROOT/tests/python_for_tests``.
This directory contains a class used to control how to build MARBL and what options should be available to the user.
It also maintains settings for running the standalone tests on a handful of super computers (loading proper modules, etc).

.. toctree::
   :maxdepth: 1
   :caption: Contents:

   build_tests.rst
   unit_tests.rst
   regression_tests.rst