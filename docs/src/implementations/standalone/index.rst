.. _standalone_implementation:

===========================
The MARBL Standalone Driver
===========================

The MARBL stand alone driver currently exists solely as a way to test the MARBL code base.
Testing is broken into three categories:

#. Does the Fortran code :ref:`build <build_tests>` correctly? (This tests both the MARBL library and the test framework driver.)
#. :ref:`Unit testing <unit_tests>`: do specific subroutines return the correct value?
#. :ref:`Regression testing <regression_tests>`: do specific call sequences continue to return the same value?

.. toctree::
   :maxdepth: 1
   :caption: Contents:

   build_tests.rst
   unit_tests.rst
   regression_tests.rst