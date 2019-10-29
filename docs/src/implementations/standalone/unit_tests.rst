.. _unit_tests:

===================
Unit Tests in MARBL
===================

There are two subdirectories in ``$MARBLROOT/tests/unit_tests``: ``get_put/`` and ``utils_routines/``.
Each contains a python script that launches a test to check the correctness of a small piece of the MARBL code base.

-----------------------------------------------
Testing the ``get()`` and ``put()`` subroutines
-----------------------------------------------

.. code-block:: none

  Beginning get_put test...
  Setting variables to .true. or -1 ...
  ... Done!
  Making sure variables are .true. or -1
  ... Done!

  -------------
  Timer summary
  -------------

  There are 4 timers being returned
  ----
  MARBL Init:       0.148 seconds
  MARBL surface_flux_compute:       0.000 seconds
  MARBL interior_tendency_compute:       0.000 seconds
  MARBL carbonate chemistry:       0.000 seconds

------------------------------------
Testing additional small subroutines
------------------------------------


.. code-block:: none

  -----------------
  Linear Root Tests
  -----------------

  PASS: Test 1 [linear root: root between (1.0, -1.0) and (2.0, 1.0)]
        Root at x = 1.5
  PASS: Test 2 [linear root: root between (2.0, 1.0) and (1.0, -1.0)]
        Root at x = 1.5
  PASS: Test 3 [linear root: root between (1.0, 1.0) and (2.0, -1.0)]
        Root at x = 1.5
  PASS: Test 4 [linear root: root between (2.0, -1.0) and (1.0, 1.0)]
        Root at x = 1.5
  PASS: Test 5 [linear root: root between (5.0, .0) and (7.0, 3.0)]
        Root at x = 5.0
  PASS: Test 6 [linear root: root between (5.0, 3.0) and (7.0, .0)]
        Root at x = 7.0
  PASS: Test 7 [linear root: root between (5.0, .0) and (7.0, .0)]
        Root at x = 7.0
  PASS: Test 8 (linear root: root between (1.0, 1.0) and (2.0, 3.0)]
        No root found
  PASS: Test 9 (linear root: root between (1.0, -1.0) and (2.0, -3.0)]
        No root found

  ** passed all linear root tests **

  --------------------------
  String -> Substrings Tests
  --------------------------

  PASS: Test 1 (str_to_substrs: .true.)
        substr(1): .true.
  PASS: Test 2 (str_to_substrs: 123, 456)
        substr(1): 123
        substr(2):  456
  PASS: Test 3 (str_to_substrs: 'ABC, DEF', 'GHI')
        substr(1): 'ABC, DEF'
        substr(2):  'GHI'

  ** passed all string to substrings tests **

  -----------------------
  Comment-Stripping Tests
  -----------------------

  PASS: Test 1 (strip_comments: ciso_on = .true.  ! Turn on ciso)
        string: ciso_on = .true.
  PASS: Test 2 (strip_comments: autotrophs(1)%lname='Small Phytoplankton!')
        string: autotrophs(1)%lname='Small Phytoplankton!'

  ** passed all comment stripping tests **

  All unit tests passed!
