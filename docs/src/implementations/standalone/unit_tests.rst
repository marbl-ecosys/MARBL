.. _unit_tests:

===================
Unit Tests in MARBL
===================

There are two subdirectories in ``$MARBLROOT/tests/unit_tests``: ``get_put/`` and ``utils_routines/``.
Each contains a python script that launches a test to check the correctness of a small piece of the MARBL code base.

-----------------------------------------------
Testing the ``get()`` and ``put()`` subroutines
-----------------------------------------------

This test is designed to ensure the variable names passed to ``marbl_instance%put()`` alter the correct variable in memory.
All logical variables are set to ``.true.``, string variables are set to ``'-1'``, and numeric variables are set to ``-1``.
Then values are checked via ``marbl_instance%get()`` -- any values that are not ``.true.`` or ``-1`` were not set properly.

This builds upon error checks in the MARBL code base itself.
Here is output from a failing test, where ``gQsi_min`` inadvertently points to ``gQsi_max`` in memory:

.. code-block:: none

  Beginning get_put test...
  MARBL ERROR (marbl_settings_mod:add_var): gQsi_min and gQsi_max both point to same variable in memory.
  MARBL ERROR (marbl_settings_mod:marbl_settings_define_general_parms): Error reported from this%add_var(gQsi_min)
  MARBL ERROR (marbl_init_mod:marbl_init_parameters_pre_tracers): Error reported from marbl_settings_define_general_parms()
  MARBL ERROR (marbl_interface:init): Error reported from marbl_init_parameters_pre_tracers
  MARBL ERROR (marbl_get_put_drv:test): Error reported from marbl_loc%init
  STOP 1
  (run_exe): ERROR in executable

Perhaps this unit test has been deprecated, as the above error appears when running any executable that initializes a MARBL instance.
The expected output from the test is:

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
  MARBL Init:       0.141 seconds
  MARBL surface_flux_compute:       0.000 seconds
  MARBL interior_tendency_compute:       0.000 seconds
  MARBL carbonate chemistry:       0.000 seconds

------------------------------------
Testing additional small subroutines
------------------------------------

There are two small utility functions in ``marbl_utils_mod`` that MARBL relies on,
and both of them are tested for correctness.

The first is ``marbl_utils_linear_root`` -- given two points in ``(x, y)`` space,
this function returns a linear root between the point if one exists.
It is used to compute the saturation depth for calcite and argonite.
The unit tests ensure that several conditions are handled correctly:

#. Finding a root when increasing from a negative value to a positive value
#. The same, but passing in the point with the larger ``x`` value first
#. Finding a root when decreasing from a positive value to a negative value
#. The same, but passing in the point with the larger ``x`` value first
#. Finding a root when the ``y`` value at the left endpoint is 0
   (should return the left endpoint ``x`` value)
#. Finding a root when the ``y`` value at the right endpoint is 0
   (should return the right endpoint ``x`` value)
#. Finding a root when the ``y`` value at both endpoints are 0
   (should return the right endpoint ``x`` value)
#. Determining that no root exists if both ``y`` values are positive
#. Determining that no root exists if both ``y`` values are negative

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

There second function, ``marbl_utils_str_to_substrs()``, takes a string and a delimiter as arguments and returns
an array of substrings where each substring ends before a delimiter and the next substring starts after.
This is useful when the settings file defines values for an array, such as over-riding the first five elements of ``tracer_restore_vars``:

.. code-block:: none

  tracer_restore_vars = '','','','',''

It is also useful for ignoring comments in the settings file, which use the ``!`` character. In both of these cases,
it is important to ensure that if the delimiter appears within a string itself then it is not treated as a delimiter.
The tests for this subroutine are:

#. If the delimiter is ``,`` then a string with no ``,`` returns a single substring equal to the original string
#. If the delimiter is ``,`` then a string with a ``,`` is broken into two substrings
#. If the delimiter is ``,`` then a ``,`` appearing between two ``'`` is ignored
#. If the delimiter is ``!`` then the first substring only contains text leading up to the first ``!``
#. If the delimiter is ``!`` then a ``!`` appearing between two ``'`` is ignored

.. code-block:: none

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

If more utility functions are added in the future, they will also be tested here.
Further, if we encounter an edge case where one of the functions misbehaves,
that edge case will be added to the tests after the function has been fixed.
