.. _tracer_state:

==========================================
What Tracer Tendencies will MARBL Compute?
==========================================

This is an important question, because the GCM will need to provide the current state for each tracer at each timestep (including initial conditions at the beginning of the run).
MARBL provides a stand-alone test in ``$MARBL/tests/regression_tests/requested_tracers`` that shows how the MARBL library passes this information to the GCM.
For example, running

::

  $ ./requested_tracers.py

Provides a list of the tracers in the base ecosystem module. The test output is below:

.. block comes from output of requested_tracers.py
.. code-block:: none

  -----------------
  Requested tracers
  -----------------

  1. PO4
  2. NO3
  3. SiO3
  4. NH4
  5. Fe
  6. Lig
  7. O2
  8. DIC
  9. DIC_ALT_CO2
  10. ALK
  11. ALK_ALT_CO2
  12. DOC
  13. DON
  14. DOP
  15. DOPr
  16. DONr
  17. DOCr
  18. zooC
  19. spChl
  20. spC
  21. spP
  22. spFe
  23. spCaCO3
  24. diatChl
  25. diatC
  26. diatP
  27. diatFe
  28. diatSi
  29. diazChl
  30. diazC
  31. diazP
  32. diazFe

--------------------------------------------------
Tracer Metadata Available from the MARBL Interface
--------------------------------------------------

The details are found in ``$MARBL/tests/driver_src/marbl.F90``:

.. block comes from tests/driver_src/marbl.F90
.. code-block:: fortran

  call driver_status_log%log_header('Requested tracers', subname)
  do n=1, size(marbl_instance%tracer_metadata)
    write(log_message, "(I0, 2A)") n, '. ',                               &
      trim(marbl_instance%tracer_metadata(n)%short_name)
    call driver_status_log%log_noerror(log_message, subname)
  end do

The ``marbl_interface_class`` contains an object ``tracer_metadata``, the length of which is equal to the number of tracers MARBL is computing tendencies of.
The ``tracer_metadata_type`` contains metadata for each tracer:

.. block comes from marbl_interface_public_types
.. code-block:: fortran

  type, public :: marbl_tracer_metadata_type
     character(len=char_len) :: short_name
     character(len=char_len) :: long_name
     character(len=char_len) :: units
     character(len=char_len) :: tend_units
     character(len=char_len) :: flux_units
     logical                 :: lfull_depth_tavg
     character(len=char_len) :: tracer_module_name
  end type marbl_tracer_metadata_type

The ``short_name`` and ``long_name`` are both unique to the tracer, and either can be used to inform the GCM which tracer each index refers to.

-----------------------------------------
Example: Accessing Tracer Metadata in POP
-----------------------------------------

Details are on the :ref:`implementation <pop_tracers>` page
