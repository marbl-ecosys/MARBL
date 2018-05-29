.. _add-namelist-parm:

===========================
Adding a Namelist Parameter
===========================

This is a 5 step process.
There are 4 changes to make in the Fortran code, all of which are made in ``marbl_parms.F90`` [*]_ (which may be renamed ``marbl_parms_mod.F90`` in the near future).
There is also the added step of adding the new namelist to the model namelist generation tool.
Currently this is handled by the GCM (we will use POP as an example) but soon MARBL will be able to generate ``marbl_in`` for GCMs that use MARBL namelists directly to set parameters.
Other GCMs will be able to use a ``put()`` call to change the parameter.

------------------
MARBL Code Changes
------------------

For this example, we follow the minimum O2 needed for prodution and consumption parameter, which is ``parm_o2_min`` in the code.

~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
Step 1. Create new module-level variable
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

All namelist parameters are module variables in ``marbl_parms.F90``.
Further, all subroutines and module variables are public in this module.

.. code-block:: fortran

  real(kind=r8), target :: &
       parm_Fe_bioavail,      & ! fraction of Fe flux that is bioavailable
       parm_o2_min,           & ! min O2 needed for prod & consump. (nmol/cm^3)
       parm_o2_min_delta,     & ! width of min O2 range (nmol/cm^3)
       parm_kappa_nitrif,     & ! nitrification inverse time constant (1/sec)
       .
       .
       .

.. note::
  The ``target`` attribute is necessary because MARBL's internal parameter registry makes use of pointers.

~~~~~~~~~~~~~~~~~~~~~~~~~
Step 2. Set default value
~~~~~~~~~~~~~~~~~~~~~~~~~

MARBL convention is to have a reasonable default value defined in case the variable is not included in ``&marbl_parms_nml``.

.. code-block:: fortran

  subroutine marbl_parms_set_defaults(km)
  .
  .
  .
    !-----------------------------------------------------------------------
    !  &marbl_parms_nml
    !-----------------------------------------------------------------------

    parm_Fe_bioavail         = 1.0_r8
    parm_o2_min              = 5.0_r8
    parm_o2_min_delta        = 5.0_r8
    parm_kappa_nitrif        = 0.06_r8 * dps  ! (= 1/( days))

~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
Step 3. Add the parameter to ``&marbl_parms_nml``
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

To allow the variable to be read by the namelist, it needs to be defined in the namelist.

.. code-block:: fortran

  subroutine marbl_parms_read_namelist(nl_buffer, marbl_status_log)
  .
  .
  .
    NAMELIST /marbl_parms_nml/ &
         parm_Fe_bioavail, &
         parm_o2_min, &
         parm_o2_min_delta, &
         parm_kappa_nitrif, &
         .
         .
         .

~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
Step 4. Add the parameter to MARBL registry
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

This registry allows the parameter to be both set by and returned to the GCM.

.. code-block:: fortran

  subroutine marbl_define_parameters(this, marbl_status_log)
  .
  .
  .
    sname     = 'parm_o2_min'
    lname     = 'Minimum O2 needed for production and consumption'
    units     = 'nmol/cm^3'
    datatype  = 'real'
    group     = 'marbl_parms_nml'
    rptr      => parm_o2_min
    call this%add_var(sname, lname, units, datatype, group,                 &
                        marbl_status_log, rptr=rptr)
    if (marbl_status_log%labort_marbl) then
      call log_add_var_error(marbl_status_log, sname, subname)
      return
    end if

To set the value of this parameter, make the following call between the ``init`` and ``complete_config_and_init`` stages of setup:

.. code-block:: fortran

  call marbl_instance%parameters%put('parm_o2_min', 5.0_r8, marbl_instance%StatusLog)

To pass this value to the GCM, make the following call anytime after ``complete_config_and_init``:

.. code-block:: fortran

  call marbl_instance%parameters%get('parm_o2_min', local_parm_o2_min, marbl_instance%StatusLog)

----------------
POP Code Changes
----------------

To add a new variable to the MARBL namelist, see the `POP FAQ <http://www.cesm.ucar.edu/models/cesm1.2/pop2/doc/faq/#dev_newnamelist>`_.
As mentioned previously, MARBL will soon have a tool for generating ``marbl_in`` itself, at which point this task will not require any changes to the GCM.

.. note::
  MARBL expects to read the namelist from a string rather than a file.
  The GCM needs to read the namelist from a file, store it in a string, and then ensure that every task has a copy of the string.
  This is a different workflow than POP, where the master task reads the namelist directly from a file and then broadcasts each parameter to all the other tasks.
  We use this method to avoid the need for multiple broadcasts -- in POP, the master task stores the namelists in a single string and broadcasts that string to all other tasks.
  From that point on, every task is setting its parameter values with a namelist read.

.. [*] The ``&marbl_config_nml`` namelist is modified via similar steps in ``marbl_config_mod.F90``.
