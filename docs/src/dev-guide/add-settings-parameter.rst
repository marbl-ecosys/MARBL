.. _add-settings-parameter:

========================
Adding a MARBL Parameter
========================

This is a five step process.
There are three changes to make in the Fortran code, all of which are made in ``marbl_settings_mod.F90`` [*]_.
There are also two steps to make sure the parameter is picked up by the python tools to allow non-default values to be set.

Settings parameters are sorted into four categories that are processed in the following order:

.. code-block:: none

  general_parms
  PFT_counts
  PFT_derived_types
  tracer_dependent

This is necessary because the general parameter settings may affect the number of PFTs being modeled,
which changes the dimensions of the PFT derived types, which may affect the tracer count.
During initialization, parameters will be defined and the default values will be set for each category.
The defaults will be overwritten if the GCM called ``marbl_instance%put_setting()`` before calling ``init()``.

For this example, we follow the minimum O2 needed for prodution and consumption parameter, which is ``parm_o2_min`` in the code.
``parm_o2_min`` is a general parameter.

.. [*] The only exception is for parameters dealing with PFTs, those are in ``marbl_pft_mod.F90``.

----------------------------------------
Step 1. Create new module-level variable
----------------------------------------

All parameter settings are module variables in ``marbl_settings_mod.F90``.
Further, all subroutines and module variables are public in this module.

.. block comes from marbl_settings_mod
.. code-block:: fortran

  real(kind=r8), target :: &
  real(r8), target :: &
       Jint_Ctot_thres_molpm2pyr,  & ! MARBL will abort if abs(Jint_Ctot) exceeds this threshold
       .
       .
       .
       parm_Fe_bioavail,           & ! fraction of Fe flux that is bioavailable
       parm_o2_min,                & ! min O2 needed for prod & consump. (nmol/cm^3)
       parm_o2_min_delta,          & ! width of min O2 range (nmol/cm^3)
       .
       .
       .

.. note::
  The ``target`` attribute is necessary because MARBL's internal parameter registry makes use of pointers.

-------------------------------------------
Step 2. Add the parameter to MARBL registry
-------------------------------------------

This registry allows the parameter to be both set by and returned to the GCM.
Parameters in each of the four possible categories are defined separately from each other, in one of the following subroutines:

.. block comes from marbl_settings_mod
.. code-block:: fortran

  subroutine marbl_settings_define_general_parms(this, marbl_status_log)
  subroutine marbl_settings_define_PFT_counts(this, marbl_status_log)
  subroutine marbl_settings_define_PFT_derived_types(this, marbl_status_log)
  subroutine marbl_settings_define_tracer_dependent(this, marbl_status_log)

``parm_o2_min`` is registered in ``marbl_settings_define_general_parms``:

.. block comes from marbl_settings_mod
.. code-block:: fortran

  subroutine marbl_settings_define_general_parms(this, marbl_status_log)
  .
  .
  .
    sname     = 'parm_o2_min'
    lname     = 'Minimum O2 needed for production and consumption'
    units     = 'nmol/cm^3'
    datatype  = 'real'
    rptr      => parm_o2_min
    call this%add_var(sname, lname, units, datatype, category,       &
                        marbl_status_log, rptr=rptr)
    call check_and_log_add_var_error(marbl_status_log, sname, subname, labort_marbl_loc)

-------------------------
Step 3. Set default value
-------------------------

MARBL convention is to have a reasonable default value defined in case the variable is not changed via an input settings file.
Defaults for each of the four categories are set separately from each other, immediately after parameters for that category are defined.
The subroutines for setting defaults are

.. block comes from marbl_settings_mod
.. code-block:: fortran

  subroutine marbl_settings_set_defaults_general_parms()
  subroutine marbl_settings_set_defaults_PFT_counts(marbl_status_log)
  subroutine marbl_settings_set_defaults_PFT_derived_types(marbl_status_log)
  subroutine marbl_settings_set_defaults_tracer_dependent(marbl_status_log)


``parm_o2_min`` is set in ``marbl_settings_set_defaults_general_parms``:

.. block comes from marbl_settings_mod
.. code-block:: fortran

  subroutine marbl_settings_set_defaults_general_parms()
    .
    .
    .
    parm_Fe_bioavail              = 1.0_r8          ! CESM USERS - DO NOT CHANGE HERE! POP calls put_setting() for this var, see CESM NOTE above
    parm_o2_min                   = 5.0_r8          ! CESM USERS - DO NOT CHANGE HERE! POP calls put_setting() for this var, see CESM NOTE above
    parm_o2_min_delta             = 5.0_r8          ! CESM USERS - DO NOT CHANGE HERE! POP calls put_setting() for this var, see CESM NOTE above

--------------------------------------
Step 4. Update the settings YAML files
--------------------------------------

We use a YAML file to provide an easy-to-edit and human-readable text file containing a list of all parameters and their default values.
On the ``development`` branch, make changes to ``defaults/settings_latest.yaml``.
Release branches may only offer specific versions of this file, such as ``defaults/settings_cesm2.1.yaml``.

.. block comes from settings_latest.yaml
.. code-block:: yaml

  # ABOUT THIS FILE
  # ---------------
  # MARBL users can change settings values for runtime-configurable variables via a settings
  # input file. MARBL provides a python script that can generate an input file by reading a
  # JSON file containing the configurable variables and default values, but JSON does not allow
  # comments in the file format so the workflow is to edit this YAML file and then generate
  # the JSON file via $MARBL/MARBL_tools/yaml_to_json.py
  #
  # Parameters in MARBL are divided into four different stages, based on the order in which they are set
  # 1. General Parameters: variables that have no dependencies on other stages
  #    (note that init_bury_coeff_opt is alone in general_parms2 because it depends on ladjust_bury_coeff)
  # 2. PFT Counts: variables that can not be set until after PFT_defaults (in General Parameters) is known
  # 3. PFT Derived Types: variables that can not be set until PFT Counts are known
  #                       (autotroph_cnt, zooplankton_cnt, and max_grazer_prey_cnt)
  # 4. Post-Tracer: variables that can not be set until the tracer count is known
  #                 (tracer count depends on PFT Derived Types)
  #
  # All variables need to provide the following metadata:
  # 1. longname: a description of the variable
  # 2. subcategory: when writing parameters to the log, MARBL will group variables by subcategory
  # 3. units: physical units (use "unitless" for pure numbers and "non-numeric" for strings / logicals)
  # 4. datatype: integer, real, logical, or string
  # 5. default_value: Value to use unless overwritten by the MARBL input file
  #       NOTE: some parameters provide different default values for different configurations;
  #             e.g. in CESM, the value of some parameters is resolution-dependent. In these
  #             cases, default_value should be a dictionary with a "default" key and then keys
  #             for whatever resolutions differ from the default.
  #
  #             Accepted keys:
  #                1. default
  #                2. CESM_x3
  #
  # There are also some optional metadata options:
  # 1. valid_values: only values that MARBL will accept (default_value must be in valid_values!)
  # 2. cannot change:
  # 3. must set:
  # 4. _append_to_config_keywords: if default values of variables processed later depend on the
  #                                value of another variable, then that variable needs to have
  #                                _append_to_config_keywords = True
  #
  .
  .
  .
  ################################################################################
  #                        Category 1: General Parameters                        #
  ################################################################################

  general_parms :
     .
     .
     .
     parm_o2_min :
        longname : Minimum O2 needed for production & consumption
        subcategory : 4. general parameters
        units : nmol/cm^3
        datatype : real
        default_value : 5.0

-------------------------------------
Step 5. Convert the YAML file to JSON
-------------------------------------

We prefer editing YAML files to editing JSON files because they are much easier to maintain (and allow user comments).
Unfortunately, python does not include a YAML parser in the default distributions.
Rather than require all users to install ``pyYAML``, we require that of MARBL developers and then ask them to convert the YAML files to JSON.
The ``MARBL_tools/yaml_to_json.py`` script is provided to do just that:

.. code-block:: none

  $ cd MARBL_tools
  $ ./yaml_to_json.py

The rest of the python scripts provided in the ``MARBL_tools/`` subdirectory rely on the JSON file rather than the YAML.
``MARBL_tools/MARBL_generate_settings_file.py`` will turn the JSON file into a list for the GCM to parse:

.. block comes from marbl.input
.. code-block:: none

  ! general parameters
  .
  .
  .
  parm_o2_min = 5.0
  parm_o2_min_delta = 5.0

It is then up to the GCM to read this text file and pass it line by line to ``marbl_instance%put_setting()``
