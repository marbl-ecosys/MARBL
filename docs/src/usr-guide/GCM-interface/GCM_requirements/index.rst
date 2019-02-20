.. _GCM_requirements:

=======================
Requirements on the GCM
=======================

After MARBL has been setup, the GCM will need to ensure that it can provide MARBL with all the data MARBL has requested and run any computations that MARBL is not capable of.

---------------------
Provide data to MARBL
---------------------

MARBL will need the following from the GCM:

#. :ref:`Tracer state <tracer_state>` (including initial state)
#. :ref:`Specific forcing fields <forcing_fields>`
#. :ref:`Saved state from the previous timestep <saved_state>`

---------------------------
Computations across columns
---------------------------

If running with ``ladjust_bury_coeff = .true.`` then MARBL will ask the GCM to provide results from the two functions mentioned below.
Note that the typical runcase sets ``ladjust_bury_coeff = .false.``, so for first-time implementations it is okay to skip this section.

#. :ref:`Global sums <global_sums>` of data MARBL computes column-by-column
#. :ref:`Running means <running_means>` of fields (eventually MARBL will `compute these internally <https://github.com/marbl-ecosys/MARBL/issues/77>`_ and use saved state to maintain the mean)


.. toctree::
  :hidden:

  tracer_state.rst
  forcing_fields.rst
  saved_state.rst
  global_sums.rst
  running_means.rst
