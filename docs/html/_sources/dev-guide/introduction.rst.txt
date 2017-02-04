.. _intro:

Introduction to MARBL framework
===============================

Tracer equation view of biogeochemistry
---------------------------------------

MARBL is designed to be a modular implementation of ocean biogoechemistry suitable for coupling to ocean general circulation models (OGCM).
In the OGCM context, the prognostic equation governing the evolution of an arbitrary tracer :math:`\chi` in the ocean is

.. math::
   :label: tracer
	   
   \begin{equation}
      \frac{\partial \chi}{\partial t} 
      + \nabla\cdot(\mathbf{u}\chi) 
      - \nabla\cdot\left(K\cdot \nabla\chi\right) = B_\chi(\mathbf{x})
   \end{equation}
   
where and :math:`B_\chi(\mathbf{x})` is the sum of sources minus sinks for :math:`\chi`, computed as a function of the model state vector, :math:`\mathbf{x}`.

The OGCM computes the lefthand-side of :eq:`tracer` (time tendency, advection, diffusion); MARBL's role is to compute the righthand-side (:math:`B_\chi(\mathbf{x})`).
MARBL returns this tendency to the OGCM, which steps :eq:`tracer` forward in time.
MARBL also computes air-sea fluxes for constituents like CO\ :sub:`2`; the OGCM is responsible for handling these in a manner consistent with its numerics.
MARBL also returns diagnostic output, including intermediate terms used in the computation of :math:`B_\chi(\mathbf{x})`, such as net primary productivity or grazing of phytoplankton.
The OGCM must compute diagnostics relating to the total tracer concentration and terms from the righthand-side of :eq:`tracer`.

Implementation overview
-----------------------

MARBL is compiled as a standalone library with no explicit dependencies on aspects of the OGCM code.
The OGCM includes a *MARBL driver*, which is responsible for all communication with MARBL making use of MARBL's interface layer.

MARBL is configured to operate on vertical columns.  The OGCM passes data into MARBL on these columns, including information describing the domain (i.e., `dz` or the vertical layer thickness, which may vary in time).

MARBL returns data on columns, which must then be remapped back to the OGCM's data format.

.. image:: ../_static/modular-design.png
   :align: center

Coding conventions
------------------

MARBL is written in Fortran.  
A few constructs commonly used in MARBL may be unfamiliar to scientific programmers.

Associate construct
^^^^^^^^^^^^^^^^^^^

The ``associate`` construct allows complex variables or expression to be denoted by a simple name or "alias."
The association between the name and the underlying variable is terminated at the end of the associate block.

For example:

.. code-block:: fortran

   do m=1,tracer_restore_cnt
    n = interior_forcing_ind%tracer_id(m)
    associate(restore_field => interior_forcings(restoring_inds(n))%field_1d, &
              inv_tau       =>  interior_forcings(inv_tau_inds(n))%field_1d)
      interior_restore(n,:) = (restore_field(1,:) - interior_tracers(n,:)) * inv_tau(1,:)
    end associate
  end do

In this case, ``inv_tau`` points to the more complicated expression ``interior_forcings(inv_tau_inds(n))%field_1d)``, and similarly for ``restore_field``.
The assocation is terminated at ``end associate``.


Object-oriented programming features
^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^
Object-oriented programming constructs permit the definition of classes that both contain data and methods which can perform operations on that data.

Here's a relatively simple example, the declaration of the ``marbl_domain_type``, which is a class used to bundle information about the OGCM domain.

.. code-block:: fortran
   
  type, public :: marbl_domain_type
     integer(int_kind)     :: num_PAR_subcols               ! number of PAR subcols
     integer(int_kind)     :: num_elements_surface_forcing  ! number of surface forcing columns
     integer(int_kind)     :: num_elements_interior_forcing ! number of interior forcing columns
     integer(int_kind)     :: km                            ! number of vertical grid cells
     integer(int_kind)     :: kmt                           ! index of ocean floor
     real(r8), allocatable :: zt(:)                         ! (km) vert dist from sfc to midpoint of layer
     real(r8), allocatable :: zw(:)                         ! (km) vert dist from sfc to bottom of layer
     real(r8), allocatable :: delta_z(:)                    ! (km) delta z - different values for partial bottom cells
     real(r8), allocatable :: dz(:)                         ! (km) delta z - same values for partial bottom cells
   contains
     procedure, public :: construct => marbl_domain_constructor
  end type marbl_domain_type

This type includes a constructor `method`, referenced to the subroutine ``marbl_domain_constructor``.

The subroutine looks like this:

.. code-block:: fortran
		
   subroutine marbl_domain_constructor(this, &
       num_levels, num_PAR_subcols, &
       num_elements_surface_forcing, num_elements_interior_forcing, &
       dz, zw, zt)

    class(marbl_domain_type), intent(inout) :: this
    integer (int_kind) , intent(in) :: num_levels
    integer (int_kind) , intent(in) :: num_PAR_subcols
    integer (int_kind) , intent(in) :: num_elements_surface_forcing
    integer (int_kind) , intent(in) :: num_elements_interior_forcing
    real (r8)          , intent(in) :: dz(num_levels) 
    real (r8)          , intent(in) :: zw(num_levels) 
    real (r8)          , intent(in) :: zt(num_levels) 

    integer :: k

    ! FIXME #24: remove dz from data type, use delta_z for all vertical depths
    allocate(this%dz(num_levels))
    allocate(this%delta_z(num_levels))
    allocate(this%zw(num_levels))
    allocate(this%zt(num_levels))

    this%km = num_levels
    this%num_PAR_subcols = num_PAR_subcols
    this%num_elements_surface_forcing = num_elements_surface_forcing
    this%num_elements_interior_forcing = num_elements_interior_forcing

    do k = 1, num_levels
       this%delta_z(k) = dz(k)
       this%dz(k)      = dz(k)
       this%zw(k)      = zw(k)
       this%zt(k)      = zt(k)
    end do

  end subroutine marbl_domain_constructor
  
One key thing to note here is the use of ``this``, the first argument to the subroutine.
In this case, ``this`` stands for the particular instance of an object of type ``marbl_domain_type``.
The subroutine is actually part of this object, which is essentially constructing itself.


	     

	   
	   
