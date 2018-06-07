.. _light-attenuation:

===============================
 Light attenuation formulation
===============================

Starting point is equation 6 of :cite:`Morel-2001`

.. math::
   :label: ze_chl_tot

   Z_e =
   \begin{cases}
   912.5 \left[ \mathrm{Chl_{tot}} \right]^{-0.839} & 10 < Z_e < 102 \\
   426.3 \left[ \mathrm{Chl_{tot}} \right]^{-0.547} & 102 < Z_e < 180
   \end{cases}

In this approximation, :math:`\left[ \mathrm{Chl_{tot}} \right]` is
:math:`\left[ \mathrm{Chl}(z) \right]` integrated from the surface to :math:`Z_e`,
the depth of the euphotic zone.

We will convert this formula to one based on :math:`\left[ \mathrm{Chl}(z) \right]`
averaged over the euphotic zone

.. math::
   :label: chl_tot_def

   \left[ \mathrm{Chl_{tot}} \right] =  Z_e \overline{\left[ \mathrm{Chl} \right]}

The crossover point :math:`Z_e=102` corresponds to
:math:`\left[ \mathrm{Chl_{tot}} \right]=13.65`, which is equivalent to
:math:`\overline{\left[ \mathrm{Chl} \right]}=0.1338`.

Substituting equation :eq:`chl_tot_def` into equation :eq:`ze_chl_tot`
and solving for :math:`Z_e` yields

.. math::
   :label: ze_chl_avg

   Z_e =
   \begin{cases}
     40.710 \overline{\left[ \mathrm{Chl} \right]}^{-0.4562} & \overline{\left[ \mathrm{Chl} \right]} > 0.1338 \\
     50.105 \overline{\left[ \mathrm{Chl} \right]}^{-0.3536} & \overline{\left[ \mathrm{Chl} \right]} < 0.1338
   \end{cases}

The euphotic zone depth is defined to be the depth where PAR is 1\% of its surface value
:cite:`Morel-1988`.

We denote the attenuation coefficient of PAR as :math:`K`, and its effective average over the
euphotic zone as :math:`\overline{K}`.

So we have

.. math::

   0.01 = e^{-Z_e \overline{K}}.

Solving for :math:`Z_e` yields

.. math::
   :label: Kbar_def

   Z_e = - \log 0.01 / \overline{K} = \log 100 / \overline{K}.

Substituting equation :eq:`Kbar_def` into equation :eq:`ze_chl_avg` and
solving for :math:`\overline{K}` yields

.. math::
   :label: Kbar_chl_avg

   \overline{K} =
   \begin{cases}
   0.1131 \overline{\left[ \mathrm{Chl} \right]}^{0.4562} & \overline{\left[ \mathrm{Chl} \right]} > 0.1338 \\
   0.0919 \overline{\left[ \mathrm{Chl} \right]}^{0.3536} & \overline{\left[ \mathrm{Chl} \right]} < 0.1338
   \end{cases}

In the model implementation, this equation relating :math:`\overline{K}` to
:math:`\overline{\left[ \mathrm{Chl} \right]}` is applied to each model layer.

The crossover point was recomputed to be where the curves
cross, yielding :math:`\overline{\left[ \mathrm{Chl} \right]}=0.13224`.

The units of :math:`K` in equation :eq:`Kbar_chl_avg` are 1/m.

Model units are cm, so the model implementation includes multiplication by 0.01.

.. only:: html

  .. rubric:: References

.. bibliography:: ../references.bib
  :filter: docname in docnames
