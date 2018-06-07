.. _sci-guide:

==============================
MARBL scientific documentation
==============================

The default MARBL configuration invokes the Biogeochemical Elemental Cycling (BEC) model  :cite:`Moore-2004`. which is an ecosystem/biogeochemistry model designed to run within the ocean circulation component of CESM.

The ecosystem includes multiple phytoplankton functional groups (diatoms, diazotrophs, small phytoplankton, and coccolithophores) and multiple potentially growth limiting nutrients (nitrate, ammonium, phosphate, silicate, and iron) :cite:`Moore-2002,Moore-2004`.
There is one zooplankton group, dissolved organic material (semi-labile), sinking particulate pools and explicit simulation of the biogeochemical cycling of key elements (C, N, P, Fe, Si, O, plus alkalinity) :cite:`Moore-2004`.
The ecosystem component is coupled with a carbonate chemistry module based on the Ocean Carbon Model Intercomparison Project (OCMIP) :cite:`Doney-2009` allowing dynamic computation of surface ocean pCO2 and air-sea CO2 flux.

The model allows for water column denitrication, whereby nitrate is consumed during remineralization in place of O2 once ambient O2 concentrations fall below 4 micro-molar :cite:`Moore-2007`.
Photoadaptation is calculated as a variable phytoplankton ratio of chlorophyll to nitrogen based on Geider et al. :cite:`Geider-1998`.
The model allows for variable Fe/C and Si/C ratios with an optimum and minimum value prescribed.
As ambient Fe (or Si for diatoms) concentrations decline the phytoplankton lower their cellular quotas.
Phytoplankton N/P ratios are fixed at the Redfield value of 16, but the diazotroph group has a higher N/P atomic ratio of 50 (see detailed description of the model in Moore et al., 2002 :cite:`Moore-2002`, and Moore et al., 2004 :cite:`Moore-2004`).
Thus, community N/P uptake varies with the phytoplankton community composition.

The ecosystem model results have been compared extensively against in situ data (e.g., JGOFS time series stations) and SeaWiFS satellite ocean color observations in a global mixed layer only variant and coupled with a full-depth, global 3-D general circulation model :cite:`Moore-2002,Moore-2004,Doney-2009`.
In both cases, the simulated output is in generally good agreement with bulk ecosystem observations (e.g., total biomass, productivity, nutrients, export) across diverse ecosystems that include both macro-nutrient and iron-limited regimes as well as very different physical environments from high latitude sites to the mid-ocean gyres.
The model also incorporates the work of Moore and Braucher :cite:`Moore-2008`, who incorporated an improved sedimentary iron source and scavenging parameterization, greatly improving simulated iron fields relative to observations, and the work of Krishnamurthy et al. :cite:`Krishnamurthy-2007`, who describe the impact of atmospheric deposition of nitrogen.

.. note::
  This description is still under development.

.. toctree::
   :maxdepth: 1
   :caption: Contents:

   phytoplankton-growth.rst
   light-attenuation.rst

.. only:: html

  .. rubric:: References

.. bibliography:: ../references.bib
   :filter: docname in docnames
