.. _working-on-docs:

Working on the documentation
============================

Here are guidelines for installing and using Sphinx to develop documentation.

Local environment
-----------------
The MARBL documentation is built in `Sphinx
<http://www.sphinx-doc.org>`_
from content written in reStructuredText.

The following extensions are required.

* `sphinxcontrib-bibtex <https://sphinxcontrib-bibtex.readthedocs.io>`_


Python must be available locally.

Miniconda is a nice tool for maintaining Python:
https://conda.io/miniconda.html

It's helpful to setup an environment. See `here
<https://conda.io/docs/using/envs.html>`_
for more on conda environments.

With conda installed, do the following ::
  
  $ conda create --name marbl-docs sphinx sphinx_rtd_theme
  $ source activate marbl-docs
  $ pip install sphinxcontrib-bibtex

This creates an environment call "marbl-docs" and installs the required extensions.

To deactivate the "marbl-docs" environment ::

  $ source deactivate marbl-docs

Documentation workflow
----------------------

Here's some notes on how to modify the documentation.


Do all development work on a branch
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

Checkout a new local branch using ::

  $ ../marbl> git checkout -b my_branch

to create a branch or omit the ``-b`` to checkout an existing branch.

Edit documentation source
~~~~~~~~~~~~~~~~~~~~~~~~~

Modify and/or add `reStructuredText
<http://www.sphinx-doc.org/en/stable/rest.html#rst-primer>`_
files.

The documentation has three major sections

==   ====================================  =======================
\    :ref:`Developer's guide<dev-guide>`   ../docs/src/dev-guide
\    :ref:`Scientific manual<sci-guide>`   ../docs/src/sci-guide
\    :ref:`User guide<usr-guide>`          ../docs/src/usr-guide          
==   ====================================  =======================

The file `index.html` in each of these directories includes the table of contents for each section; this file must be modified when new pages are added.

Begin each `rst` file with a label that is the same as the file name::

  .. _myfilename:

Note the position of the underscore and ending colon.
This enables referencing this page from elsewhere in the project using ::

  :ref:`Name of link<myfilename>`

Build the documentation
~~~~~~~~~~~~~~~~~~~~~~~

Once changes are complete, build from `src` using ::

  $ ../marbl/src> make html

The compiled documentation ends up in ``../marbl/docs/html``.
You can view the files there in a browser locally as you work.

Commit changes
~~~~~~~~~~~~~~

You can check the status of your modification using ::

  $ ../marbl> git status

When you are ready to commit ::
  
  $ ../marbl/docs> git add .
  $ ../marbl/docs> git commit -m 'message describing changes'

|
|

.. admonition:: reStructuredText resource

   The authoritative `reStructuredText User Documentation
   <http://docutils.sourceforge.net/rst.html>`_. 


