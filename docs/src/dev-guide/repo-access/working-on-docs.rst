.. _working-on-docs:

============================
Working on the documentation
============================

Here are guidelines for installing and using Sphinx to develop documentation.

-----------------
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

With conda installed, do the following (the last command assumes you are in the root of your MARBL repository):

.. code-block:: none

  $ conda create --name marbl-docs pip
  $ conda activate marbl-docs
  (marbl-docs)$ pip install -r docs/py_requirements.txt

This creates an environment call "marbl-docs" and ensures that ``pip install`` commands are local to the environment rather than global.

To deactivate the "marbl-docs" environment run

.. code-block:: none

  (marbl-docs)$ conda deactivate

----------------------
Documentation workflow
----------------------

Here's some notes on how to modify the documentation.


~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
Do all development work on a branch
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

Checkout a new local branch using

.. code-block:: none

  (marbl-docs)$ git checkout -b my_branch

to create a branch or omit the ``-b`` to checkout an existing branch.

~~~~~~~~~~~~~~~~~~~~~~~~~
Edit documentation source
~~~~~~~~~~~~~~~~~~~~~~~~~

Modify and/or add `reStructuredText
<http://www.sphinx-doc.org/en/stable/rest.html#rst-primer>`_
files.

The documentation has three major sections

==   ====================================  ============================
\    :ref:`Developer's guide<dev-guide>`   ``MARBL/docs/src/dev-guide``
\    :ref:`Scientific manual<sci-guide>`   ``MARBL/docs/src/sci-guide``
\    :ref:`User guide<usr-guide>`          ``MARBL/docs/src/usr-guide``
==   ====================================  ============================

The file ``index.html`` in each of these directories includes the table of contents for each section; this file must be modified when new pages are added.

Begin each ``rst`` file with a label that is the same as the file name

.. code-block:: none

  .. _myfilename:

Note the position of the underscore and ending colon.
This enables referencing this page from elsewhere in the project using

.. code-block:: none

  :ref:`Name of link<myfilename>`

~~~~~~~~~~~~~~~~~~~~~~~
Build the documentation
~~~~~~~~~~~~~~~~~~~~~~~

Once changes are complete, build from `src` using

.. code-block:: none

  (marbl-docs)[docs/src]$ make clean html

The compiled documentation ends up in ``MARBL/docs/html``.
You can view the files there in a browser locally as you work.

~~~~~~~~~~~~~~
Commit changes
~~~~~~~~~~~~~~

You can check the status of your modification using

.. code-block:: none

  [MARBL]$ git status

When you are ready to commit

.. code-block:: none

  [MARBL/docs]$ git add .
  [MARBL/docs]$ git commit -m 'message describing changes'

---------------------------
Headers in ReStructuredText
---------------------------

reStructuredText parses special characters to create titles, subtitles, and other headers in a non-unique way, which is to say that there are multiple ways to produce the same set of headers.
Any non-alphanumeric [7-bit] character repeated for the entire length of the line above it will turn the line above it into a header.
If you desire, you can also overline the header text with the same string.
The order you use the special characters must be consistent within a file (the first character choice produces a title, the second character choice produces a subtitle, and so on).
For example, the following two blocks of code translate into the same page:

.. code-block:: none

  Title
  -----

  Subtitle
  ~~~~~~~~

  Subsubtitle
  ===========

and

.. code-block:: none

  Title
  +++++

  ^^^^^^^^
  Subtitle
  ^^^^^^^^

  Subsubtitle
  ___________

For consistency, MARBL documentation should use the same pattern across all files.
(Again, this is not a requirement of reStructuredText.)
The preferred pattern is

.. code-block:: none

  =====
  Title
  =====

  --------
  Subtitle
  --------

  ~~~~~~~~~~~
  Subsubtitle
  ~~~~~~~~~~~

Note that this convention is entirely arbitrary, but should make reading ``.rst`` files a little easier.
If you find a need for a Subsubsubtitle, choose your favorite special character that is not already in use and then edit this page accordingly.

.. admonition:: reStructuredText resource

   The authoritative `reStructuredText User Documentation
   <http://docutils.sourceforge.net/rst.html>`_.
