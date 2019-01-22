.. _shutdown:

========
Shutdown
========

The shutdown stage is where MARBL deallocates memory (including memory allocated inside of derived types, such as the diagnostic indexing types).
The only object still accessible after shutdown is ``marbl_interface%timer_summary``, so GCMs can still access performance timers.

The ``shutdown()`` interface
----------------------------

.. block comes from marbl_interface
.. code-block:: fortran

  subroutine shutdown(this)

    class(marbl_interface_class), intent(inout) :: this

No additional arguments are needed for calls to ``marbl_instance%shutdown()``.