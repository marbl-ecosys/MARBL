module marbl_interface_constants
  ! constants that need to be shared between the marbl public api and marbl private data.

  ! This module should hav NO dependencies on other modules other than marbl_kinds?

  implicit none

  private
  
  ! string sizes
  integer, public, parameter :: marbl_str_length = 1024

end module marbl_interface_constants
