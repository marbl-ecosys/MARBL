module marbl_kinds_mod

  implicit none

  public

  integer, parameter, public ::                 &
       char_len       = 384                    ,&
       char_len_long  = 512                    ,&
       log_kind       = kind(.true.)           ,&
       int_kind       = kind(1)                ,&
       i4             = selected_int_kind(6)   ,&
       i8             = selected_int_kind(13)  ,&
       r8             = selected_real_kind(13) ,&  
       r4             = selected_real_kind(6)  

end module marbl_kinds_mod
