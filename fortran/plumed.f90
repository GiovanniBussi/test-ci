! vim:ft=fortran



module plumed_module
  use iso_c_binding
  implicit none

  ! names are private by default
  private

  ! only these names are public
  public :: plumed_f_create
  public :: plumed_f_create_dlopen
  public :: plumed_f_create_reference
  public :: plumed_f_create_invalid
  public :: plumed_f_cmd
  public :: plumed_f_finalize
  public :: plumed_f_installed
  public :: plumed_f_valid
  public :: plumed_f_use_count
  public :: plumed_f_global
  public :: plumed_f_ginitialized
  public :: plumed_f_gcreate
  public :: plumed_f_gcmd
  public :: plumed_f_gfinalize
  public :: plumed_f_gvalid

  ! used to enforce keyword-only arguments
  type :: dummy_type
  end type dummy_type

  ! this type maps to the struct plumed defined in src/wrapper/Plumed.h
  type, bind(C) :: cplumed
    type(c_ptr) :: ptr
  end type cplumed

  ! this type maps to the struct plumed_nothrow_handler defined in src/wrapper/Plumed.h
  type, bind(C) :: cplumed_nothrow_handler
    type(c_ptr)    :: ptr
    type(c_funptr) :: handler
  end type cplumed_nothrow_handler

  ! this function is used to translate 32-char to c identifiers, only used internally
  interface
    function plumed_f2c(c) bind(C)
      import
      character(kind=c_char), intent(in) :: c(32)
      type(cplumed) :: plumed_f2c
    end function plumed_f2c
  end interface

  ! now there are interfaces to the classic Fortran functions

  interface
    subroutine plumed_f_create(c) bind(C)
      import
      character(kind=c_char), intent(out) :: c(32)
    end subroutine plumed_f_create
  end interface

  interface
    subroutine plumed_f_create_dlopen(path,c) bind(C)
      import
      character(kind=c_char), intent(in)  :: path(*)
      character(kind=c_char), intent(out) :: c(32)
    end subroutine plumed_f_create_dlopen
  end interface

  interface
    subroutine plumed_f_create_reference(r,c) bind(C)
      import
      character(kind=c_char), intent(in)  :: r(32)
      character(kind=c_char), intent(out) :: c(32)
    end subroutine plumed_f_create_reference
  end interface

  interface
    subroutine plumed_f_create_invalid(c) bind(C)
      import
      character(kind=c_char), intent(out) :: c(32)
    end subroutine plumed_f_create_invalid
  end interface

  interface
    subroutine plumed_f_finalize(c) bind(C)
      import
      character(kind=c_char), intent(in) :: c(32)
    end subroutine plumed_f_finalize
  end interface

  interface
    subroutine plumed_f_installed(i) bind(C)
      import
      integer(kind=c_int), intent(out) :: i
    end subroutine plumed_f_installed
  end interface

  interface
    subroutine plumed_f_valid(c,i) bind(C)
      import
      character(kind=c_char), intent(in) :: c(32)
      integer(kind=c_int),    intent(out) :: i
    end subroutine plumed_f_valid
  end interface

  interface
    subroutine plumed_f_use_count(c,i) bind(C)
      import
      character(kind=c_char), intent(in)  :: c(32)
      integer(kind=c_int),    intent(out) :: i
    end subroutine plumed_f_use_count
  end interface

  interface
    subroutine plumed_f_global(c) bind(C)
      import
      character(kind=c_char), intent(out) :: c(32)
    end subroutine plumed_f_global
  end interface 

  interface
    subroutine plumed_f_ginitialized(i) bind(C)
      import
      integer(kind=c_int), intent(out) :: i
    end subroutine plumed_f_ginitialized
  end interface

  interface
    subroutine plumed_f_gcreate() bind(C)
    end subroutine plumed_f_gcreate
  end interface
  
  interface
    subroutine plumed_f_gfinalize() bind(C)
    end subroutine plumed_f_gfinalize
  end interface

  interface
    subroutine plumed_f_gvalid(i) bind(C)
      import
      integer(kind=c_int),    intent(out) :: i
    end subroutine plumed_f_gvalid
  end interface

  ! now there are the interfaces to the special C function handling type checks and exceptions

  interface
    subroutine plumed_f_cmd_safe_nothrow_ptr(p,key,val,pass_shape,flags,nothrow) bind(C)
      import
      type(cplumed),                  value :: p
      character(kind=c_char), intent(in)    :: key(*)
      type(c_ptr),                    value :: val
      integer(kind=c_size_t)                :: pass_shape(*)
      integer(kind=c_size_t),         value :: flags
      type(cplumed_nothrow_handler),  value :: nothrow
    end subroutine plumed_f_cmd_safe_nothrow_ptr
  end interface

  interface
    subroutine plumed_f_cmd_safe_nothrow_char(p,key,val,pass_shape,flags,nothrow) bind(C)
      import
      type(cplumed),                  value :: p
      character(kind=c_char), intent(in)    :: key(*)
      character(kind=c_char)                :: val(*)
      integer(kind=c_size_t)                :: pass_shape(*)
      integer(kind=c_size_t),         value :: flags
      type(cplumed_nothrow_handler),  value :: nothrow
    end subroutine plumed_f_cmd_safe_nothrow_char
  end interface

  interface
    subroutine plumed_f_cmd_safe_nothrow_int_scalar(p,key,val,pass_shape,flags,nothrow) bind(C)
      import
      type(cplumed),                  value :: p
      character(kind=c_char), intent(in)    :: key(*)
      integer(kind=c_int)                           :: val
      integer(kind=c_size_t)                :: pass_shape(*)
      integer(kind=c_size_t),         value :: flags
      type(cplumed_nothrow_handler),  value :: nothrow
    end subroutine plumed_f_cmd_safe_nothrow_int_scalar
  end interface
  interface
    subroutine plumed_f_cmd_safe_nothrow_int(p,key,val,pass_shape,flags,nothrow) bind(C)
      import
      type(cplumed),                  value :: p
      character(kind=c_char), intent(in)    :: key(*)
      integer(kind=c_int)                           :: val(*)
      integer(kind=c_size_t)                :: pass_shape(*)
      integer(kind=c_size_t),         value :: flags
      type(cplumed_nothrow_handler),  value :: nothrow
    end subroutine plumed_f_cmd_safe_nothrow_int
  end interface
  interface
    subroutine plumed_f_cmd_safe_nothrow_short_scalar(p,key,val,pass_shape,flags,nothrow) bind(C)
      import
      type(cplumed),                  value :: p
      character(kind=c_char), intent(in)    :: key(*)
      integer(kind=c_short)                           :: val
      integer(kind=c_size_t)                :: pass_shape(*)
      integer(kind=c_size_t),         value :: flags
      type(cplumed_nothrow_handler),  value :: nothrow
    end subroutine plumed_f_cmd_safe_nothrow_short_scalar
  end interface
  interface
    subroutine plumed_f_cmd_safe_nothrow_short(p,key,val,pass_shape,flags,nothrow) bind(C)
      import
      type(cplumed),                  value :: p
      character(kind=c_char), intent(in)    :: key(*)
      integer(kind=c_short)                           :: val(*)
      integer(kind=c_size_t)                :: pass_shape(*)
      integer(kind=c_size_t),         value :: flags
      type(cplumed_nothrow_handler),  value :: nothrow
    end subroutine plumed_f_cmd_safe_nothrow_short
  end interface
  interface
    subroutine plumed_f_cmd_safe_nothrow_long_scalar(p,key,val,pass_shape,flags,nothrow) bind(C)
      import
      type(cplumed),                  value :: p
      character(kind=c_char), intent(in)    :: key(*)
      integer(kind=c_long)                           :: val
      integer(kind=c_size_t)                :: pass_shape(*)
      integer(kind=c_size_t),         value :: flags
      type(cplumed_nothrow_handler),  value :: nothrow
    end subroutine plumed_f_cmd_safe_nothrow_long_scalar
  end interface
  interface
    subroutine plumed_f_cmd_safe_nothrow_long(p,key,val,pass_shape,flags,nothrow) bind(C)
      import
      type(cplumed),                  value :: p
      character(kind=c_char), intent(in)    :: key(*)
      integer(kind=c_long)                           :: val(*)
      integer(kind=c_size_t)                :: pass_shape(*)
      integer(kind=c_size_t),         value :: flags
      type(cplumed_nothrow_handler),  value :: nothrow
    end subroutine plumed_f_cmd_safe_nothrow_long
  end interface
  interface
    subroutine plumed_f_cmd_safe_nothrow_float_scalar(p,key,val,pass_shape,flags,nothrow) bind(C)
      import
      type(cplumed),                  value :: p
      character(kind=c_char), intent(in)    :: key(*)
      real(kind=c_float)                           :: val
      integer(kind=c_size_t)                :: pass_shape(*)
      integer(kind=c_size_t),         value :: flags
      type(cplumed_nothrow_handler),  value :: nothrow
    end subroutine plumed_f_cmd_safe_nothrow_float_scalar
  end interface
  interface
    subroutine plumed_f_cmd_safe_nothrow_float(p,key,val,pass_shape,flags,nothrow) bind(C)
      import
      type(cplumed),                  value :: p
      character(kind=c_char), intent(in)    :: key(*)
      real(kind=c_float)                           :: val(*)
      integer(kind=c_size_t)                :: pass_shape(*)
      integer(kind=c_size_t),         value :: flags
      type(cplumed_nothrow_handler),  value :: nothrow
    end subroutine plumed_f_cmd_safe_nothrow_float
  end interface
  interface
    subroutine plumed_f_cmd_safe_nothrow_double_scalar(p,key,val,pass_shape,flags,nothrow) bind(C)
      import
      type(cplumed),                  value :: p
      character(kind=c_char), intent(in)    :: key(*)
      real(kind=c_double)                           :: val
      integer(kind=c_size_t)                :: pass_shape(*)
      integer(kind=c_size_t),         value :: flags
      type(cplumed_nothrow_handler),  value :: nothrow
    end subroutine plumed_f_cmd_safe_nothrow_double_scalar
  end interface
  interface
    subroutine plumed_f_cmd_safe_nothrow_double(p,key,val,pass_shape,flags,nothrow) bind(C)
      import
      type(cplumed),                  value :: p
      character(kind=c_char), intent(in)    :: key(*)
      real(kind=c_double)                           :: val(*)
      integer(kind=c_size_t)                :: pass_shape(*)
      integer(kind=c_size_t),         value :: flags
      type(cplumed_nothrow_handler),  value :: nothrow
    end subroutine plumed_f_cmd_safe_nothrow_double
  end interface
  interface
    subroutine plumed_f_cmd_safe_nothrow_long_double_scalar(p,key,val,pass_shape,flags,nothrow) bind(C)
      import
      type(cplumed),                  value :: p
      character(kind=c_char), intent(in)    :: key(*)
      real(kind=c_long_double)                           :: val
      integer(kind=c_size_t)                :: pass_shape(*)
      integer(kind=c_size_t),         value :: flags
      type(cplumed_nothrow_handler),  value :: nothrow
    end subroutine plumed_f_cmd_safe_nothrow_long_double_scalar
  end interface
  interface
    subroutine plumed_f_cmd_safe_nothrow_long_double(p,key,val,pass_shape,flags,nothrow) bind(C)
      import
      type(cplumed),                  value :: p
      character(kind=c_char), intent(in)    :: key(*)
      real(kind=c_long_double)                           :: val(*)
      integer(kind=c_size_t)                :: pass_shape(*)
      integer(kind=c_size_t),         value :: flags
      type(cplumed_nothrow_handler),  value :: nothrow
    end subroutine plumed_f_cmd_safe_nothrow_long_double
  end interface

  ! here are the interfaces used for overloading
  interface plumed_f_cmd
    module procedure plumed_f_cmd_ptr
    module procedure plumed_f_cmd_char
    module procedure plumed_f_cmd_integer_0_0
    module procedure plumed_f_cmd_integer_0_1
    module procedure plumed_f_cmd_integer_0_2
    module procedure plumed_f_cmd_integer_0_3
    module procedure plumed_f_cmd_integer_0_4
    module procedure plumed_f_cmd_integer_1_0
    module procedure plumed_f_cmd_integer_1_1
    module procedure plumed_f_cmd_integer_1_2
    module procedure plumed_f_cmd_integer_1_3
    module procedure plumed_f_cmd_integer_1_4
    module procedure plumed_f_cmd_integer_2_0
    module procedure plumed_f_cmd_integer_2_1
    module procedure plumed_f_cmd_integer_2_2
    module procedure plumed_f_cmd_integer_2_3
    module procedure plumed_f_cmd_integer_2_4
    module procedure plumed_f_cmd_real_0_0
    module procedure plumed_f_cmd_real_0_1
    module procedure plumed_f_cmd_real_0_2
    module procedure plumed_f_cmd_real_0_3
    module procedure plumed_f_cmd_real_0_4
    module procedure plumed_f_cmd_real_1_0
    module procedure plumed_f_cmd_real_1_1
    module procedure plumed_f_cmd_real_1_2
    module procedure plumed_f_cmd_real_1_3
    module procedure plumed_f_cmd_real_1_4
    module procedure plumed_f_cmd_real_2_0
    module procedure plumed_f_cmd_real_2_1
    module procedure plumed_f_cmd_real_2_2
    module procedure plumed_f_cmd_real_2_3
    module procedure plumed_f_cmd_real_2_4
  end interface plumed_f_cmd

  interface plumed_f_gcmd
    module procedure plumed_f_gcmd_ptr
    module procedure plumed_f_gcmd_char
    module procedure plumed_f_gcmd_integer_0_0
    module procedure plumed_f_gcmd_integer_0_1
    module procedure plumed_f_gcmd_integer_0_2
    module procedure plumed_f_gcmd_integer_0_3
    module procedure plumed_f_gcmd_integer_0_4
    module procedure plumed_f_gcmd_integer_1_0
    module procedure plumed_f_gcmd_integer_1_1
    module procedure plumed_f_gcmd_integer_1_2
    module procedure plumed_f_gcmd_integer_1_3
    module procedure plumed_f_gcmd_integer_1_4
    module procedure plumed_f_gcmd_integer_2_0
    module procedure plumed_f_gcmd_integer_2_1
    module procedure plumed_f_gcmd_integer_2_2
    module procedure plumed_f_gcmd_integer_2_3
    module procedure plumed_f_gcmd_integer_2_4
    module procedure plumed_f_gcmd_real_0_0
    module procedure plumed_f_gcmd_real_0_1
    module procedure plumed_f_gcmd_real_0_2
    module procedure plumed_f_gcmd_real_0_3
    module procedure plumed_f_gcmd_real_0_4
    module procedure plumed_f_gcmd_real_1_0
    module procedure plumed_f_gcmd_real_1_1
    module procedure plumed_f_gcmd_real_1_2
    module procedure plumed_f_gcmd_real_1_3
    module procedure plumed_f_gcmd_real_1_4
    module procedure plumed_f_gcmd_real_2_0
    module procedure plumed_f_gcmd_real_2_1
    module procedure plumed_f_gcmd_real_2_2
    module procedure plumed_f_gcmd_real_2_3
    module procedure plumed_f_gcmd_real_2_4
  end interface plumed_f_gcmd

  contains

     ! this is a callback function.
     ! notice that it ends up in global namespace (no protection for being a module function!)
     ! be careful with name thus
     subroutine plumed_f_eh(ierror_ptr,code,what_ptr,opt_ptr) bind(C)
       type(c_ptr),         value :: ierror_ptr
       integer(kind=c_int), value :: code
       type(c_ptr),         value :: what_ptr
       type(c_ptr),         value :: opt_ptr
       integer,           pointer :: ierror
       call c_f_pointer(ierror_ptr,ierror)
       ierror=code
     end subroutine plumed_f_eh

     ! we then define all the functions needed for overloading

     subroutine plumed_f_cmd_ptr(p,key,val,dummy,ierror)
       character(kind=c_char,len=32), intent(in)    :: p
       character(kind=c_char,len=*),  intent(in)    :: key
       type(c_ptr),                     value       :: val
       type(dummy_type),   optional                 :: dummy
       integer,            optional,  intent(out)   :: ierror
       integer,            target :: myerror
       integer(kind=c_size_t) :: pass_shape(1)
       integer(kind=c_size_t) :: flags
       type(cplumed_nothrow_handler) :: nothrow
       pass_shape=(/0/)
       flags=33554432*2 ! 0x2000000*2, non-const pointer
       if(present(ierror)) then
         myerror=0
         nothrow%ptr=c_loc(myerror)
         nothrow%handler=c_funloc(plumed_f_eh)
         call plumed_f_cmd_safe_nothrow_ptr(plumed_f2c(p),key,val,pass_shape,flags,nothrow)
         ierror=myerror
       else
         nothrow%ptr=c_null_ptr
         nothrow%handler=c_null_funptr
         call plumed_f_cmd_safe_nothrow_ptr(plumed_f2c(p),key,val,pass_shape,flags,nothrow)
       endif
     end subroutine plumed_f_cmd_ptr

     subroutine plumed_f_gcmd_ptr(key,val,dummy,ierror)
       character(kind=c_char,len=*),  intent(in)    :: key
       type(c_ptr),                      value      :: val
       type(dummy_type),   optional                 :: dummy
       integer,            optional,  intent(out)   :: ierror
       character(kind=c_char,len=32) :: global
       call plumed_f_global(global)
       call plumed_f_cmd(global,key,val,ierror=ierror)
     end subroutine plumed_f_gcmd_ptr

     subroutine plumed_f_cmd_char(p,key,val,dummy,ierror)
       character(kind=c_char,len=32), intent(in)    :: p
       character(kind=c_char,len=*),  intent(in)    :: key
       character(kind=c_char,len=*), asynchronous   :: val
       type(dummy_type),   optional                 :: dummy
       integer,            optional,  intent(out)   :: ierror
       integer,            target :: myerror
       integer(kind=c_size_t) :: pass_shape(2)
       integer(kind=c_size_t) :: flags
       type(cplumed_nothrow_handler) :: nothrow
       pass_shape=(/len(val),0/)
       flags=33554432*2 ! 0x2000000*2, non-const pointer
       if(present(ierror)) then
         myerror=0
         nothrow%ptr=c_loc(myerror)
         nothrow%handler=c_funloc(plumed_f_eh)
         call plumed_f_cmd_safe_nothrow_char(plumed_f2c(p),key,val,pass_shape,flags,nothrow)
         ierror=myerror
       else
         nothrow%ptr=c_null_ptr
         nothrow%handler=c_null_funptr
         call plumed_f_cmd_safe_nothrow_char(plumed_f2c(p),key,val,pass_shape,flags,nothrow)
       endif
     end subroutine plumed_f_cmd_char

     subroutine plumed_f_gcmd_char(key,val,dummy,ierror)
       character(kind=c_char,len=*),  intent(in)    :: key
       character(kind=c_char,len=*), asynchronous   :: val
       type(dummy_type),   optional                 :: dummy
       integer,            optional,  intent(out)   :: ierror
       character(kind=c_char,len=32) :: global
       call plumed_f_global(global)
       call plumed_f_cmd(global,key,val,ierror=ierror)
     end subroutine plumed_f_gcmd_char

    subroutine plumed_f_cmd_integer_0_0(p,key,val,dummy,ierror)
      character(kind=c_char,len=32), intent(in)    :: p
      character(kind=c_char,len=*),  intent(in)    :: key
      integer(KIND=c_int), asynchronous              :: val
       type(dummy_type),   optional                 :: dummy
      integer,            optional,  intent(out)   :: ierror
      integer,            target :: myerror
      integer(kind=c_size_t) :: flags
      type(cplumed_nothrow_handler) :: nothrow
      integer(kind=c_size_t) :: pass_shape(2)
      pass_shape=(/1,0/)
       flags=33554432*2 ! 0x2000000*2, non-const pointer
       if(present(ierror)) then
         myerror=0
         nothrow%ptr=c_loc(myerror)
         nothrow%handler=c_funloc(plumed_f_eh)
         call plumed_f_cmd_safe_nothrow_int_scalar(plumed_f2c(p),key,val,pass_shape,flags,nothrow)
         ierror=myerror
       else
         nothrow%ptr=c_null_ptr
         nothrow%handler=c_null_funptr
         call plumed_f_cmd_safe_nothrow_int_scalar(plumed_f2c(p),key,val,pass_shape,flags,nothrow)
       endif
    end subroutine plumed_f_cmd_integer_0_0
    subroutine plumed_f_gcmd_integer_0_0(key,val,dummy,ierror)
      character(kind=c_char,len=*),  intent(in)    :: key
      integer(kind=c_int), asynchronous              :: val
      type(dummy_type),   optional                 :: dummy
      integer,            optional,  intent(out)   :: ierror
      character(kind=c_char,len=32) :: global
      call plumed_f_global(global)
      call plumed_f_cmd(global,key,val,ierror=ierror)
    end subroutine plumed_f_gcmd_integer_0_0
    subroutine plumed_f_cmd_integer_0_1(p,key,val,dummy,ierror)
      character(kind=c_char,len=32), intent(in)    :: p
      character(kind=c_char,len=*),  intent(in)    :: key
      integer(KIND=c_int), asynchronous              :: val(:)
       type(dummy_type),   optional                 :: dummy
      integer,            optional,  intent(out)   :: ierror
      integer,            target :: myerror
      integer(kind=c_size_t) :: flags
      type(cplumed_nothrow_handler) :: nothrow
      integer(kind=c_size_t) :: pass_shape(2)
      pass_shape(1)=size(val,1)
      pass_shape(2)=0
       flags=33554432*2 ! 0x2000000*2, non-const pointer
       if(present(ierror)) then
         myerror=0
         nothrow%ptr=c_loc(myerror)
         nothrow%handler=c_funloc(plumed_f_eh)
         call plumed_f_cmd_safe_nothrow_int(plumed_f2c(p),key,val,pass_shape,flags,nothrow)
         ierror=myerror
       else
         nothrow%ptr=c_null_ptr
         nothrow%handler=c_null_funptr
         call plumed_f_cmd_safe_nothrow_int(plumed_f2c(p),key,val,pass_shape,flags,nothrow)
       endif
    end subroutine plumed_f_cmd_integer_0_1
    subroutine plumed_f_gcmd_integer_0_1(key,val,dummy,ierror)
      character(kind=c_char,len=*),  intent(in)    :: key
      integer(kind=c_int), asynchronous              :: val(:)
      type(dummy_type),   optional                 :: dummy
      integer,            optional,  intent(out)   :: ierror
      character(kind=c_char,len=32) :: global
      call plumed_f_global(global)
      call plumed_f_cmd(global,key,val,ierror=ierror)
    end subroutine plumed_f_gcmd_integer_0_1
    subroutine plumed_f_cmd_integer_0_2(p,key,val,dummy,ierror)
      character(kind=c_char,len=32), intent(in)    :: p
      character(kind=c_char,len=*),  intent(in)    :: key
      integer(KIND=c_int), asynchronous              :: val(:,:)
       type(dummy_type),   optional                 :: dummy
      integer,            optional,  intent(out)   :: ierror
      integer,            target :: myerror
      integer(kind=c_size_t) :: flags
      type(cplumed_nothrow_handler) :: nothrow
      integer(kind=c_size_t) :: pass_shape(3)
      pass_shape(1)=size(val,2)
      pass_shape(2)=size(val,1)
      pass_shape(3)=0
       flags=33554432*2 ! 0x2000000*2, non-const pointer
       if(present(ierror)) then
         myerror=0
         nothrow%ptr=c_loc(myerror)
         nothrow%handler=c_funloc(plumed_f_eh)
         call plumed_f_cmd_safe_nothrow_int(plumed_f2c(p),key,val,pass_shape,flags,nothrow)
         ierror=myerror
       else
         nothrow%ptr=c_null_ptr
         nothrow%handler=c_null_funptr
         call plumed_f_cmd_safe_nothrow_int(plumed_f2c(p),key,val,pass_shape,flags,nothrow)
       endif
    end subroutine plumed_f_cmd_integer_0_2
    subroutine plumed_f_gcmd_integer_0_2(key,val,dummy,ierror)
      character(kind=c_char,len=*),  intent(in)    :: key
      integer(kind=c_int), asynchronous              :: val(:,:)
      type(dummy_type),   optional                 :: dummy
      integer,            optional,  intent(out)   :: ierror
      character(kind=c_char,len=32) :: global
      call plumed_f_global(global)
      call plumed_f_cmd(global,key,val,ierror=ierror)
    end subroutine plumed_f_gcmd_integer_0_2
    subroutine plumed_f_cmd_integer_0_3(p,key,val,dummy,ierror)
      character(kind=c_char,len=32), intent(in)    :: p
      character(kind=c_char,len=*),  intent(in)    :: key
      integer(KIND=c_int), asynchronous              :: val(:,:,:)
       type(dummy_type),   optional                 :: dummy
      integer,            optional,  intent(out)   :: ierror
      integer,            target :: myerror
      integer(kind=c_size_t) :: flags
      type(cplumed_nothrow_handler) :: nothrow
      integer(kind=c_size_t) :: pass_shape(4)
      pass_shape(1)=size(val,3)
      pass_shape(2)=size(val,2)
      pass_shape(3)=size(val,1)
      pass_shape(4)=0
       flags=33554432*2 ! 0x2000000*2, non-const pointer
       if(present(ierror)) then
         myerror=0
         nothrow%ptr=c_loc(myerror)
         nothrow%handler=c_funloc(plumed_f_eh)
         call plumed_f_cmd_safe_nothrow_int(plumed_f2c(p),key,val,pass_shape,flags,nothrow)
         ierror=myerror
       else
         nothrow%ptr=c_null_ptr
         nothrow%handler=c_null_funptr
         call plumed_f_cmd_safe_nothrow_int(plumed_f2c(p),key,val,pass_shape,flags,nothrow)
       endif
    end subroutine plumed_f_cmd_integer_0_3
    subroutine plumed_f_gcmd_integer_0_3(key,val,dummy,ierror)
      character(kind=c_char,len=*),  intent(in)    :: key
      integer(kind=c_int), asynchronous              :: val(:,:,:)
      type(dummy_type),   optional                 :: dummy
      integer,            optional,  intent(out)   :: ierror
      character(kind=c_char,len=32) :: global
      call plumed_f_global(global)
      call plumed_f_cmd(global,key,val,ierror=ierror)
    end subroutine plumed_f_gcmd_integer_0_3
    subroutine plumed_f_cmd_integer_0_4(p,key,val,dummy,ierror)
      character(kind=c_char,len=32), intent(in)    :: p
      character(kind=c_char,len=*),  intent(in)    :: key
      integer(KIND=c_int), asynchronous              :: val(:,:,:,:)
       type(dummy_type),   optional                 :: dummy
      integer,            optional,  intent(out)   :: ierror
      integer,            target :: myerror
      integer(kind=c_size_t) :: flags
      type(cplumed_nothrow_handler) :: nothrow
      integer(kind=c_size_t) :: pass_shape(5)
      pass_shape(1)=size(val,4)
      pass_shape(2)=size(val,3)
      pass_shape(3)=size(val,2)
      pass_shape(4)=size(val,1)
      pass_shape(5)=0
       flags=33554432*2 ! 0x2000000*2, non-const pointer
       if(present(ierror)) then
         myerror=0
         nothrow%ptr=c_loc(myerror)
         nothrow%handler=c_funloc(plumed_f_eh)
         call plumed_f_cmd_safe_nothrow_int(plumed_f2c(p),key,val,pass_shape,flags,nothrow)
         ierror=myerror
       else
         nothrow%ptr=c_null_ptr
         nothrow%handler=c_null_funptr
         call plumed_f_cmd_safe_nothrow_int(plumed_f2c(p),key,val,pass_shape,flags,nothrow)
       endif
    end subroutine plumed_f_cmd_integer_0_4
    subroutine plumed_f_gcmd_integer_0_4(key,val,dummy,ierror)
      character(kind=c_char,len=*),  intent(in)    :: key
      integer(kind=c_int), asynchronous              :: val(:,:,:,:)
      type(dummy_type),   optional                 :: dummy
      integer,            optional,  intent(out)   :: ierror
      character(kind=c_char,len=32) :: global
      call plumed_f_global(global)
      call plumed_f_cmd(global,key,val,ierror=ierror)
    end subroutine plumed_f_gcmd_integer_0_4
    subroutine plumed_f_cmd_integer_1_0(p,key,val,dummy,ierror)
      character(kind=c_char,len=32), intent(in)    :: p
      character(kind=c_char,len=*),  intent(in)    :: key
      integer(KIND=c_short), asynchronous              :: val
       type(dummy_type),   optional                 :: dummy
      integer,            optional,  intent(out)   :: ierror
      integer,            target :: myerror
      integer(kind=c_size_t) :: flags
      type(cplumed_nothrow_handler) :: nothrow
      integer(kind=c_size_t) :: pass_shape(2)
      pass_shape=(/1,0/)
       flags=33554432*2 ! 0x2000000*2, non-const pointer
       if(present(ierror)) then
         myerror=0
         nothrow%ptr=c_loc(myerror)
         nothrow%handler=c_funloc(plumed_f_eh)
         call plumed_f_cmd_safe_nothrow_short_scalar(plumed_f2c(p),key,val,pass_shape,flags,nothrow)
         ierror=myerror
       else
         nothrow%ptr=c_null_ptr
         nothrow%handler=c_null_funptr
         call plumed_f_cmd_safe_nothrow_short_scalar(plumed_f2c(p),key,val,pass_shape,flags,nothrow)
       endif
    end subroutine plumed_f_cmd_integer_1_0
    subroutine plumed_f_gcmd_integer_1_0(key,val,dummy,ierror)
      character(kind=c_char,len=*),  intent(in)    :: key
      integer(kind=c_short), asynchronous              :: val
      type(dummy_type),   optional                 :: dummy
      integer,            optional,  intent(out)   :: ierror
      character(kind=c_char,len=32) :: global
      call plumed_f_global(global)
      call plumed_f_cmd(global,key,val,ierror=ierror)
    end subroutine plumed_f_gcmd_integer_1_0
    subroutine plumed_f_cmd_integer_1_1(p,key,val,dummy,ierror)
      character(kind=c_char,len=32), intent(in)    :: p
      character(kind=c_char,len=*),  intent(in)    :: key
      integer(KIND=c_short), asynchronous              :: val(:)
       type(dummy_type),   optional                 :: dummy
      integer,            optional,  intent(out)   :: ierror
      integer,            target :: myerror
      integer(kind=c_size_t) :: flags
      type(cplumed_nothrow_handler) :: nothrow
      integer(kind=c_size_t) :: pass_shape(2)
      pass_shape(1)=size(val,1)
      pass_shape(2)=0
       flags=33554432*2 ! 0x2000000*2, non-const pointer
       if(present(ierror)) then
         myerror=0
         nothrow%ptr=c_loc(myerror)
         nothrow%handler=c_funloc(plumed_f_eh)
         call plumed_f_cmd_safe_nothrow_short(plumed_f2c(p),key,val,pass_shape,flags,nothrow)
         ierror=myerror
       else
         nothrow%ptr=c_null_ptr
         nothrow%handler=c_null_funptr
         call plumed_f_cmd_safe_nothrow_short(plumed_f2c(p),key,val,pass_shape,flags,nothrow)
       endif
    end subroutine plumed_f_cmd_integer_1_1
    subroutine plumed_f_gcmd_integer_1_1(key,val,dummy,ierror)
      character(kind=c_char,len=*),  intent(in)    :: key
      integer(kind=c_short), asynchronous              :: val(:)
      type(dummy_type),   optional                 :: dummy
      integer,            optional,  intent(out)   :: ierror
      character(kind=c_char,len=32) :: global
      call plumed_f_global(global)
      call plumed_f_cmd(global,key,val,ierror=ierror)
    end subroutine plumed_f_gcmd_integer_1_1
    subroutine plumed_f_cmd_integer_1_2(p,key,val,dummy,ierror)
      character(kind=c_char,len=32), intent(in)    :: p
      character(kind=c_char,len=*),  intent(in)    :: key
      integer(KIND=c_short), asynchronous              :: val(:,:)
       type(dummy_type),   optional                 :: dummy
      integer,            optional,  intent(out)   :: ierror
      integer,            target :: myerror
      integer(kind=c_size_t) :: flags
      type(cplumed_nothrow_handler) :: nothrow
      integer(kind=c_size_t) :: pass_shape(3)
      pass_shape(1)=size(val,2)
      pass_shape(2)=size(val,1)
      pass_shape(3)=0
       flags=33554432*2 ! 0x2000000*2, non-const pointer
       if(present(ierror)) then
         myerror=0
         nothrow%ptr=c_loc(myerror)
         nothrow%handler=c_funloc(plumed_f_eh)
         call plumed_f_cmd_safe_nothrow_short(plumed_f2c(p),key,val,pass_shape,flags,nothrow)
         ierror=myerror
       else
         nothrow%ptr=c_null_ptr
         nothrow%handler=c_null_funptr
         call plumed_f_cmd_safe_nothrow_short(plumed_f2c(p),key,val,pass_shape,flags,nothrow)
       endif
    end subroutine plumed_f_cmd_integer_1_2
    subroutine plumed_f_gcmd_integer_1_2(key,val,dummy,ierror)
      character(kind=c_char,len=*),  intent(in)    :: key
      integer(kind=c_short), asynchronous              :: val(:,:)
      type(dummy_type),   optional                 :: dummy
      integer,            optional,  intent(out)   :: ierror
      character(kind=c_char,len=32) :: global
      call plumed_f_global(global)
      call plumed_f_cmd(global,key,val,ierror=ierror)
    end subroutine plumed_f_gcmd_integer_1_2
    subroutine plumed_f_cmd_integer_1_3(p,key,val,dummy,ierror)
      character(kind=c_char,len=32), intent(in)    :: p
      character(kind=c_char,len=*),  intent(in)    :: key
      integer(KIND=c_short), asynchronous              :: val(:,:,:)
       type(dummy_type),   optional                 :: dummy
      integer,            optional,  intent(out)   :: ierror
      integer,            target :: myerror
      integer(kind=c_size_t) :: flags
      type(cplumed_nothrow_handler) :: nothrow
      integer(kind=c_size_t) :: pass_shape(4)
      pass_shape(1)=size(val,3)
      pass_shape(2)=size(val,2)
      pass_shape(3)=size(val,1)
      pass_shape(4)=0
       flags=33554432*2 ! 0x2000000*2, non-const pointer
       if(present(ierror)) then
         myerror=0
         nothrow%ptr=c_loc(myerror)
         nothrow%handler=c_funloc(plumed_f_eh)
         call plumed_f_cmd_safe_nothrow_short(plumed_f2c(p),key,val,pass_shape,flags,nothrow)
         ierror=myerror
       else
         nothrow%ptr=c_null_ptr
         nothrow%handler=c_null_funptr
         call plumed_f_cmd_safe_nothrow_short(plumed_f2c(p),key,val,pass_shape,flags,nothrow)
       endif
    end subroutine plumed_f_cmd_integer_1_3
    subroutine plumed_f_gcmd_integer_1_3(key,val,dummy,ierror)
      character(kind=c_char,len=*),  intent(in)    :: key
      integer(kind=c_short), asynchronous              :: val(:,:,:)
      type(dummy_type),   optional                 :: dummy
      integer,            optional,  intent(out)   :: ierror
      character(kind=c_char,len=32) :: global
      call plumed_f_global(global)
      call plumed_f_cmd(global,key,val,ierror=ierror)
    end subroutine plumed_f_gcmd_integer_1_3
    subroutine plumed_f_cmd_integer_1_4(p,key,val,dummy,ierror)
      character(kind=c_char,len=32), intent(in)    :: p
      character(kind=c_char,len=*),  intent(in)    :: key
      integer(KIND=c_short), asynchronous              :: val(:,:,:,:)
       type(dummy_type),   optional                 :: dummy
      integer,            optional,  intent(out)   :: ierror
      integer,            target :: myerror
      integer(kind=c_size_t) :: flags
      type(cplumed_nothrow_handler) :: nothrow
      integer(kind=c_size_t) :: pass_shape(5)
      pass_shape(1)=size(val,4)
      pass_shape(2)=size(val,3)
      pass_shape(3)=size(val,2)
      pass_shape(4)=size(val,1)
      pass_shape(5)=0
       flags=33554432*2 ! 0x2000000*2, non-const pointer
       if(present(ierror)) then
         myerror=0
         nothrow%ptr=c_loc(myerror)
         nothrow%handler=c_funloc(plumed_f_eh)
         call plumed_f_cmd_safe_nothrow_short(plumed_f2c(p),key,val,pass_shape,flags,nothrow)
         ierror=myerror
       else
         nothrow%ptr=c_null_ptr
         nothrow%handler=c_null_funptr
         call plumed_f_cmd_safe_nothrow_short(plumed_f2c(p),key,val,pass_shape,flags,nothrow)
       endif
    end subroutine plumed_f_cmd_integer_1_4
    subroutine plumed_f_gcmd_integer_1_4(key,val,dummy,ierror)
      character(kind=c_char,len=*),  intent(in)    :: key
      integer(kind=c_short), asynchronous              :: val(:,:,:,:)
      type(dummy_type),   optional                 :: dummy
      integer,            optional,  intent(out)   :: ierror
      character(kind=c_char,len=32) :: global
      call plumed_f_global(global)
      call plumed_f_cmd(global,key,val,ierror=ierror)
    end subroutine plumed_f_gcmd_integer_1_4
    subroutine plumed_f_cmd_integer_2_0(p,key,val,dummy,ierror)
      character(kind=c_char,len=32), intent(in)    :: p
      character(kind=c_char,len=*),  intent(in)    :: key
      integer(KIND=c_long), asynchronous              :: val
       type(dummy_type),   optional                 :: dummy
      integer,            optional,  intent(out)   :: ierror
      integer,            target :: myerror
      integer(kind=c_size_t) :: flags
      type(cplumed_nothrow_handler) :: nothrow
      integer(kind=c_size_t) :: pass_shape(2)
      pass_shape=(/1,0/)
       flags=33554432*2 ! 0x2000000*2, non-const pointer
       if(present(ierror)) then
         myerror=0
         nothrow%ptr=c_loc(myerror)
         nothrow%handler=c_funloc(plumed_f_eh)
         call plumed_f_cmd_safe_nothrow_long_scalar(plumed_f2c(p),key,val,pass_shape,flags,nothrow)
         ierror=myerror
       else
         nothrow%ptr=c_null_ptr
         nothrow%handler=c_null_funptr
         call plumed_f_cmd_safe_nothrow_long_scalar(plumed_f2c(p),key,val,pass_shape,flags,nothrow)
       endif
    end subroutine plumed_f_cmd_integer_2_0
    subroutine plumed_f_gcmd_integer_2_0(key,val,dummy,ierror)
      character(kind=c_char,len=*),  intent(in)    :: key
      integer(kind=c_long), asynchronous              :: val
      type(dummy_type),   optional                 :: dummy
      integer,            optional,  intent(out)   :: ierror
      character(kind=c_char,len=32) :: global
      call plumed_f_global(global)
      call plumed_f_cmd(global,key,val,ierror=ierror)
    end subroutine plumed_f_gcmd_integer_2_0
    subroutine plumed_f_cmd_integer_2_1(p,key,val,dummy,ierror)
      character(kind=c_char,len=32), intent(in)    :: p
      character(kind=c_char,len=*),  intent(in)    :: key
      integer(KIND=c_long), asynchronous              :: val(:)
       type(dummy_type),   optional                 :: dummy
      integer,            optional,  intent(out)   :: ierror
      integer,            target :: myerror
      integer(kind=c_size_t) :: flags
      type(cplumed_nothrow_handler) :: nothrow
      integer(kind=c_size_t) :: pass_shape(2)
      pass_shape(1)=size(val,1)
      pass_shape(2)=0
       flags=33554432*2 ! 0x2000000*2, non-const pointer
       if(present(ierror)) then
         myerror=0
         nothrow%ptr=c_loc(myerror)
         nothrow%handler=c_funloc(plumed_f_eh)
         call plumed_f_cmd_safe_nothrow_long(plumed_f2c(p),key,val,pass_shape,flags,nothrow)
         ierror=myerror
       else
         nothrow%ptr=c_null_ptr
         nothrow%handler=c_null_funptr
         call plumed_f_cmd_safe_nothrow_long(plumed_f2c(p),key,val,pass_shape,flags,nothrow)
       endif
    end subroutine plumed_f_cmd_integer_2_1
    subroutine plumed_f_gcmd_integer_2_1(key,val,dummy,ierror)
      character(kind=c_char,len=*),  intent(in)    :: key
      integer(kind=c_long), asynchronous              :: val(:)
      type(dummy_type),   optional                 :: dummy
      integer,            optional,  intent(out)   :: ierror
      character(kind=c_char,len=32) :: global
      call plumed_f_global(global)
      call plumed_f_cmd(global,key,val,ierror=ierror)
    end subroutine plumed_f_gcmd_integer_2_1
    subroutine plumed_f_cmd_integer_2_2(p,key,val,dummy,ierror)
      character(kind=c_char,len=32), intent(in)    :: p
      character(kind=c_char,len=*),  intent(in)    :: key
      integer(KIND=c_long), asynchronous              :: val(:,:)
       type(dummy_type),   optional                 :: dummy
      integer,            optional,  intent(out)   :: ierror
      integer,            target :: myerror
      integer(kind=c_size_t) :: flags
      type(cplumed_nothrow_handler) :: nothrow
      integer(kind=c_size_t) :: pass_shape(3)
      pass_shape(1)=size(val,2)
      pass_shape(2)=size(val,1)
      pass_shape(3)=0
       flags=33554432*2 ! 0x2000000*2, non-const pointer
       if(present(ierror)) then
         myerror=0
         nothrow%ptr=c_loc(myerror)
         nothrow%handler=c_funloc(plumed_f_eh)
         call plumed_f_cmd_safe_nothrow_long(plumed_f2c(p),key,val,pass_shape,flags,nothrow)
         ierror=myerror
       else
         nothrow%ptr=c_null_ptr
         nothrow%handler=c_null_funptr
         call plumed_f_cmd_safe_nothrow_long(plumed_f2c(p),key,val,pass_shape,flags,nothrow)
       endif
    end subroutine plumed_f_cmd_integer_2_2
    subroutine plumed_f_gcmd_integer_2_2(key,val,dummy,ierror)
      character(kind=c_char,len=*),  intent(in)    :: key
      integer(kind=c_long), asynchronous              :: val(:,:)
      type(dummy_type),   optional                 :: dummy
      integer,            optional,  intent(out)   :: ierror
      character(kind=c_char,len=32) :: global
      call plumed_f_global(global)
      call plumed_f_cmd(global,key,val,ierror=ierror)
    end subroutine plumed_f_gcmd_integer_2_2
    subroutine plumed_f_cmd_integer_2_3(p,key,val,dummy,ierror)
      character(kind=c_char,len=32), intent(in)    :: p
      character(kind=c_char,len=*),  intent(in)    :: key
      integer(KIND=c_long), asynchronous              :: val(:,:,:)
       type(dummy_type),   optional                 :: dummy
      integer,            optional,  intent(out)   :: ierror
      integer,            target :: myerror
      integer(kind=c_size_t) :: flags
      type(cplumed_nothrow_handler) :: nothrow
      integer(kind=c_size_t) :: pass_shape(4)
      pass_shape(1)=size(val,3)
      pass_shape(2)=size(val,2)
      pass_shape(3)=size(val,1)
      pass_shape(4)=0
       flags=33554432*2 ! 0x2000000*2, non-const pointer
       if(present(ierror)) then
         myerror=0
         nothrow%ptr=c_loc(myerror)
         nothrow%handler=c_funloc(plumed_f_eh)
         call plumed_f_cmd_safe_nothrow_long(plumed_f2c(p),key,val,pass_shape,flags,nothrow)
         ierror=myerror
       else
         nothrow%ptr=c_null_ptr
         nothrow%handler=c_null_funptr
         call plumed_f_cmd_safe_nothrow_long(plumed_f2c(p),key,val,pass_shape,flags,nothrow)
       endif
    end subroutine plumed_f_cmd_integer_2_3
    subroutine plumed_f_gcmd_integer_2_3(key,val,dummy,ierror)
      character(kind=c_char,len=*),  intent(in)    :: key
      integer(kind=c_long), asynchronous              :: val(:,:,:)
      type(dummy_type),   optional                 :: dummy
      integer,            optional,  intent(out)   :: ierror
      character(kind=c_char,len=32) :: global
      call plumed_f_global(global)
      call plumed_f_cmd(global,key,val,ierror=ierror)
    end subroutine plumed_f_gcmd_integer_2_3
    subroutine plumed_f_cmd_integer_2_4(p,key,val,dummy,ierror)
      character(kind=c_char,len=32), intent(in)    :: p
      character(kind=c_char,len=*),  intent(in)    :: key
      integer(KIND=c_long), asynchronous              :: val(:,:,:,:)
       type(dummy_type),   optional                 :: dummy
      integer,            optional,  intent(out)   :: ierror
      integer,            target :: myerror
      integer(kind=c_size_t) :: flags
      type(cplumed_nothrow_handler) :: nothrow
      integer(kind=c_size_t) :: pass_shape(5)
      pass_shape(1)=size(val,4)
      pass_shape(2)=size(val,3)
      pass_shape(3)=size(val,2)
      pass_shape(4)=size(val,1)
      pass_shape(5)=0
       flags=33554432*2 ! 0x2000000*2, non-const pointer
       if(present(ierror)) then
         myerror=0
         nothrow%ptr=c_loc(myerror)
         nothrow%handler=c_funloc(plumed_f_eh)
         call plumed_f_cmd_safe_nothrow_long(plumed_f2c(p),key,val,pass_shape,flags,nothrow)
         ierror=myerror
       else
         nothrow%ptr=c_null_ptr
         nothrow%handler=c_null_funptr
         call plumed_f_cmd_safe_nothrow_long(plumed_f2c(p),key,val,pass_shape,flags,nothrow)
       endif
    end subroutine plumed_f_cmd_integer_2_4
    subroutine plumed_f_gcmd_integer_2_4(key,val,dummy,ierror)
      character(kind=c_char,len=*),  intent(in)    :: key
      integer(kind=c_long), asynchronous              :: val(:,:,:,:)
      type(dummy_type),   optional                 :: dummy
      integer,            optional,  intent(out)   :: ierror
      character(kind=c_char,len=32) :: global
      call plumed_f_global(global)
      call plumed_f_cmd(global,key,val,ierror=ierror)
    end subroutine plumed_f_gcmd_integer_2_4
    subroutine plumed_f_cmd_real_0_0(p,key,val,dummy,ierror)
      character(kind=c_char,len=32), intent(in)    :: p
      character(kind=c_char,len=*),  intent(in)    :: key
      real(KIND=c_float), asynchronous              :: val
       type(dummy_type),   optional                 :: dummy
      integer,            optional,  intent(out)   :: ierror
      integer,            target :: myerror
      integer(kind=c_size_t) :: flags
      type(cplumed_nothrow_handler) :: nothrow
      integer(kind=c_size_t) :: pass_shape(2)
      pass_shape=(/1,0/)
       flags=33554432*2 ! 0x2000000*2, non-const pointer
       if(present(ierror)) then
         myerror=0
         nothrow%ptr=c_loc(myerror)
         nothrow%handler=c_funloc(plumed_f_eh)
         call plumed_f_cmd_safe_nothrow_float_scalar(plumed_f2c(p),key,val,pass_shape,flags,nothrow)
         ierror=myerror
       else
         nothrow%ptr=c_null_ptr
         nothrow%handler=c_null_funptr
         call plumed_f_cmd_safe_nothrow_float_scalar(plumed_f2c(p),key,val,pass_shape,flags,nothrow)
       endif
    end subroutine plumed_f_cmd_real_0_0
    subroutine plumed_f_gcmd_real_0_0(key,val,dummy,ierror)
      character(kind=c_char,len=*),  intent(in)    :: key
      real(kind=c_float), asynchronous              :: val
      type(dummy_type),   optional                 :: dummy
      integer,            optional,  intent(out)   :: ierror
      character(kind=c_char,len=32) :: global
      call plumed_f_global(global)
      call plumed_f_cmd(global,key,val,ierror=ierror)
    end subroutine plumed_f_gcmd_real_0_0
    subroutine plumed_f_cmd_real_0_1(p,key,val,dummy,ierror)
      character(kind=c_char,len=32), intent(in)    :: p
      character(kind=c_char,len=*),  intent(in)    :: key
      real(KIND=c_float), asynchronous              :: val(:)
       type(dummy_type),   optional                 :: dummy
      integer,            optional,  intent(out)   :: ierror
      integer,            target :: myerror
      integer(kind=c_size_t) :: flags
      type(cplumed_nothrow_handler) :: nothrow
      integer(kind=c_size_t) :: pass_shape(2)
      pass_shape(1)=size(val,1)
      pass_shape(2)=0
       flags=33554432*2 ! 0x2000000*2, non-const pointer
       if(present(ierror)) then
         myerror=0
         nothrow%ptr=c_loc(myerror)
         nothrow%handler=c_funloc(plumed_f_eh)
         call plumed_f_cmd_safe_nothrow_float(plumed_f2c(p),key,val,pass_shape,flags,nothrow)
         ierror=myerror
       else
         nothrow%ptr=c_null_ptr
         nothrow%handler=c_null_funptr
         call plumed_f_cmd_safe_nothrow_float(plumed_f2c(p),key,val,pass_shape,flags,nothrow)
       endif
    end subroutine plumed_f_cmd_real_0_1
    subroutine plumed_f_gcmd_real_0_1(key,val,dummy,ierror)
      character(kind=c_char,len=*),  intent(in)    :: key
      real(kind=c_float), asynchronous              :: val(:)
      type(dummy_type),   optional                 :: dummy
      integer,            optional,  intent(out)   :: ierror
      character(kind=c_char,len=32) :: global
      call plumed_f_global(global)
      call plumed_f_cmd(global,key,val,ierror=ierror)
    end subroutine plumed_f_gcmd_real_0_1
    subroutine plumed_f_cmd_real_0_2(p,key,val,dummy,ierror)
      character(kind=c_char,len=32), intent(in)    :: p
      character(kind=c_char,len=*),  intent(in)    :: key
      real(KIND=c_float), asynchronous              :: val(:,:)
       type(dummy_type),   optional                 :: dummy
      integer,            optional,  intent(out)   :: ierror
      integer,            target :: myerror
      integer(kind=c_size_t) :: flags
      type(cplumed_nothrow_handler) :: nothrow
      integer(kind=c_size_t) :: pass_shape(3)
      pass_shape(1)=size(val,2)
      pass_shape(2)=size(val,1)
      pass_shape(3)=0
       flags=33554432*2 ! 0x2000000*2, non-const pointer
       if(present(ierror)) then
         myerror=0
         nothrow%ptr=c_loc(myerror)
         nothrow%handler=c_funloc(plumed_f_eh)
         call plumed_f_cmd_safe_nothrow_float(plumed_f2c(p),key,val,pass_shape,flags,nothrow)
         ierror=myerror
       else
         nothrow%ptr=c_null_ptr
         nothrow%handler=c_null_funptr
         call plumed_f_cmd_safe_nothrow_float(plumed_f2c(p),key,val,pass_shape,flags,nothrow)
       endif
    end subroutine plumed_f_cmd_real_0_2
    subroutine plumed_f_gcmd_real_0_2(key,val,dummy,ierror)
      character(kind=c_char,len=*),  intent(in)    :: key
      real(kind=c_float), asynchronous              :: val(:,:)
      type(dummy_type),   optional                 :: dummy
      integer,            optional,  intent(out)   :: ierror
      character(kind=c_char,len=32) :: global
      call plumed_f_global(global)
      call plumed_f_cmd(global,key,val,ierror=ierror)
    end subroutine plumed_f_gcmd_real_0_2
    subroutine plumed_f_cmd_real_0_3(p,key,val,dummy,ierror)
      character(kind=c_char,len=32), intent(in)    :: p
      character(kind=c_char,len=*),  intent(in)    :: key
      real(KIND=c_float), asynchronous              :: val(:,:,:)
       type(dummy_type),   optional                 :: dummy
      integer,            optional,  intent(out)   :: ierror
      integer,            target :: myerror
      integer(kind=c_size_t) :: flags
      type(cplumed_nothrow_handler) :: nothrow
      integer(kind=c_size_t) :: pass_shape(4)
      pass_shape(1)=size(val,3)
      pass_shape(2)=size(val,2)
      pass_shape(3)=size(val,1)
      pass_shape(4)=0
       flags=33554432*2 ! 0x2000000*2, non-const pointer
       if(present(ierror)) then
         myerror=0
         nothrow%ptr=c_loc(myerror)
         nothrow%handler=c_funloc(plumed_f_eh)
         call plumed_f_cmd_safe_nothrow_float(plumed_f2c(p),key,val,pass_shape,flags,nothrow)
         ierror=myerror
       else
         nothrow%ptr=c_null_ptr
         nothrow%handler=c_null_funptr
         call plumed_f_cmd_safe_nothrow_float(plumed_f2c(p),key,val,pass_shape,flags,nothrow)
       endif
    end subroutine plumed_f_cmd_real_0_3
    subroutine plumed_f_gcmd_real_0_3(key,val,dummy,ierror)
      character(kind=c_char,len=*),  intent(in)    :: key
      real(kind=c_float), asynchronous              :: val(:,:,:)
      type(dummy_type),   optional                 :: dummy
      integer,            optional,  intent(out)   :: ierror
      character(kind=c_char,len=32) :: global
      call plumed_f_global(global)
      call plumed_f_cmd(global,key,val,ierror=ierror)
    end subroutine plumed_f_gcmd_real_0_3
    subroutine plumed_f_cmd_real_0_4(p,key,val,dummy,ierror)
      character(kind=c_char,len=32), intent(in)    :: p
      character(kind=c_char,len=*),  intent(in)    :: key
      real(KIND=c_float), asynchronous              :: val(:,:,:,:)
       type(dummy_type),   optional                 :: dummy
      integer,            optional,  intent(out)   :: ierror
      integer,            target :: myerror
      integer(kind=c_size_t) :: flags
      type(cplumed_nothrow_handler) :: nothrow
      integer(kind=c_size_t) :: pass_shape(5)
      pass_shape(1)=size(val,4)
      pass_shape(2)=size(val,3)
      pass_shape(3)=size(val,2)
      pass_shape(4)=size(val,1)
      pass_shape(5)=0
       flags=33554432*2 ! 0x2000000*2, non-const pointer
       if(present(ierror)) then
         myerror=0
         nothrow%ptr=c_loc(myerror)
         nothrow%handler=c_funloc(plumed_f_eh)
         call plumed_f_cmd_safe_nothrow_float(plumed_f2c(p),key,val,pass_shape,flags,nothrow)
         ierror=myerror
       else
         nothrow%ptr=c_null_ptr
         nothrow%handler=c_null_funptr
         call plumed_f_cmd_safe_nothrow_float(plumed_f2c(p),key,val,pass_shape,flags,nothrow)
       endif
    end subroutine plumed_f_cmd_real_0_4
    subroutine plumed_f_gcmd_real_0_4(key,val,dummy,ierror)
      character(kind=c_char,len=*),  intent(in)    :: key
      real(kind=c_float), asynchronous              :: val(:,:,:,:)
      type(dummy_type),   optional                 :: dummy
      integer,            optional,  intent(out)   :: ierror
      character(kind=c_char,len=32) :: global
      call plumed_f_global(global)
      call plumed_f_cmd(global,key,val,ierror=ierror)
    end subroutine plumed_f_gcmd_real_0_4
    subroutine plumed_f_cmd_real_1_0(p,key,val,dummy,ierror)
      character(kind=c_char,len=32), intent(in)    :: p
      character(kind=c_char,len=*),  intent(in)    :: key
      real(KIND=c_double), asynchronous              :: val
       type(dummy_type),   optional                 :: dummy
      integer,            optional,  intent(out)   :: ierror
      integer,            target :: myerror
      integer(kind=c_size_t) :: flags
      type(cplumed_nothrow_handler) :: nothrow
      integer(kind=c_size_t) :: pass_shape(2)
      pass_shape=(/1,0/)
       flags=33554432*2 ! 0x2000000*2, non-const pointer
       if(present(ierror)) then
         myerror=0
         nothrow%ptr=c_loc(myerror)
         nothrow%handler=c_funloc(plumed_f_eh)
         call plumed_f_cmd_safe_nothrow_double_scalar(plumed_f2c(p),key,val,pass_shape,flags,nothrow)
         ierror=myerror
       else
         nothrow%ptr=c_null_ptr
         nothrow%handler=c_null_funptr
         call plumed_f_cmd_safe_nothrow_double_scalar(plumed_f2c(p),key,val,pass_shape,flags,nothrow)
       endif
    end subroutine plumed_f_cmd_real_1_0
    subroutine plumed_f_gcmd_real_1_0(key,val,dummy,ierror)
      character(kind=c_char,len=*),  intent(in)    :: key
      real(kind=c_double), asynchronous              :: val
      type(dummy_type),   optional                 :: dummy
      integer,            optional,  intent(out)   :: ierror
      character(kind=c_char,len=32) :: global
      call plumed_f_global(global)
      call plumed_f_cmd(global,key,val,ierror=ierror)
    end subroutine plumed_f_gcmd_real_1_0
    subroutine plumed_f_cmd_real_1_1(p,key,val,dummy,ierror)
      character(kind=c_char,len=32), intent(in)    :: p
      character(kind=c_char,len=*),  intent(in)    :: key
      real(KIND=c_double), asynchronous              :: val(:)
       type(dummy_type),   optional                 :: dummy
      integer,            optional,  intent(out)   :: ierror
      integer,            target :: myerror
      integer(kind=c_size_t) :: flags
      type(cplumed_nothrow_handler) :: nothrow
      integer(kind=c_size_t) :: pass_shape(2)
      pass_shape(1)=size(val,1)
      pass_shape(2)=0
       flags=33554432*2 ! 0x2000000*2, non-const pointer
       if(present(ierror)) then
         myerror=0
         nothrow%ptr=c_loc(myerror)
         nothrow%handler=c_funloc(plumed_f_eh)
         call plumed_f_cmd_safe_nothrow_double(plumed_f2c(p),key,val,pass_shape,flags,nothrow)
         ierror=myerror
       else
         nothrow%ptr=c_null_ptr
         nothrow%handler=c_null_funptr
         call plumed_f_cmd_safe_nothrow_double(plumed_f2c(p),key,val,pass_shape,flags,nothrow)
       endif
    end subroutine plumed_f_cmd_real_1_1
    subroutine plumed_f_gcmd_real_1_1(key,val,dummy,ierror)
      character(kind=c_char,len=*),  intent(in)    :: key
      real(kind=c_double), asynchronous              :: val(:)
      type(dummy_type),   optional                 :: dummy
      integer,            optional,  intent(out)   :: ierror
      character(kind=c_char,len=32) :: global
      call plumed_f_global(global)
      call plumed_f_cmd(global,key,val,ierror=ierror)
    end subroutine plumed_f_gcmd_real_1_1
    subroutine plumed_f_cmd_real_1_2(p,key,val,dummy,ierror)
      character(kind=c_char,len=32), intent(in)    :: p
      character(kind=c_char,len=*),  intent(in)    :: key
      real(KIND=c_double), asynchronous              :: val(:,:)
       type(dummy_type),   optional                 :: dummy
      integer,            optional,  intent(out)   :: ierror
      integer,            target :: myerror
      integer(kind=c_size_t) :: flags
      type(cplumed_nothrow_handler) :: nothrow
      integer(kind=c_size_t) :: pass_shape(3)
      pass_shape(1)=size(val,2)
      pass_shape(2)=size(val,1)
      pass_shape(3)=0
       flags=33554432*2 ! 0x2000000*2, non-const pointer
       if(present(ierror)) then
         myerror=0
         nothrow%ptr=c_loc(myerror)
         nothrow%handler=c_funloc(plumed_f_eh)
         call plumed_f_cmd_safe_nothrow_double(plumed_f2c(p),key,val,pass_shape,flags,nothrow)
         ierror=myerror
       else
         nothrow%ptr=c_null_ptr
         nothrow%handler=c_null_funptr
         call plumed_f_cmd_safe_nothrow_double(plumed_f2c(p),key,val,pass_shape,flags,nothrow)
       endif
    end subroutine plumed_f_cmd_real_1_2
    subroutine plumed_f_gcmd_real_1_2(key,val,dummy,ierror)
      character(kind=c_char,len=*),  intent(in)    :: key
      real(kind=c_double), asynchronous              :: val(:,:)
      type(dummy_type),   optional                 :: dummy
      integer,            optional,  intent(out)   :: ierror
      character(kind=c_char,len=32) :: global
      call plumed_f_global(global)
      call plumed_f_cmd(global,key,val,ierror=ierror)
    end subroutine plumed_f_gcmd_real_1_2
    subroutine plumed_f_cmd_real_1_3(p,key,val,dummy,ierror)
      character(kind=c_char,len=32), intent(in)    :: p
      character(kind=c_char,len=*),  intent(in)    :: key
      real(KIND=c_double), asynchronous              :: val(:,:,:)
       type(dummy_type),   optional                 :: dummy
      integer,            optional,  intent(out)   :: ierror
      integer,            target :: myerror
      integer(kind=c_size_t) :: flags
      type(cplumed_nothrow_handler) :: nothrow
      integer(kind=c_size_t) :: pass_shape(4)
      pass_shape(1)=size(val,3)
      pass_shape(2)=size(val,2)
      pass_shape(3)=size(val,1)
      pass_shape(4)=0
       flags=33554432*2 ! 0x2000000*2, non-const pointer
       if(present(ierror)) then
         myerror=0
         nothrow%ptr=c_loc(myerror)
         nothrow%handler=c_funloc(plumed_f_eh)
         call plumed_f_cmd_safe_nothrow_double(plumed_f2c(p),key,val,pass_shape,flags,nothrow)
         ierror=myerror
       else
         nothrow%ptr=c_null_ptr
         nothrow%handler=c_null_funptr
         call plumed_f_cmd_safe_nothrow_double(plumed_f2c(p),key,val,pass_shape,flags,nothrow)
       endif
    end subroutine plumed_f_cmd_real_1_3
    subroutine plumed_f_gcmd_real_1_3(key,val,dummy,ierror)
      character(kind=c_char,len=*),  intent(in)    :: key
      real(kind=c_double), asynchronous              :: val(:,:,:)
      type(dummy_type),   optional                 :: dummy
      integer,            optional,  intent(out)   :: ierror
      character(kind=c_char,len=32) :: global
      call plumed_f_global(global)
      call plumed_f_cmd(global,key,val,ierror=ierror)
    end subroutine plumed_f_gcmd_real_1_3
    subroutine plumed_f_cmd_real_1_4(p,key,val,dummy,ierror)
      character(kind=c_char,len=32), intent(in)    :: p
      character(kind=c_char,len=*),  intent(in)    :: key
      real(KIND=c_double), asynchronous              :: val(:,:,:,:)
       type(dummy_type),   optional                 :: dummy
      integer,            optional,  intent(out)   :: ierror
      integer,            target :: myerror
      integer(kind=c_size_t) :: flags
      type(cplumed_nothrow_handler) :: nothrow
      integer(kind=c_size_t) :: pass_shape(5)
      pass_shape(1)=size(val,4)
      pass_shape(2)=size(val,3)
      pass_shape(3)=size(val,2)
      pass_shape(4)=size(val,1)
      pass_shape(5)=0
       flags=33554432*2 ! 0x2000000*2, non-const pointer
       if(present(ierror)) then
         myerror=0
         nothrow%ptr=c_loc(myerror)
         nothrow%handler=c_funloc(plumed_f_eh)
         call plumed_f_cmd_safe_nothrow_double(plumed_f2c(p),key,val,pass_shape,flags,nothrow)
         ierror=myerror
       else
         nothrow%ptr=c_null_ptr
         nothrow%handler=c_null_funptr
         call plumed_f_cmd_safe_nothrow_double(plumed_f2c(p),key,val,pass_shape,flags,nothrow)
       endif
    end subroutine plumed_f_cmd_real_1_4
    subroutine plumed_f_gcmd_real_1_4(key,val,dummy,ierror)
      character(kind=c_char,len=*),  intent(in)    :: key
      real(kind=c_double), asynchronous              :: val(:,:,:,:)
      type(dummy_type),   optional                 :: dummy
      integer,            optional,  intent(out)   :: ierror
      character(kind=c_char,len=32) :: global
      call plumed_f_global(global)
      call plumed_f_cmd(global,key,val,ierror=ierror)
    end subroutine plumed_f_gcmd_real_1_4
    subroutine plumed_f_cmd_real_2_0(p,key,val,dummy,ierror)
      character(kind=c_char,len=32), intent(in)    :: p
      character(kind=c_char,len=*),  intent(in)    :: key
      real(KIND=c_long_double), asynchronous              :: val
       type(dummy_type),   optional                 :: dummy
      integer,            optional,  intent(out)   :: ierror
      integer,            target :: myerror
      integer(kind=c_size_t) :: flags
      type(cplumed_nothrow_handler) :: nothrow
      integer(kind=c_size_t) :: pass_shape(2)
      pass_shape=(/1,0/)
       flags=33554432*2 ! 0x2000000*2, non-const pointer
       if(present(ierror)) then
         myerror=0
         nothrow%ptr=c_loc(myerror)
         nothrow%handler=c_funloc(plumed_f_eh)
         call plumed_f_cmd_safe_nothrow_long_double_scalar(plumed_f2c(p),key,val,pass_shape,flags,nothrow)
         ierror=myerror
       else
         nothrow%ptr=c_null_ptr
         nothrow%handler=c_null_funptr
         call plumed_f_cmd_safe_nothrow_long_double_scalar(plumed_f2c(p),key,val,pass_shape,flags,nothrow)
       endif
    end subroutine plumed_f_cmd_real_2_0
    subroutine plumed_f_gcmd_real_2_0(key,val,dummy,ierror)
      character(kind=c_char,len=*),  intent(in)    :: key
      real(kind=c_long_double), asynchronous              :: val
      type(dummy_type),   optional                 :: dummy
      integer,            optional,  intent(out)   :: ierror
      character(kind=c_char,len=32) :: global
      call plumed_f_global(global)
      call plumed_f_cmd(global,key,val,ierror=ierror)
    end subroutine plumed_f_gcmd_real_2_0
    subroutine plumed_f_cmd_real_2_1(p,key,val,dummy,ierror)
      character(kind=c_char,len=32), intent(in)    :: p
      character(kind=c_char,len=*),  intent(in)    :: key
      real(KIND=c_long_double), asynchronous              :: val(:)
       type(dummy_type),   optional                 :: dummy
      integer,            optional,  intent(out)   :: ierror
      integer,            target :: myerror
      integer(kind=c_size_t) :: flags
      type(cplumed_nothrow_handler) :: nothrow
      integer(kind=c_size_t) :: pass_shape(2)
      pass_shape(1)=size(val,1)
      pass_shape(2)=0
       flags=33554432*2 ! 0x2000000*2, non-const pointer
       if(present(ierror)) then
         myerror=0
         nothrow%ptr=c_loc(myerror)
         nothrow%handler=c_funloc(plumed_f_eh)
         call plumed_f_cmd_safe_nothrow_long_double(plumed_f2c(p),key,val,pass_shape,flags,nothrow)
         ierror=myerror
       else
         nothrow%ptr=c_null_ptr
         nothrow%handler=c_null_funptr
         call plumed_f_cmd_safe_nothrow_long_double(plumed_f2c(p),key,val,pass_shape,flags,nothrow)
       endif
    end subroutine plumed_f_cmd_real_2_1
    subroutine plumed_f_gcmd_real_2_1(key,val,dummy,ierror)
      character(kind=c_char,len=*),  intent(in)    :: key
      real(kind=c_long_double), asynchronous              :: val(:)
      type(dummy_type),   optional                 :: dummy
      integer,            optional,  intent(out)   :: ierror
      character(kind=c_char,len=32) :: global
      call plumed_f_global(global)
      call plumed_f_cmd(global,key,val,ierror=ierror)
    end subroutine plumed_f_gcmd_real_2_1
    subroutine plumed_f_cmd_real_2_2(p,key,val,dummy,ierror)
      character(kind=c_char,len=32), intent(in)    :: p
      character(kind=c_char,len=*),  intent(in)    :: key
      real(KIND=c_long_double), asynchronous              :: val(:,:)
       type(dummy_type),   optional                 :: dummy
      integer,            optional,  intent(out)   :: ierror
      integer,            target :: myerror
      integer(kind=c_size_t) :: flags
      type(cplumed_nothrow_handler) :: nothrow
      integer(kind=c_size_t) :: pass_shape(3)
      pass_shape(1)=size(val,2)
      pass_shape(2)=size(val,1)
      pass_shape(3)=0
       flags=33554432*2 ! 0x2000000*2, non-const pointer
       if(present(ierror)) then
         myerror=0
         nothrow%ptr=c_loc(myerror)
         nothrow%handler=c_funloc(plumed_f_eh)
         call plumed_f_cmd_safe_nothrow_long_double(plumed_f2c(p),key,val,pass_shape,flags,nothrow)
         ierror=myerror
       else
         nothrow%ptr=c_null_ptr
         nothrow%handler=c_null_funptr
         call plumed_f_cmd_safe_nothrow_long_double(plumed_f2c(p),key,val,pass_shape,flags,nothrow)
       endif
    end subroutine plumed_f_cmd_real_2_2
    subroutine plumed_f_gcmd_real_2_2(key,val,dummy,ierror)
      character(kind=c_char,len=*),  intent(in)    :: key
      real(kind=c_long_double), asynchronous              :: val(:,:)
      type(dummy_type),   optional                 :: dummy
      integer,            optional,  intent(out)   :: ierror
      character(kind=c_char,len=32) :: global
      call plumed_f_global(global)
      call plumed_f_cmd(global,key,val,ierror=ierror)
    end subroutine plumed_f_gcmd_real_2_2
    subroutine plumed_f_cmd_real_2_3(p,key,val,dummy,ierror)
      character(kind=c_char,len=32), intent(in)    :: p
      character(kind=c_char,len=*),  intent(in)    :: key
      real(KIND=c_long_double), asynchronous              :: val(:,:,:)
       type(dummy_type),   optional                 :: dummy
      integer,            optional,  intent(out)   :: ierror
      integer,            target :: myerror
      integer(kind=c_size_t) :: flags
      type(cplumed_nothrow_handler) :: nothrow
      integer(kind=c_size_t) :: pass_shape(4)
      pass_shape(1)=size(val,3)
      pass_shape(2)=size(val,2)
      pass_shape(3)=size(val,1)
      pass_shape(4)=0
       flags=33554432*2 ! 0x2000000*2, non-const pointer
       if(present(ierror)) then
         myerror=0
         nothrow%ptr=c_loc(myerror)
         nothrow%handler=c_funloc(plumed_f_eh)
         call plumed_f_cmd_safe_nothrow_long_double(plumed_f2c(p),key,val,pass_shape,flags,nothrow)
         ierror=myerror
       else
         nothrow%ptr=c_null_ptr
         nothrow%handler=c_null_funptr
         call plumed_f_cmd_safe_nothrow_long_double(plumed_f2c(p),key,val,pass_shape,flags,nothrow)
       endif
    end subroutine plumed_f_cmd_real_2_3
    subroutine plumed_f_gcmd_real_2_3(key,val,dummy,ierror)
      character(kind=c_char,len=*),  intent(in)    :: key
      real(kind=c_long_double), asynchronous              :: val(:,:,:)
      type(dummy_type),   optional                 :: dummy
      integer,            optional,  intent(out)   :: ierror
      character(kind=c_char,len=32) :: global
      call plumed_f_global(global)
      call plumed_f_cmd(global,key,val,ierror=ierror)
    end subroutine plumed_f_gcmd_real_2_3
    subroutine plumed_f_cmd_real_2_4(p,key,val,dummy,ierror)
      character(kind=c_char,len=32), intent(in)    :: p
      character(kind=c_char,len=*),  intent(in)    :: key
      real(KIND=c_long_double), asynchronous              :: val(:,:,:,:)
       type(dummy_type),   optional                 :: dummy
      integer,            optional,  intent(out)   :: ierror
      integer,            target :: myerror
      integer(kind=c_size_t) :: flags
      type(cplumed_nothrow_handler) :: nothrow
      integer(kind=c_size_t) :: pass_shape(5)
      pass_shape(1)=size(val,4)
      pass_shape(2)=size(val,3)
      pass_shape(3)=size(val,2)
      pass_shape(4)=size(val,1)
      pass_shape(5)=0
       flags=33554432*2 ! 0x2000000*2, non-const pointer
       if(present(ierror)) then
         myerror=0
         nothrow%ptr=c_loc(myerror)
         nothrow%handler=c_funloc(plumed_f_eh)
         call plumed_f_cmd_safe_nothrow_long_double(plumed_f2c(p),key,val,pass_shape,flags,nothrow)
         ierror=myerror
       else
         nothrow%ptr=c_null_ptr
         nothrow%handler=c_null_funptr
         call plumed_f_cmd_safe_nothrow_long_double(plumed_f2c(p),key,val,pass_shape,flags,nothrow)
       endif
    end subroutine plumed_f_cmd_real_2_4
    subroutine plumed_f_gcmd_real_2_4(key,val,dummy,ierror)
      character(kind=c_char,len=*),  intent(in)    :: key
      real(kind=c_long_double), asynchronous              :: val(:,:,:,:)
      type(dummy_type),   optional                 :: dummy
      integer,            optional,  intent(out)   :: ierror
      character(kind=c_char,len=32) :: global
      call plumed_f_global(global)
      call plumed_f_cmd(global,key,val,ierror=ierror)
    end subroutine plumed_f_gcmd_real_2_4

end module plumed_module

