! vim:ft=fortran



module plumed_module
  use iso_c_binding
  implicit none

  private
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
  public :: plumed_error

  type :: plumed_error
    integer                         :: code=0
    character(len = :), allocatable :: what
  end type plumed_error

  type :: dummy_type
  end type dummy_type

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

  interface
    subroutine plumed_f_cmd_safe_nothrow_char(p,key,val,pass_shape,nocopy,e,eh) bind(C)
      import
      character(kind=c_char), intent(in)    :: p(32)
      character(kind=c_char), intent(in)    :: key(*)
      character(kind=c_char)                :: val(*)
      integer(kind=c_size_t) :: pass_shape(*)
      integer(kind=c_int),            value :: nocopy
      type(c_ptr),                    value :: e
      type(c_ptr),                    value :: eh
    end subroutine plumed_f_cmd_safe_nothrow_char
  end interface

  interface plumed_f_cmd
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

  interface
    subroutine plumed_f_cmd_safe_nothrow_int_scalar(p,key,val,pass_shape,nocopy,e,eh) bind(C,name="plumed_f_cmd_safe_nothrow_int")
      import
      character(kind=c_char), intent(in)    :: p(32)
      character(kind=c_char), intent(in)    :: key(*)
      integer(kind=c_int)                           :: val
      integer(kind=c_size_t) :: pass_shape(*)
      integer(kind=c_int),            value :: nocopy
      type(c_ptr),                    value :: e
      type(c_ptr),                    value :: eh
    end subroutine plumed_f_cmd_safe_nothrow_int_scalar
  end interface
  interface
    subroutine plumed_f_cmd_safe_nothrow_int(p,key,val,pass_shape,nocopy,e,eh) bind(C,name="plumed_f_cmd_safe_nothrow_int")
      import
      character(kind=c_char), intent(in)    :: p(32)
      character(kind=c_char), intent(in)    :: key(*)
      integer(kind=c_int)                           :: val(*)
      integer(kind=c_size_t) :: pass_shape(*)
      integer(kind=c_int),            value :: nocopy
      type(c_ptr),                    value :: e
      type(c_ptr),                    value :: eh
    end subroutine plumed_f_cmd_safe_nothrow_int
  end interface
  interface
    subroutine plumed_f_cmd_safe_nothrow_short_scalar(p,key,val,pass_shape,nocopy,e,eh)&
        & bind(C,name="plumed_f_cmd_safe_nothrow_short")
      import
      character(kind=c_char), intent(in)    :: p(32)
      character(kind=c_char), intent(in)    :: key(*)
      integer(kind=c_short)                           :: val
      integer(kind=c_size_t) :: pass_shape(*)
      integer(kind=c_int),            value :: nocopy
      type(c_ptr),                    value :: e
      type(c_ptr),                    value :: eh
    end subroutine plumed_f_cmd_safe_nothrow_short_scalar
  end interface
  interface
    subroutine plumed_f_cmd_safe_nothrow_short(p,key,val,pass_shape,nocopy,e,eh) bind(C,name="plumed_f_cmd_safe_nothrow_short")
      import
      character(kind=c_char), intent(in)    :: p(32)
      character(kind=c_char), intent(in)    :: key(*)
      integer(kind=c_short)                           :: val(*)
      integer(kind=c_size_t) :: pass_shape(*)
      integer(kind=c_int),            value :: nocopy
      type(c_ptr),                    value :: e
      type(c_ptr),                    value :: eh
    end subroutine plumed_f_cmd_safe_nothrow_short
  end interface
  interface
    subroutine plumed_f_cmd_safe_nothrow_long_scalar(p,key,val,pass_shape,nocopy,e,eh) bind(C,name="plumed_f_cmd_safe_nothrow_long")
      import
      character(kind=c_char), intent(in)    :: p(32)
      character(kind=c_char), intent(in)    :: key(*)
      integer(kind=c_long)                           :: val
      integer(kind=c_size_t) :: pass_shape(*)
      integer(kind=c_int),            value :: nocopy
      type(c_ptr),                    value :: e
      type(c_ptr),                    value :: eh
    end subroutine plumed_f_cmd_safe_nothrow_long_scalar
  end interface
  interface
    subroutine plumed_f_cmd_safe_nothrow_long(p,key,val,pass_shape,nocopy,e,eh) bind(C,name="plumed_f_cmd_safe_nothrow_long")
      import
      character(kind=c_char), intent(in)    :: p(32)
      character(kind=c_char), intent(in)    :: key(*)
      integer(kind=c_long)                           :: val(*)
      integer(kind=c_size_t) :: pass_shape(*)
      integer(kind=c_int),            value :: nocopy
      type(c_ptr),                    value :: e
      type(c_ptr),                    value :: eh
    end subroutine plumed_f_cmd_safe_nothrow_long
  end interface
  interface
    subroutine plumed_f_cmd_safe_nothrow_float_scalar(p,key,val,pass_shape,nocopy,e,eh)&
        & bind(C,name="plumed_f_cmd_safe_nothrow_float")
      import
      character(kind=c_char), intent(in)    :: p(32)
      character(kind=c_char), intent(in)    :: key(*)
      real(kind=c_float)                           :: val
      integer(kind=c_size_t) :: pass_shape(*)
      integer(kind=c_int),            value :: nocopy
      type(c_ptr),                    value :: e
      type(c_ptr),                    value :: eh
    end subroutine plumed_f_cmd_safe_nothrow_float_scalar
  end interface
  interface
    subroutine plumed_f_cmd_safe_nothrow_float(p,key,val,pass_shape,nocopy,e,eh) bind(C,name="plumed_f_cmd_safe_nothrow_float")
      import
      character(kind=c_char), intent(in)    :: p(32)
      character(kind=c_char), intent(in)    :: key(*)
      real(kind=c_float)                           :: val(*)
      integer(kind=c_size_t) :: pass_shape(*)
      integer(kind=c_int),            value :: nocopy
      type(c_ptr),                    value :: e
      type(c_ptr),                    value :: eh
    end subroutine plumed_f_cmd_safe_nothrow_float
  end interface
  interface
    subroutine plumed_f_cmd_safe_nothrow_double_scalar(p,key,val,pass_shape,nocopy,e,eh)&
        & bind(C,name="plumed_f_cmd_safe_nothrow_double")
      import
      character(kind=c_char), intent(in)    :: p(32)
      character(kind=c_char), intent(in)    :: key(*)
      real(kind=c_double)                           :: val
      integer(kind=c_size_t) :: pass_shape(*)
      integer(kind=c_int),            value :: nocopy
      type(c_ptr),                    value :: e
      type(c_ptr),                    value :: eh
    end subroutine plumed_f_cmd_safe_nothrow_double_scalar
  end interface
  interface
    subroutine plumed_f_cmd_safe_nothrow_double(p,key,val,pass_shape,nocopy,e,eh) bind(C,name="plumed_f_cmd_safe_nothrow_double")
      import
      character(kind=c_char), intent(in)    :: p(32)
      character(kind=c_char), intent(in)    :: key(*)
      real(kind=c_double)                           :: val(*)
      integer(kind=c_size_t) :: pass_shape(*)
      integer(kind=c_int),            value :: nocopy
      type(c_ptr),                    value :: e
      type(c_ptr),                    value :: eh
    end subroutine plumed_f_cmd_safe_nothrow_double
  end interface
  interface
    subroutine plumed_f_cmd_safe_nothrow_long_double_scalar(p,key,val,pass_shape,nocopy,e,eh)&
        & bind(C,name="plumed_f_cmd_safe_nothrow_long_double")
      import
      character(kind=c_char), intent(in)    :: p(32)
      character(kind=c_char), intent(in)    :: key(*)
      real(kind=c_long_double)                           :: val
      integer(kind=c_size_t) :: pass_shape(*)
      integer(kind=c_int),            value :: nocopy
      type(c_ptr),                    value :: e
      type(c_ptr),                    value :: eh
    end subroutine plumed_f_cmd_safe_nothrow_long_double_scalar
  end interface
  interface
    subroutine plumed_f_cmd_safe_nothrow_long_double(p,key,val,pass_shape,nocopy,e,eh) bind(C,name="plumed_f_cmd_safe_nothrow_long_&
        &double")
      import
      character(kind=c_char), intent(in)    :: p(32)
      character(kind=c_char), intent(in)    :: key(*)
      real(kind=c_long_double)                           :: val(*)
      integer(kind=c_size_t) :: pass_shape(*)
      integer(kind=c_int),            value :: nocopy
      type(c_ptr),                    value :: e
      type(c_ptr),                    value :: eh
    end subroutine plumed_f_cmd_safe_nothrow_long_double
  end interface

  contains

     subroutine error_handler(error_ptr,code,what_ptr,opt_ptr) bind(C)
       type(c_ptr),         value :: error_ptr
       integer(kind=c_int), value :: code
       type(c_ptr),         value :: what_ptr
       type(c_ptr),         value :: opt_ptr
       type(plumed_error), pointer :: error
       character(len=1, kind=C_CHAR), pointer :: p_chars(:)
       integer :: i,j
       call c_f_pointer(error_ptr,error)
       error%code=code
       if (.not. C_associated(what_ptr)) then
         error%what=""
       else
         call C_F_pointer(what_ptr, p_chars, [huge(0)])
         do i = 1, huge(0)
           if (p_chars(i) == C_NULL_CHAR) exit
         enddo
         allocate(character(i-1) :: error%what)
         do j = 1,i-1
           error%what(j:j)=p_chars(j)
         enddo
       endif
     end subroutine error_handler

     subroutine plumed_f_cmd_char(p,key,val,dummy,error,ierror,nocopy)
       character(kind=c_char,len=32), intent(in)    :: p
       character(kind=c_char,len=*),  intent(in)    :: key
       character(kind=c_char,len=*), asynchronous   :: val
       type(dummy_type),   optional                 :: dummy
       type(plumed_error), optional,  intent(out)   :: error
       integer,            optional,  intent(out)   :: ierror
       logical,            optional,  intent(in)    :: nocopy
       type(plumed_error), target :: myerror
       integer(kind=c_size_t) :: pass_shape(2)
       integer(kind=c_int)    :: inocopy
       pass_shape=(/len(val),0/)
       if(present(nocopy)) then
         if(nocopy) then
           inocopy=1
         endif
       else
         inocopy=0
       endif
       if(present(error) .or. present(ierror)) then
         call plumed_f_cmd_safe_nothrow_char(p,key,val,pass_shape,inocopy,c_loc(myerror),c_funloc(error_handler))
         if(present(error)) then
           error=myerror
         endif
         if(present(ierror)) then
           ierror=myerror%code
         endif
       else
         call plumed_f_cmd_safe_nothrow_char(p,key,val,pass_shape,inocopy,c_null_ptr,c_null_ptr)
       endif
     end subroutine plumed_f_cmd_char

     subroutine plumed_f_gcmd_char(key,val,dummy,error,ierror,nocopy)
       character(kind=c_char,len=*),  intent(in)    :: key
       character(kind=c_char,len=*), asynchronous   :: val
       type(dummy_type),   optional                 :: dummy
       type(plumed_error), optional,  intent(out)   :: error
       integer,            optional,  intent(out)   :: ierror
       logical,            optional,  intent(in)    :: nocopy
       character(len=32) :: global
       call plumed_f_global(global)
       call plumed_f_cmd(global,key,val,error=error,ierror=ierror)
     end subroutine plumed_f_gcmd_char

    subroutine plumed_f_cmd_integer_0_0(p,key,val,dummy,error,ierror,nocopy)
      character(kind=c_char,len=32), intent(in)    :: p
      character(kind=c_char,len=*),  intent(in)    :: key
      integer(KIND=c_int), asynchronous              :: val
       type(dummy_type),   optional                 :: dummy
      type(plumed_error), optional,  intent(out)   :: error
      integer,            optional,  intent(out)   :: ierror
      logical,            optional,  intent(in)    :: nocopy
      type(plumed_error), target :: myerror
      integer(kind=c_int)    :: inocopy
      integer(kind=c_size_t) :: pass_shape(2)
      pass_shape=(/1,0/)
       if(present(nocopy)) then
         if(nocopy) then
           inocopy=1
         endif
       else
         inocopy=0
       endif
       if(present(error) .or. present(ierror)) then
         call plumed_f_cmd_safe_nothrow_int_scalar(p,key,val,pass_shape,inocopy,c_loc(myerror),c_funloc(error_handler))
         if(present(error)) then
           error=myerror
         endif
         if(present(ierror)) then
           ierror=myerror%code
         endif
       else
         call plumed_f_cmd_safe_nothrow_int_scalar(p,key,val,pass_shape,inocopy,c_null_ptr,c_null_ptr)
       endif
    end subroutine plumed_f_cmd_integer_0_0
    subroutine plumed_f_gcmd_integer_0_0(key,val,dummy,error,ierror,nocopy)
      character(kind=c_char,len=*),  intent(in)    :: key
      integer(kind=c_int), asynchronous              :: val
      type(dummy_type),   optional                 :: dummy
      type(plumed_error), optional,  intent(out)   :: error
      integer,            optional,  intent(out)   :: ierror
      logical,            optional,  intent(in)    :: nocopy
      character(len=32) :: global
      call plumed_f_global(global)
      call plumed_f_cmd(global,key,val,error=error,ierror=ierror,nocopy=nocopy)
    end subroutine plumed_f_gcmd_integer_0_0
    subroutine plumed_f_cmd_integer_0_1(p,key,val,dummy,error,ierror,nocopy)
      character(kind=c_char,len=32), intent(in)    :: p
      character(kind=c_char,len=*),  intent(in)    :: key
      integer(KIND=c_int), asynchronous              :: val(:)
       type(dummy_type),   optional                 :: dummy
      type(plumed_error), optional,  intent(out)   :: error
      integer,            optional,  intent(out)   :: ierror
      logical,            optional,  intent(in)    :: nocopy
      type(plumed_error), target :: myerror
      integer(kind=c_int)    :: inocopy
      integer(kind=c_size_t) :: pass_shape(2)
      pass_shape(1)=size(val,1)
      pass_shape(2)=0
       if(present(nocopy)) then
         if(nocopy) then
           inocopy=1
         endif
       else
         inocopy=0
       endif
       if(present(error) .or. present(ierror)) then
         call plumed_f_cmd_safe_nothrow_int(p,key,val,pass_shape,inocopy,c_loc(myerror),c_funloc(error_handler))
         if(present(error)) then
           error=myerror
         endif
         if(present(ierror)) then
           ierror=myerror%code
         endif
       else
         call plumed_f_cmd_safe_nothrow_int(p,key,val,pass_shape,inocopy,c_null_ptr,c_null_ptr)
       endif
    end subroutine plumed_f_cmd_integer_0_1
    subroutine plumed_f_gcmd_integer_0_1(key,val,dummy,error,ierror,nocopy)
      character(kind=c_char,len=*),  intent(in)    :: key
      integer(kind=c_int), asynchronous              :: val(:)
      type(dummy_type),   optional                 :: dummy
      type(plumed_error), optional,  intent(out)   :: error
      integer,            optional,  intent(out)   :: ierror
      logical,            optional,  intent(in)    :: nocopy
      character(len=32) :: global
      call plumed_f_global(global)
      call plumed_f_cmd(global,key,val,error=error,ierror=ierror,nocopy=nocopy)
    end subroutine plumed_f_gcmd_integer_0_1
    subroutine plumed_f_cmd_integer_0_2(p,key,val,dummy,error,ierror,nocopy)
      character(kind=c_char,len=32), intent(in)    :: p
      character(kind=c_char,len=*),  intent(in)    :: key
      integer(KIND=c_int), asynchronous              :: val(:,:)
       type(dummy_type),   optional                 :: dummy
      type(plumed_error), optional,  intent(out)   :: error
      integer,            optional,  intent(out)   :: ierror
      logical,            optional,  intent(in)    :: nocopy
      type(plumed_error), target :: myerror
      integer(kind=c_int)    :: inocopy
      integer(kind=c_size_t) :: pass_shape(3)
      pass_shape(1)=size(val,2)
      pass_shape(2)=size(val,1)
      pass_shape(3)=0
       if(present(nocopy)) then
         if(nocopy) then
           inocopy=1
         endif
       else
         inocopy=0
       endif
       if(present(error) .or. present(ierror)) then
         call plumed_f_cmd_safe_nothrow_int(p,key,val,pass_shape,inocopy,c_loc(myerror),c_funloc(error_handler))
         if(present(error)) then
           error=myerror
         endif
         if(present(ierror)) then
           ierror=myerror%code
         endif
       else
         call plumed_f_cmd_safe_nothrow_int(p,key,val,pass_shape,inocopy,c_null_ptr,c_null_ptr)
       endif
    end subroutine plumed_f_cmd_integer_0_2
    subroutine plumed_f_gcmd_integer_0_2(key,val,dummy,error,ierror,nocopy)
      character(kind=c_char,len=*),  intent(in)    :: key
      integer(kind=c_int), asynchronous              :: val(:,:)
      type(dummy_type),   optional                 :: dummy
      type(plumed_error), optional,  intent(out)   :: error
      integer,            optional,  intent(out)   :: ierror
      logical,            optional,  intent(in)    :: nocopy
      character(len=32) :: global
      call plumed_f_global(global)
      call plumed_f_cmd(global,key,val,error=error,ierror=ierror,nocopy=nocopy)
    end subroutine plumed_f_gcmd_integer_0_2
    subroutine plumed_f_cmd_integer_0_3(p,key,val,dummy,error,ierror,nocopy)
      character(kind=c_char,len=32), intent(in)    :: p
      character(kind=c_char,len=*),  intent(in)    :: key
      integer(KIND=c_int), asynchronous              :: val(:,:,:)
       type(dummy_type),   optional                 :: dummy
      type(plumed_error), optional,  intent(out)   :: error
      integer,            optional,  intent(out)   :: ierror
      logical,            optional,  intent(in)    :: nocopy
      type(plumed_error), target :: myerror
      integer(kind=c_int)    :: inocopy
      integer(kind=c_size_t) :: pass_shape(4)
      pass_shape(1)=size(val,3)
      pass_shape(2)=size(val,2)
      pass_shape(3)=size(val,1)
      pass_shape(4)=0
       if(present(nocopy)) then
         if(nocopy) then
           inocopy=1
         endif
       else
         inocopy=0
       endif
       if(present(error) .or. present(ierror)) then
         call plumed_f_cmd_safe_nothrow_int(p,key,val,pass_shape,inocopy,c_loc(myerror),c_funloc(error_handler))
         if(present(error)) then
           error=myerror
         endif
         if(present(ierror)) then
           ierror=myerror%code
         endif
       else
         call plumed_f_cmd_safe_nothrow_int(p,key,val,pass_shape,inocopy,c_null_ptr,c_null_ptr)
       endif
    end subroutine plumed_f_cmd_integer_0_3
    subroutine plumed_f_gcmd_integer_0_3(key,val,dummy,error,ierror,nocopy)
      character(kind=c_char,len=*),  intent(in)    :: key
      integer(kind=c_int), asynchronous              :: val(:,:,:)
      type(dummy_type),   optional                 :: dummy
      type(plumed_error), optional,  intent(out)   :: error
      integer,            optional,  intent(out)   :: ierror
      logical,            optional,  intent(in)    :: nocopy
      character(len=32) :: global
      call plumed_f_global(global)
      call plumed_f_cmd(global,key,val,error=error,ierror=ierror,nocopy=nocopy)
    end subroutine plumed_f_gcmd_integer_0_3
    subroutine plumed_f_cmd_integer_0_4(p,key,val,dummy,error,ierror,nocopy)
      character(kind=c_char,len=32), intent(in)    :: p
      character(kind=c_char,len=*),  intent(in)    :: key
      integer(KIND=c_int), asynchronous              :: val(:,:,:,:)
       type(dummy_type),   optional                 :: dummy
      type(plumed_error), optional,  intent(out)   :: error
      integer,            optional,  intent(out)   :: ierror
      logical,            optional,  intent(in)    :: nocopy
      type(plumed_error), target :: myerror
      integer(kind=c_int)    :: inocopy
      integer(kind=c_size_t) :: pass_shape(5)
      pass_shape(1)=size(val,4)
      pass_shape(2)=size(val,3)
      pass_shape(3)=size(val,2)
      pass_shape(4)=size(val,1)
      pass_shape(5)=0
       if(present(nocopy)) then
         if(nocopy) then
           inocopy=1
         endif
       else
         inocopy=0
       endif
       if(present(error) .or. present(ierror)) then
         call plumed_f_cmd_safe_nothrow_int(p,key,val,pass_shape,inocopy,c_loc(myerror),c_funloc(error_handler))
         if(present(error)) then
           error=myerror
         endif
         if(present(ierror)) then
           ierror=myerror%code
         endif
       else
         call plumed_f_cmd_safe_nothrow_int(p,key,val,pass_shape,inocopy,c_null_ptr,c_null_ptr)
       endif
    end subroutine plumed_f_cmd_integer_0_4
    subroutine plumed_f_gcmd_integer_0_4(key,val,dummy,error,ierror,nocopy)
      character(kind=c_char,len=*),  intent(in)    :: key
      integer(kind=c_int), asynchronous              :: val(:,:,:,:)
      type(dummy_type),   optional                 :: dummy
      type(plumed_error), optional,  intent(out)   :: error
      integer,            optional,  intent(out)   :: ierror
      logical,            optional,  intent(in)    :: nocopy
      character(len=32) :: global
      call plumed_f_global(global)
      call plumed_f_cmd(global,key,val,error=error,ierror=ierror,nocopy=nocopy)
    end subroutine plumed_f_gcmd_integer_0_4
    subroutine plumed_f_cmd_integer_1_0(p,key,val,dummy,error,ierror,nocopy)
      character(kind=c_char,len=32), intent(in)    :: p
      character(kind=c_char,len=*),  intent(in)    :: key
      integer(KIND=c_short), asynchronous              :: val
       type(dummy_type),   optional                 :: dummy
      type(plumed_error), optional,  intent(out)   :: error
      integer,            optional,  intent(out)   :: ierror
      logical,            optional,  intent(in)    :: nocopy
      type(plumed_error), target :: myerror
      integer(kind=c_int)    :: inocopy
      integer(kind=c_size_t) :: pass_shape(2)
      pass_shape=(/1,0/)
       if(present(nocopy)) then
         if(nocopy) then
           inocopy=1
         endif
       else
         inocopy=0
       endif
       if(present(error) .or. present(ierror)) then
         call plumed_f_cmd_safe_nothrow_short_scalar(p,key,val,pass_shape,inocopy,c_loc(myerror),c_funloc(error_handler))
         if(present(error)) then
           error=myerror
         endif
         if(present(ierror)) then
           ierror=myerror%code
         endif
       else
         call plumed_f_cmd_safe_nothrow_short_scalar(p,key,val,pass_shape,inocopy,c_null_ptr,c_null_ptr)
       endif
    end subroutine plumed_f_cmd_integer_1_0
    subroutine plumed_f_gcmd_integer_1_0(key,val,dummy,error,ierror,nocopy)
      character(kind=c_char,len=*),  intent(in)    :: key
      integer(kind=c_short), asynchronous              :: val
      type(dummy_type),   optional                 :: dummy
      type(plumed_error), optional,  intent(out)   :: error
      integer,            optional,  intent(out)   :: ierror
      logical,            optional,  intent(in)    :: nocopy
      character(len=32) :: global
      call plumed_f_global(global)
      call plumed_f_cmd(global,key,val,error=error,ierror=ierror,nocopy=nocopy)
    end subroutine plumed_f_gcmd_integer_1_0
    subroutine plumed_f_cmd_integer_1_1(p,key,val,dummy,error,ierror,nocopy)
      character(kind=c_char,len=32), intent(in)    :: p
      character(kind=c_char,len=*),  intent(in)    :: key
      integer(KIND=c_short), asynchronous              :: val(:)
       type(dummy_type),   optional                 :: dummy
      type(plumed_error), optional,  intent(out)   :: error
      integer,            optional,  intent(out)   :: ierror
      logical,            optional,  intent(in)    :: nocopy
      type(plumed_error), target :: myerror
      integer(kind=c_int)    :: inocopy
      integer(kind=c_size_t) :: pass_shape(2)
      pass_shape(1)=size(val,1)
      pass_shape(2)=0
       if(present(nocopy)) then
         if(nocopy) then
           inocopy=1
         endif
       else
         inocopy=0
       endif
       if(present(error) .or. present(ierror)) then
         call plumed_f_cmd_safe_nothrow_short(p,key,val,pass_shape,inocopy,c_loc(myerror),c_funloc(error_handler))
         if(present(error)) then
           error=myerror
         endif
         if(present(ierror)) then
           ierror=myerror%code
         endif
       else
         call plumed_f_cmd_safe_nothrow_short(p,key,val,pass_shape,inocopy,c_null_ptr,c_null_ptr)
       endif
    end subroutine plumed_f_cmd_integer_1_1
    subroutine plumed_f_gcmd_integer_1_1(key,val,dummy,error,ierror,nocopy)
      character(kind=c_char,len=*),  intent(in)    :: key
      integer(kind=c_short), asynchronous              :: val(:)
      type(dummy_type),   optional                 :: dummy
      type(plumed_error), optional,  intent(out)   :: error
      integer,            optional,  intent(out)   :: ierror
      logical,            optional,  intent(in)    :: nocopy
      character(len=32) :: global
      call plumed_f_global(global)
      call plumed_f_cmd(global,key,val,error=error,ierror=ierror,nocopy=nocopy)
    end subroutine plumed_f_gcmd_integer_1_1
    subroutine plumed_f_cmd_integer_1_2(p,key,val,dummy,error,ierror,nocopy)
      character(kind=c_char,len=32), intent(in)    :: p
      character(kind=c_char,len=*),  intent(in)    :: key
      integer(KIND=c_short), asynchronous              :: val(:,:)
       type(dummy_type),   optional                 :: dummy
      type(plumed_error), optional,  intent(out)   :: error
      integer,            optional,  intent(out)   :: ierror
      logical,            optional,  intent(in)    :: nocopy
      type(plumed_error), target :: myerror
      integer(kind=c_int)    :: inocopy
      integer(kind=c_size_t) :: pass_shape(3)
      pass_shape(1)=size(val,2)
      pass_shape(2)=size(val,1)
      pass_shape(3)=0
       if(present(nocopy)) then
         if(nocopy) then
           inocopy=1
         endif
       else
         inocopy=0
       endif
       if(present(error) .or. present(ierror)) then
         call plumed_f_cmd_safe_nothrow_short(p,key,val,pass_shape,inocopy,c_loc(myerror),c_funloc(error_handler))
         if(present(error)) then
           error=myerror
         endif
         if(present(ierror)) then
           ierror=myerror%code
         endif
       else
         call plumed_f_cmd_safe_nothrow_short(p,key,val,pass_shape,inocopy,c_null_ptr,c_null_ptr)
       endif
    end subroutine plumed_f_cmd_integer_1_2
    subroutine plumed_f_gcmd_integer_1_2(key,val,dummy,error,ierror,nocopy)
      character(kind=c_char,len=*),  intent(in)    :: key
      integer(kind=c_short), asynchronous              :: val(:,:)
      type(dummy_type),   optional                 :: dummy
      type(plumed_error), optional,  intent(out)   :: error
      integer,            optional,  intent(out)   :: ierror
      logical,            optional,  intent(in)    :: nocopy
      character(len=32) :: global
      call plumed_f_global(global)
      call plumed_f_cmd(global,key,val,error=error,ierror=ierror,nocopy=nocopy)
    end subroutine plumed_f_gcmd_integer_1_2
    subroutine plumed_f_cmd_integer_1_3(p,key,val,dummy,error,ierror,nocopy)
      character(kind=c_char,len=32), intent(in)    :: p
      character(kind=c_char,len=*),  intent(in)    :: key
      integer(KIND=c_short), asynchronous              :: val(:,:,:)
       type(dummy_type),   optional                 :: dummy
      type(plumed_error), optional,  intent(out)   :: error
      integer,            optional,  intent(out)   :: ierror
      logical,            optional,  intent(in)    :: nocopy
      type(plumed_error), target :: myerror
      integer(kind=c_int)    :: inocopy
      integer(kind=c_size_t) :: pass_shape(4)
      pass_shape(1)=size(val,3)
      pass_shape(2)=size(val,2)
      pass_shape(3)=size(val,1)
      pass_shape(4)=0
       if(present(nocopy)) then
         if(nocopy) then
           inocopy=1
         endif
       else
         inocopy=0
       endif
       if(present(error) .or. present(ierror)) then
         call plumed_f_cmd_safe_nothrow_short(p,key,val,pass_shape,inocopy,c_loc(myerror),c_funloc(error_handler))
         if(present(error)) then
           error=myerror
         endif
         if(present(ierror)) then
           ierror=myerror%code
         endif
       else
         call plumed_f_cmd_safe_nothrow_short(p,key,val,pass_shape,inocopy,c_null_ptr,c_null_ptr)
       endif
    end subroutine plumed_f_cmd_integer_1_3
    subroutine plumed_f_gcmd_integer_1_3(key,val,dummy,error,ierror,nocopy)
      character(kind=c_char,len=*),  intent(in)    :: key
      integer(kind=c_short), asynchronous              :: val(:,:,:)
      type(dummy_type),   optional                 :: dummy
      type(plumed_error), optional,  intent(out)   :: error
      integer,            optional,  intent(out)   :: ierror
      logical,            optional,  intent(in)    :: nocopy
      character(len=32) :: global
      call plumed_f_global(global)
      call plumed_f_cmd(global,key,val,error=error,ierror=ierror,nocopy=nocopy)
    end subroutine plumed_f_gcmd_integer_1_3
    subroutine plumed_f_cmd_integer_1_4(p,key,val,dummy,error,ierror,nocopy)
      character(kind=c_char,len=32), intent(in)    :: p
      character(kind=c_char,len=*),  intent(in)    :: key
      integer(KIND=c_short), asynchronous              :: val(:,:,:,:)
       type(dummy_type),   optional                 :: dummy
      type(plumed_error), optional,  intent(out)   :: error
      integer,            optional,  intent(out)   :: ierror
      logical,            optional,  intent(in)    :: nocopy
      type(plumed_error), target :: myerror
      integer(kind=c_int)    :: inocopy
      integer(kind=c_size_t) :: pass_shape(5)
      pass_shape(1)=size(val,4)
      pass_shape(2)=size(val,3)
      pass_shape(3)=size(val,2)
      pass_shape(4)=size(val,1)
      pass_shape(5)=0
       if(present(nocopy)) then
         if(nocopy) then
           inocopy=1
         endif
       else
         inocopy=0
       endif
       if(present(error) .or. present(ierror)) then
         call plumed_f_cmd_safe_nothrow_short(p,key,val,pass_shape,inocopy,c_loc(myerror),c_funloc(error_handler))
         if(present(error)) then
           error=myerror
         endif
         if(present(ierror)) then
           ierror=myerror%code
         endif
       else
         call plumed_f_cmd_safe_nothrow_short(p,key,val,pass_shape,inocopy,c_null_ptr,c_null_ptr)
       endif
    end subroutine plumed_f_cmd_integer_1_4
    subroutine plumed_f_gcmd_integer_1_4(key,val,dummy,error,ierror,nocopy)
      character(kind=c_char,len=*),  intent(in)    :: key
      integer(kind=c_short), asynchronous              :: val(:,:,:,:)
      type(dummy_type),   optional                 :: dummy
      type(plumed_error), optional,  intent(out)   :: error
      integer,            optional,  intent(out)   :: ierror
      logical,            optional,  intent(in)    :: nocopy
      character(len=32) :: global
      call plumed_f_global(global)
      call plumed_f_cmd(global,key,val,error=error,ierror=ierror,nocopy=nocopy)
    end subroutine plumed_f_gcmd_integer_1_4
    subroutine plumed_f_cmd_integer_2_0(p,key,val,dummy,error,ierror,nocopy)
      character(kind=c_char,len=32), intent(in)    :: p
      character(kind=c_char,len=*),  intent(in)    :: key
      integer(KIND=c_long), asynchronous              :: val
       type(dummy_type),   optional                 :: dummy
      type(plumed_error), optional,  intent(out)   :: error
      integer,            optional,  intent(out)   :: ierror
      logical,            optional,  intent(in)    :: nocopy
      type(plumed_error), target :: myerror
      integer(kind=c_int)    :: inocopy
      integer(kind=c_size_t) :: pass_shape(2)
      pass_shape=(/1,0/)
       if(present(nocopy)) then
         if(nocopy) then
           inocopy=1
         endif
       else
         inocopy=0
       endif
       if(present(error) .or. present(ierror)) then
         call plumed_f_cmd_safe_nothrow_long_scalar(p,key,val,pass_shape,inocopy,c_loc(myerror),c_funloc(error_handler))
         if(present(error)) then
           error=myerror
         endif
         if(present(ierror)) then
           ierror=myerror%code
         endif
       else
         call plumed_f_cmd_safe_nothrow_long_scalar(p,key,val,pass_shape,inocopy,c_null_ptr,c_null_ptr)
       endif
    end subroutine plumed_f_cmd_integer_2_0
    subroutine plumed_f_gcmd_integer_2_0(key,val,dummy,error,ierror,nocopy)
      character(kind=c_char,len=*),  intent(in)    :: key
      integer(kind=c_long), asynchronous              :: val
      type(dummy_type),   optional                 :: dummy
      type(plumed_error), optional,  intent(out)   :: error
      integer,            optional,  intent(out)   :: ierror
      logical,            optional,  intent(in)    :: nocopy
      character(len=32) :: global
      call plumed_f_global(global)
      call plumed_f_cmd(global,key,val,error=error,ierror=ierror,nocopy=nocopy)
    end subroutine plumed_f_gcmd_integer_2_0
    subroutine plumed_f_cmd_integer_2_1(p,key,val,dummy,error,ierror,nocopy)
      character(kind=c_char,len=32), intent(in)    :: p
      character(kind=c_char,len=*),  intent(in)    :: key
      integer(KIND=c_long), asynchronous              :: val(:)
       type(dummy_type),   optional                 :: dummy
      type(plumed_error), optional,  intent(out)   :: error
      integer,            optional,  intent(out)   :: ierror
      logical,            optional,  intent(in)    :: nocopy
      type(plumed_error), target :: myerror
      integer(kind=c_int)    :: inocopy
      integer(kind=c_size_t) :: pass_shape(2)
      pass_shape(1)=size(val,1)
      pass_shape(2)=0
       if(present(nocopy)) then
         if(nocopy) then
           inocopy=1
         endif
       else
         inocopy=0
       endif
       if(present(error) .or. present(ierror)) then
         call plumed_f_cmd_safe_nothrow_long(p,key,val,pass_shape,inocopy,c_loc(myerror),c_funloc(error_handler))
         if(present(error)) then
           error=myerror
         endif
         if(present(ierror)) then
           ierror=myerror%code
         endif
       else
         call plumed_f_cmd_safe_nothrow_long(p,key,val,pass_shape,inocopy,c_null_ptr,c_null_ptr)
       endif
    end subroutine plumed_f_cmd_integer_2_1
    subroutine plumed_f_gcmd_integer_2_1(key,val,dummy,error,ierror,nocopy)
      character(kind=c_char,len=*),  intent(in)    :: key
      integer(kind=c_long), asynchronous              :: val(:)
      type(dummy_type),   optional                 :: dummy
      type(plumed_error), optional,  intent(out)   :: error
      integer,            optional,  intent(out)   :: ierror
      logical,            optional,  intent(in)    :: nocopy
      character(len=32) :: global
      call plumed_f_global(global)
      call plumed_f_cmd(global,key,val,error=error,ierror=ierror,nocopy=nocopy)
    end subroutine plumed_f_gcmd_integer_2_1
    subroutine plumed_f_cmd_integer_2_2(p,key,val,dummy,error,ierror,nocopy)
      character(kind=c_char,len=32), intent(in)    :: p
      character(kind=c_char,len=*),  intent(in)    :: key
      integer(KIND=c_long), asynchronous              :: val(:,:)
       type(dummy_type),   optional                 :: dummy
      type(plumed_error), optional,  intent(out)   :: error
      integer,            optional,  intent(out)   :: ierror
      logical,            optional,  intent(in)    :: nocopy
      type(plumed_error), target :: myerror
      integer(kind=c_int)    :: inocopy
      integer(kind=c_size_t) :: pass_shape(3)
      pass_shape(1)=size(val,2)
      pass_shape(2)=size(val,1)
      pass_shape(3)=0
       if(present(nocopy)) then
         if(nocopy) then
           inocopy=1
         endif
       else
         inocopy=0
       endif
       if(present(error) .or. present(ierror)) then
         call plumed_f_cmd_safe_nothrow_long(p,key,val,pass_shape,inocopy,c_loc(myerror),c_funloc(error_handler))
         if(present(error)) then
           error=myerror
         endif
         if(present(ierror)) then
           ierror=myerror%code
         endif
       else
         call plumed_f_cmd_safe_nothrow_long(p,key,val,pass_shape,inocopy,c_null_ptr,c_null_ptr)
       endif
    end subroutine plumed_f_cmd_integer_2_2
    subroutine plumed_f_gcmd_integer_2_2(key,val,dummy,error,ierror,nocopy)
      character(kind=c_char,len=*),  intent(in)    :: key
      integer(kind=c_long), asynchronous              :: val(:,:)
      type(dummy_type),   optional                 :: dummy
      type(plumed_error), optional,  intent(out)   :: error
      integer,            optional,  intent(out)   :: ierror
      logical,            optional,  intent(in)    :: nocopy
      character(len=32) :: global
      call plumed_f_global(global)
      call plumed_f_cmd(global,key,val,error=error,ierror=ierror,nocopy=nocopy)
    end subroutine plumed_f_gcmd_integer_2_2
    subroutine plumed_f_cmd_integer_2_3(p,key,val,dummy,error,ierror,nocopy)
      character(kind=c_char,len=32), intent(in)    :: p
      character(kind=c_char,len=*),  intent(in)    :: key
      integer(KIND=c_long), asynchronous              :: val(:,:,:)
       type(dummy_type),   optional                 :: dummy
      type(plumed_error), optional,  intent(out)   :: error
      integer,            optional,  intent(out)   :: ierror
      logical,            optional,  intent(in)    :: nocopy
      type(plumed_error), target :: myerror
      integer(kind=c_int)    :: inocopy
      integer(kind=c_size_t) :: pass_shape(4)
      pass_shape(1)=size(val,3)
      pass_shape(2)=size(val,2)
      pass_shape(3)=size(val,1)
      pass_shape(4)=0
       if(present(nocopy)) then
         if(nocopy) then
           inocopy=1
         endif
       else
         inocopy=0
       endif
       if(present(error) .or. present(ierror)) then
         call plumed_f_cmd_safe_nothrow_long(p,key,val,pass_shape,inocopy,c_loc(myerror),c_funloc(error_handler))
         if(present(error)) then
           error=myerror
         endif
         if(present(ierror)) then
           ierror=myerror%code
         endif
       else
         call plumed_f_cmd_safe_nothrow_long(p,key,val,pass_shape,inocopy,c_null_ptr,c_null_ptr)
       endif
    end subroutine plumed_f_cmd_integer_2_3
    subroutine plumed_f_gcmd_integer_2_3(key,val,dummy,error,ierror,nocopy)
      character(kind=c_char,len=*),  intent(in)    :: key
      integer(kind=c_long), asynchronous              :: val(:,:,:)
      type(dummy_type),   optional                 :: dummy
      type(plumed_error), optional,  intent(out)   :: error
      integer,            optional,  intent(out)   :: ierror
      logical,            optional,  intent(in)    :: nocopy
      character(len=32) :: global
      call plumed_f_global(global)
      call plumed_f_cmd(global,key,val,error=error,ierror=ierror,nocopy=nocopy)
    end subroutine plumed_f_gcmd_integer_2_3
    subroutine plumed_f_cmd_integer_2_4(p,key,val,dummy,error,ierror,nocopy)
      character(kind=c_char,len=32), intent(in)    :: p
      character(kind=c_char,len=*),  intent(in)    :: key
      integer(KIND=c_long), asynchronous              :: val(:,:,:,:)
       type(dummy_type),   optional                 :: dummy
      type(plumed_error), optional,  intent(out)   :: error
      integer,            optional,  intent(out)   :: ierror
      logical,            optional,  intent(in)    :: nocopy
      type(plumed_error), target :: myerror
      integer(kind=c_int)    :: inocopy
      integer(kind=c_size_t) :: pass_shape(5)
      pass_shape(1)=size(val,4)
      pass_shape(2)=size(val,3)
      pass_shape(3)=size(val,2)
      pass_shape(4)=size(val,1)
      pass_shape(5)=0
       if(present(nocopy)) then
         if(nocopy) then
           inocopy=1
         endif
       else
         inocopy=0
       endif
       if(present(error) .or. present(ierror)) then
         call plumed_f_cmd_safe_nothrow_long(p,key,val,pass_shape,inocopy,c_loc(myerror),c_funloc(error_handler))
         if(present(error)) then
           error=myerror
         endif
         if(present(ierror)) then
           ierror=myerror%code
         endif
       else
         call plumed_f_cmd_safe_nothrow_long(p,key,val,pass_shape,inocopy,c_null_ptr,c_null_ptr)
       endif
    end subroutine plumed_f_cmd_integer_2_4
    subroutine plumed_f_gcmd_integer_2_4(key,val,dummy,error,ierror,nocopy)
      character(kind=c_char,len=*),  intent(in)    :: key
      integer(kind=c_long), asynchronous              :: val(:,:,:,:)
      type(dummy_type),   optional                 :: dummy
      type(plumed_error), optional,  intent(out)   :: error
      integer,            optional,  intent(out)   :: ierror
      logical,            optional,  intent(in)    :: nocopy
      character(len=32) :: global
      call plumed_f_global(global)
      call plumed_f_cmd(global,key,val,error=error,ierror=ierror,nocopy=nocopy)
    end subroutine plumed_f_gcmd_integer_2_4
    subroutine plumed_f_cmd_real_0_0(p,key,val,dummy,error,ierror,nocopy)
      character(kind=c_char,len=32), intent(in)    :: p
      character(kind=c_char,len=*),  intent(in)    :: key
      real(KIND=c_float), asynchronous              :: val
       type(dummy_type),   optional                 :: dummy
      type(plumed_error), optional,  intent(out)   :: error
      integer,            optional,  intent(out)   :: ierror
      logical,            optional,  intent(in)    :: nocopy
      type(plumed_error), target :: myerror
      integer(kind=c_int)    :: inocopy
      integer(kind=c_size_t) :: pass_shape(2)
      pass_shape=(/1,0/)
       if(present(nocopy)) then
         if(nocopy) then
           inocopy=1
         endif
       else
         inocopy=0
       endif
       if(present(error) .or. present(ierror)) then
         call plumed_f_cmd_safe_nothrow_float_scalar(p,key,val,pass_shape,inocopy,c_loc(myerror),c_funloc(error_handler))
         if(present(error)) then
           error=myerror
         endif
         if(present(ierror)) then
           ierror=myerror%code
         endif
       else
         call plumed_f_cmd_safe_nothrow_float_scalar(p,key,val,pass_shape,inocopy,c_null_ptr,c_null_ptr)
       endif
    end subroutine plumed_f_cmd_real_0_0
    subroutine plumed_f_gcmd_real_0_0(key,val,dummy,error,ierror,nocopy)
      character(kind=c_char,len=*),  intent(in)    :: key
      real(kind=c_float), asynchronous              :: val
      type(dummy_type),   optional                 :: dummy
      type(plumed_error), optional,  intent(out)   :: error
      integer,            optional,  intent(out)   :: ierror
      logical,            optional,  intent(in)    :: nocopy
      character(len=32) :: global
      call plumed_f_global(global)
      call plumed_f_cmd(global,key,val,error=error,ierror=ierror,nocopy=nocopy)
    end subroutine plumed_f_gcmd_real_0_0
    subroutine plumed_f_cmd_real_0_1(p,key,val,dummy,error,ierror,nocopy)
      character(kind=c_char,len=32), intent(in)    :: p
      character(kind=c_char,len=*),  intent(in)    :: key
      real(KIND=c_float), asynchronous              :: val(:)
       type(dummy_type),   optional                 :: dummy
      type(plumed_error), optional,  intent(out)   :: error
      integer,            optional,  intent(out)   :: ierror
      logical,            optional,  intent(in)    :: nocopy
      type(plumed_error), target :: myerror
      integer(kind=c_int)    :: inocopy
      integer(kind=c_size_t) :: pass_shape(2)
      pass_shape(1)=size(val,1)
      pass_shape(2)=0
       if(present(nocopy)) then
         if(nocopy) then
           inocopy=1
         endif
       else
         inocopy=0
       endif
       if(present(error) .or. present(ierror)) then
         call plumed_f_cmd_safe_nothrow_float(p,key,val,pass_shape,inocopy,c_loc(myerror),c_funloc(error_handler))
         if(present(error)) then
           error=myerror
         endif
         if(present(ierror)) then
           ierror=myerror%code
         endif
       else
         call plumed_f_cmd_safe_nothrow_float(p,key,val,pass_shape,inocopy,c_null_ptr,c_null_ptr)
       endif
    end subroutine plumed_f_cmd_real_0_1
    subroutine plumed_f_gcmd_real_0_1(key,val,dummy,error,ierror,nocopy)
      character(kind=c_char,len=*),  intent(in)    :: key
      real(kind=c_float), asynchronous              :: val(:)
      type(dummy_type),   optional                 :: dummy
      type(plumed_error), optional,  intent(out)   :: error
      integer,            optional,  intent(out)   :: ierror
      logical,            optional,  intent(in)    :: nocopy
      character(len=32) :: global
      call plumed_f_global(global)
      call plumed_f_cmd(global,key,val,error=error,ierror=ierror,nocopy=nocopy)
    end subroutine plumed_f_gcmd_real_0_1
    subroutine plumed_f_cmd_real_0_2(p,key,val,dummy,error,ierror,nocopy)
      character(kind=c_char,len=32), intent(in)    :: p
      character(kind=c_char,len=*),  intent(in)    :: key
      real(KIND=c_float), asynchronous              :: val(:,:)
       type(dummy_type),   optional                 :: dummy
      type(plumed_error), optional,  intent(out)   :: error
      integer,            optional,  intent(out)   :: ierror
      logical,            optional,  intent(in)    :: nocopy
      type(plumed_error), target :: myerror
      integer(kind=c_int)    :: inocopy
      integer(kind=c_size_t) :: pass_shape(3)
      pass_shape(1)=size(val,2)
      pass_shape(2)=size(val,1)
      pass_shape(3)=0
       if(present(nocopy)) then
         if(nocopy) then
           inocopy=1
         endif
       else
         inocopy=0
       endif
       if(present(error) .or. present(ierror)) then
         call plumed_f_cmd_safe_nothrow_float(p,key,val,pass_shape,inocopy,c_loc(myerror),c_funloc(error_handler))
         if(present(error)) then
           error=myerror
         endif
         if(present(ierror)) then
           ierror=myerror%code
         endif
       else
         call plumed_f_cmd_safe_nothrow_float(p,key,val,pass_shape,inocopy,c_null_ptr,c_null_ptr)
       endif
    end subroutine plumed_f_cmd_real_0_2
    subroutine plumed_f_gcmd_real_0_2(key,val,dummy,error,ierror,nocopy)
      character(kind=c_char,len=*),  intent(in)    :: key
      real(kind=c_float), asynchronous              :: val(:,:)
      type(dummy_type),   optional                 :: dummy
      type(plumed_error), optional,  intent(out)   :: error
      integer,            optional,  intent(out)   :: ierror
      logical,            optional,  intent(in)    :: nocopy
      character(len=32) :: global
      call plumed_f_global(global)
      call plumed_f_cmd(global,key,val,error=error,ierror=ierror,nocopy=nocopy)
    end subroutine plumed_f_gcmd_real_0_2
    subroutine plumed_f_cmd_real_0_3(p,key,val,dummy,error,ierror,nocopy)
      character(kind=c_char,len=32), intent(in)    :: p
      character(kind=c_char,len=*),  intent(in)    :: key
      real(KIND=c_float), asynchronous              :: val(:,:,:)
       type(dummy_type),   optional                 :: dummy
      type(plumed_error), optional,  intent(out)   :: error
      integer,            optional,  intent(out)   :: ierror
      logical,            optional,  intent(in)    :: nocopy
      type(plumed_error), target :: myerror
      integer(kind=c_int)    :: inocopy
      integer(kind=c_size_t) :: pass_shape(4)
      pass_shape(1)=size(val,3)
      pass_shape(2)=size(val,2)
      pass_shape(3)=size(val,1)
      pass_shape(4)=0
       if(present(nocopy)) then
         if(nocopy) then
           inocopy=1
         endif
       else
         inocopy=0
       endif
       if(present(error) .or. present(ierror)) then
         call plumed_f_cmd_safe_nothrow_float(p,key,val,pass_shape,inocopy,c_loc(myerror),c_funloc(error_handler))
         if(present(error)) then
           error=myerror
         endif
         if(present(ierror)) then
           ierror=myerror%code
         endif
       else
         call plumed_f_cmd_safe_nothrow_float(p,key,val,pass_shape,inocopy,c_null_ptr,c_null_ptr)
       endif
    end subroutine plumed_f_cmd_real_0_3
    subroutine plumed_f_gcmd_real_0_3(key,val,dummy,error,ierror,nocopy)
      character(kind=c_char,len=*),  intent(in)    :: key
      real(kind=c_float), asynchronous              :: val(:,:,:)
      type(dummy_type),   optional                 :: dummy
      type(plumed_error), optional,  intent(out)   :: error
      integer,            optional,  intent(out)   :: ierror
      logical,            optional,  intent(in)    :: nocopy
      character(len=32) :: global
      call plumed_f_global(global)
      call plumed_f_cmd(global,key,val,error=error,ierror=ierror,nocopy=nocopy)
    end subroutine plumed_f_gcmd_real_0_3
    subroutine plumed_f_cmd_real_0_4(p,key,val,dummy,error,ierror,nocopy)
      character(kind=c_char,len=32), intent(in)    :: p
      character(kind=c_char,len=*),  intent(in)    :: key
      real(KIND=c_float), asynchronous              :: val(:,:,:,:)
       type(dummy_type),   optional                 :: dummy
      type(plumed_error), optional,  intent(out)   :: error
      integer,            optional,  intent(out)   :: ierror
      logical,            optional,  intent(in)    :: nocopy
      type(plumed_error), target :: myerror
      integer(kind=c_int)    :: inocopy
      integer(kind=c_size_t) :: pass_shape(5)
      pass_shape(1)=size(val,4)
      pass_shape(2)=size(val,3)
      pass_shape(3)=size(val,2)
      pass_shape(4)=size(val,1)
      pass_shape(5)=0
       if(present(nocopy)) then
         if(nocopy) then
           inocopy=1
         endif
       else
         inocopy=0
       endif
       if(present(error) .or. present(ierror)) then
         call plumed_f_cmd_safe_nothrow_float(p,key,val,pass_shape,inocopy,c_loc(myerror),c_funloc(error_handler))
         if(present(error)) then
           error=myerror
         endif
         if(present(ierror)) then
           ierror=myerror%code
         endif
       else
         call plumed_f_cmd_safe_nothrow_float(p,key,val,pass_shape,inocopy,c_null_ptr,c_null_ptr)
       endif
    end subroutine plumed_f_cmd_real_0_4
    subroutine plumed_f_gcmd_real_0_4(key,val,dummy,error,ierror,nocopy)
      character(kind=c_char,len=*),  intent(in)    :: key
      real(kind=c_float), asynchronous              :: val(:,:,:,:)
      type(dummy_type),   optional                 :: dummy
      type(plumed_error), optional,  intent(out)   :: error
      integer,            optional,  intent(out)   :: ierror
      logical,            optional,  intent(in)    :: nocopy
      character(len=32) :: global
      call plumed_f_global(global)
      call plumed_f_cmd(global,key,val,error=error,ierror=ierror,nocopy=nocopy)
    end subroutine plumed_f_gcmd_real_0_4
    subroutine plumed_f_cmd_real_1_0(p,key,val,dummy,error,ierror,nocopy)
      character(kind=c_char,len=32), intent(in)    :: p
      character(kind=c_char,len=*),  intent(in)    :: key
      real(KIND=c_double), asynchronous              :: val
       type(dummy_type),   optional                 :: dummy
      type(plumed_error), optional,  intent(out)   :: error
      integer,            optional,  intent(out)   :: ierror
      logical,            optional,  intent(in)    :: nocopy
      type(plumed_error), target :: myerror
      integer(kind=c_int)    :: inocopy
      integer(kind=c_size_t) :: pass_shape(2)
      pass_shape=(/1,0/)
       if(present(nocopy)) then
         if(nocopy) then
           inocopy=1
         endif
       else
         inocopy=0
       endif
       if(present(error) .or. present(ierror)) then
         call plumed_f_cmd_safe_nothrow_double_scalar(p,key,val,pass_shape,inocopy,c_loc(myerror),c_funloc(error_handler))
         if(present(error)) then
           error=myerror
         endif
         if(present(ierror)) then
           ierror=myerror%code
         endif
       else
         call plumed_f_cmd_safe_nothrow_double_scalar(p,key,val,pass_shape,inocopy,c_null_ptr,c_null_ptr)
       endif
    end subroutine plumed_f_cmd_real_1_0
    subroutine plumed_f_gcmd_real_1_0(key,val,dummy,error,ierror,nocopy)
      character(kind=c_char,len=*),  intent(in)    :: key
      real(kind=c_double), asynchronous              :: val
      type(dummy_type),   optional                 :: dummy
      type(plumed_error), optional,  intent(out)   :: error
      integer,            optional,  intent(out)   :: ierror
      logical,            optional,  intent(in)    :: nocopy
      character(len=32) :: global
      call plumed_f_global(global)
      call plumed_f_cmd(global,key,val,error=error,ierror=ierror,nocopy=nocopy)
    end subroutine plumed_f_gcmd_real_1_0
    subroutine plumed_f_cmd_real_1_1(p,key,val,dummy,error,ierror,nocopy)
      character(kind=c_char,len=32), intent(in)    :: p
      character(kind=c_char,len=*),  intent(in)    :: key
      real(KIND=c_double), asynchronous              :: val(:)
       type(dummy_type),   optional                 :: dummy
      type(plumed_error), optional,  intent(out)   :: error
      integer,            optional,  intent(out)   :: ierror
      logical,            optional,  intent(in)    :: nocopy
      type(plumed_error), target :: myerror
      integer(kind=c_int)    :: inocopy
      integer(kind=c_size_t) :: pass_shape(2)
      pass_shape(1)=size(val,1)
      pass_shape(2)=0
       if(present(nocopy)) then
         if(nocopy) then
           inocopy=1
         endif
       else
         inocopy=0
       endif
       if(present(error) .or. present(ierror)) then
         call plumed_f_cmd_safe_nothrow_double(p,key,val,pass_shape,inocopy,c_loc(myerror),c_funloc(error_handler))
         if(present(error)) then
           error=myerror
         endif
         if(present(ierror)) then
           ierror=myerror%code
         endif
       else
         call plumed_f_cmd_safe_nothrow_double(p,key,val,pass_shape,inocopy,c_null_ptr,c_null_ptr)
       endif
    end subroutine plumed_f_cmd_real_1_1
    subroutine plumed_f_gcmd_real_1_1(key,val,dummy,error,ierror,nocopy)
      character(kind=c_char,len=*),  intent(in)    :: key
      real(kind=c_double), asynchronous              :: val(:)
      type(dummy_type),   optional                 :: dummy
      type(plumed_error), optional,  intent(out)   :: error
      integer,            optional,  intent(out)   :: ierror
      logical,            optional,  intent(in)    :: nocopy
      character(len=32) :: global
      call plumed_f_global(global)
      call plumed_f_cmd(global,key,val,error=error,ierror=ierror,nocopy=nocopy)
    end subroutine plumed_f_gcmd_real_1_1
    subroutine plumed_f_cmd_real_1_2(p,key,val,dummy,error,ierror,nocopy)
      character(kind=c_char,len=32), intent(in)    :: p
      character(kind=c_char,len=*),  intent(in)    :: key
      real(KIND=c_double), asynchronous              :: val(:,:)
       type(dummy_type),   optional                 :: dummy
      type(plumed_error), optional,  intent(out)   :: error
      integer,            optional,  intent(out)   :: ierror
      logical,            optional,  intent(in)    :: nocopy
      type(plumed_error), target :: myerror
      integer(kind=c_int)    :: inocopy
      integer(kind=c_size_t) :: pass_shape(3)
      pass_shape(1)=size(val,2)
      pass_shape(2)=size(val,1)
      pass_shape(3)=0
       if(present(nocopy)) then
         if(nocopy) then
           inocopy=1
         endif
       else
         inocopy=0
       endif
       if(present(error) .or. present(ierror)) then
         call plumed_f_cmd_safe_nothrow_double(p,key,val,pass_shape,inocopy,c_loc(myerror),c_funloc(error_handler))
         if(present(error)) then
           error=myerror
         endif
         if(present(ierror)) then
           ierror=myerror%code
         endif
       else
         call plumed_f_cmd_safe_nothrow_double(p,key,val,pass_shape,inocopy,c_null_ptr,c_null_ptr)
       endif
    end subroutine plumed_f_cmd_real_1_2
    subroutine plumed_f_gcmd_real_1_2(key,val,dummy,error,ierror,nocopy)
      character(kind=c_char,len=*),  intent(in)    :: key
      real(kind=c_double), asynchronous              :: val(:,:)
      type(dummy_type),   optional                 :: dummy
      type(plumed_error), optional,  intent(out)   :: error
      integer,            optional,  intent(out)   :: ierror
      logical,            optional,  intent(in)    :: nocopy
      character(len=32) :: global
      call plumed_f_global(global)
      call plumed_f_cmd(global,key,val,error=error,ierror=ierror,nocopy=nocopy)
    end subroutine plumed_f_gcmd_real_1_2
    subroutine plumed_f_cmd_real_1_3(p,key,val,dummy,error,ierror,nocopy)
      character(kind=c_char,len=32), intent(in)    :: p
      character(kind=c_char,len=*),  intent(in)    :: key
      real(KIND=c_double), asynchronous              :: val(:,:,:)
       type(dummy_type),   optional                 :: dummy
      type(plumed_error), optional,  intent(out)   :: error
      integer,            optional,  intent(out)   :: ierror
      logical,            optional,  intent(in)    :: nocopy
      type(plumed_error), target :: myerror
      integer(kind=c_int)    :: inocopy
      integer(kind=c_size_t) :: pass_shape(4)
      pass_shape(1)=size(val,3)
      pass_shape(2)=size(val,2)
      pass_shape(3)=size(val,1)
      pass_shape(4)=0
       if(present(nocopy)) then
         if(nocopy) then
           inocopy=1
         endif
       else
         inocopy=0
       endif
       if(present(error) .or. present(ierror)) then
         call plumed_f_cmd_safe_nothrow_double(p,key,val,pass_shape,inocopy,c_loc(myerror),c_funloc(error_handler))
         if(present(error)) then
           error=myerror
         endif
         if(present(ierror)) then
           ierror=myerror%code
         endif
       else
         call plumed_f_cmd_safe_nothrow_double(p,key,val,pass_shape,inocopy,c_null_ptr,c_null_ptr)
       endif
    end subroutine plumed_f_cmd_real_1_3
    subroutine plumed_f_gcmd_real_1_3(key,val,dummy,error,ierror,nocopy)
      character(kind=c_char,len=*),  intent(in)    :: key
      real(kind=c_double), asynchronous              :: val(:,:,:)
      type(dummy_type),   optional                 :: dummy
      type(plumed_error), optional,  intent(out)   :: error
      integer,            optional,  intent(out)   :: ierror
      logical,            optional,  intent(in)    :: nocopy
      character(len=32) :: global
      call plumed_f_global(global)
      call plumed_f_cmd(global,key,val,error=error,ierror=ierror,nocopy=nocopy)
    end subroutine plumed_f_gcmd_real_1_3
    subroutine plumed_f_cmd_real_1_4(p,key,val,dummy,error,ierror,nocopy)
      character(kind=c_char,len=32), intent(in)    :: p
      character(kind=c_char,len=*),  intent(in)    :: key
      real(KIND=c_double), asynchronous              :: val(:,:,:,:)
       type(dummy_type),   optional                 :: dummy
      type(plumed_error), optional,  intent(out)   :: error
      integer,            optional,  intent(out)   :: ierror
      logical,            optional,  intent(in)    :: nocopy
      type(plumed_error), target :: myerror
      integer(kind=c_int)    :: inocopy
      integer(kind=c_size_t) :: pass_shape(5)
      pass_shape(1)=size(val,4)
      pass_shape(2)=size(val,3)
      pass_shape(3)=size(val,2)
      pass_shape(4)=size(val,1)
      pass_shape(5)=0
       if(present(nocopy)) then
         if(nocopy) then
           inocopy=1
         endif
       else
         inocopy=0
       endif
       if(present(error) .or. present(ierror)) then
         call plumed_f_cmd_safe_nothrow_double(p,key,val,pass_shape,inocopy,c_loc(myerror),c_funloc(error_handler))
         if(present(error)) then
           error=myerror
         endif
         if(present(ierror)) then
           ierror=myerror%code
         endif
       else
         call plumed_f_cmd_safe_nothrow_double(p,key,val,pass_shape,inocopy,c_null_ptr,c_null_ptr)
       endif
    end subroutine plumed_f_cmd_real_1_4
    subroutine plumed_f_gcmd_real_1_4(key,val,dummy,error,ierror,nocopy)
      character(kind=c_char,len=*),  intent(in)    :: key
      real(kind=c_double), asynchronous              :: val(:,:,:,:)
      type(dummy_type),   optional                 :: dummy
      type(plumed_error), optional,  intent(out)   :: error
      integer,            optional,  intent(out)   :: ierror
      logical,            optional,  intent(in)    :: nocopy
      character(len=32) :: global
      call plumed_f_global(global)
      call plumed_f_cmd(global,key,val,error=error,ierror=ierror,nocopy=nocopy)
    end subroutine plumed_f_gcmd_real_1_4
    subroutine plumed_f_cmd_real_2_0(p,key,val,dummy,error,ierror,nocopy)
      character(kind=c_char,len=32), intent(in)    :: p
      character(kind=c_char,len=*),  intent(in)    :: key
      real(KIND=c_long_double), asynchronous              :: val
       type(dummy_type),   optional                 :: dummy
      type(plumed_error), optional,  intent(out)   :: error
      integer,            optional,  intent(out)   :: ierror
      logical,            optional,  intent(in)    :: nocopy
      type(plumed_error), target :: myerror
      integer(kind=c_int)    :: inocopy
      integer(kind=c_size_t) :: pass_shape(2)
      pass_shape=(/1,0/)
       if(present(nocopy)) then
         if(nocopy) then
           inocopy=1
         endif
       else
         inocopy=0
       endif
       if(present(error) .or. present(ierror)) then
         call plumed_f_cmd_safe_nothrow_long_double_scalar(p,key,val,pass_shape,inocopy,c_loc(myerror),c_funloc(error_handler))
         if(present(error)) then
           error=myerror
         endif
         if(present(ierror)) then
           ierror=myerror%code
         endif
       else
         call plumed_f_cmd_safe_nothrow_long_double_scalar(p,key,val,pass_shape,inocopy,c_null_ptr,c_null_ptr)
       endif
    end subroutine plumed_f_cmd_real_2_0
    subroutine plumed_f_gcmd_real_2_0(key,val,dummy,error,ierror,nocopy)
      character(kind=c_char,len=*),  intent(in)    :: key
      real(kind=c_long_double), asynchronous              :: val
      type(dummy_type),   optional                 :: dummy
      type(plumed_error), optional,  intent(out)   :: error
      integer,            optional,  intent(out)   :: ierror
      logical,            optional,  intent(in)    :: nocopy
      character(len=32) :: global
      call plumed_f_global(global)
      call plumed_f_cmd(global,key,val,error=error,ierror=ierror,nocopy=nocopy)
    end subroutine plumed_f_gcmd_real_2_0
    subroutine plumed_f_cmd_real_2_1(p,key,val,dummy,error,ierror,nocopy)
      character(kind=c_char,len=32), intent(in)    :: p
      character(kind=c_char,len=*),  intent(in)    :: key
      real(KIND=c_long_double), asynchronous              :: val(:)
       type(dummy_type),   optional                 :: dummy
      type(plumed_error), optional,  intent(out)   :: error
      integer,            optional,  intent(out)   :: ierror
      logical,            optional,  intent(in)    :: nocopy
      type(plumed_error), target :: myerror
      integer(kind=c_int)    :: inocopy
      integer(kind=c_size_t) :: pass_shape(2)
      pass_shape(1)=size(val,1)
      pass_shape(2)=0
       if(present(nocopy)) then
         if(nocopy) then
           inocopy=1
         endif
       else
         inocopy=0
       endif
       if(present(error) .or. present(ierror)) then
         call plumed_f_cmd_safe_nothrow_long_double(p,key,val,pass_shape,inocopy,c_loc(myerror),c_funloc(error_handler))
         if(present(error)) then
           error=myerror
         endif
         if(present(ierror)) then
           ierror=myerror%code
         endif
       else
         call plumed_f_cmd_safe_nothrow_long_double(p,key,val,pass_shape,inocopy,c_null_ptr,c_null_ptr)
       endif
    end subroutine plumed_f_cmd_real_2_1
    subroutine plumed_f_gcmd_real_2_1(key,val,dummy,error,ierror,nocopy)
      character(kind=c_char,len=*),  intent(in)    :: key
      real(kind=c_long_double), asynchronous              :: val(:)
      type(dummy_type),   optional                 :: dummy
      type(plumed_error), optional,  intent(out)   :: error
      integer,            optional,  intent(out)   :: ierror
      logical,            optional,  intent(in)    :: nocopy
      character(len=32) :: global
      call plumed_f_global(global)
      call plumed_f_cmd(global,key,val,error=error,ierror=ierror,nocopy=nocopy)
    end subroutine plumed_f_gcmd_real_2_1
    subroutine plumed_f_cmd_real_2_2(p,key,val,dummy,error,ierror,nocopy)
      character(kind=c_char,len=32), intent(in)    :: p
      character(kind=c_char,len=*),  intent(in)    :: key
      real(KIND=c_long_double), asynchronous              :: val(:,:)
       type(dummy_type),   optional                 :: dummy
      type(plumed_error), optional,  intent(out)   :: error
      integer,            optional,  intent(out)   :: ierror
      logical,            optional,  intent(in)    :: nocopy
      type(plumed_error), target :: myerror
      integer(kind=c_int)    :: inocopy
      integer(kind=c_size_t) :: pass_shape(3)
      pass_shape(1)=size(val,2)
      pass_shape(2)=size(val,1)
      pass_shape(3)=0
       if(present(nocopy)) then
         if(nocopy) then
           inocopy=1
         endif
       else
         inocopy=0
       endif
       if(present(error) .or. present(ierror)) then
         call plumed_f_cmd_safe_nothrow_long_double(p,key,val,pass_shape,inocopy,c_loc(myerror),c_funloc(error_handler))
         if(present(error)) then
           error=myerror
         endif
         if(present(ierror)) then
           ierror=myerror%code
         endif
       else
         call plumed_f_cmd_safe_nothrow_long_double(p,key,val,pass_shape,inocopy,c_null_ptr,c_null_ptr)
       endif
    end subroutine plumed_f_cmd_real_2_2
    subroutine plumed_f_gcmd_real_2_2(key,val,dummy,error,ierror,nocopy)
      character(kind=c_char,len=*),  intent(in)    :: key
      real(kind=c_long_double), asynchronous              :: val(:,:)
      type(dummy_type),   optional                 :: dummy
      type(plumed_error), optional,  intent(out)   :: error
      integer,            optional,  intent(out)   :: ierror
      logical,            optional,  intent(in)    :: nocopy
      character(len=32) :: global
      call plumed_f_global(global)
      call plumed_f_cmd(global,key,val,error=error,ierror=ierror,nocopy=nocopy)
    end subroutine plumed_f_gcmd_real_2_2
    subroutine plumed_f_cmd_real_2_3(p,key,val,dummy,error,ierror,nocopy)
      character(kind=c_char,len=32), intent(in)    :: p
      character(kind=c_char,len=*),  intent(in)    :: key
      real(KIND=c_long_double), asynchronous              :: val(:,:,:)
       type(dummy_type),   optional                 :: dummy
      type(plumed_error), optional,  intent(out)   :: error
      integer,            optional,  intent(out)   :: ierror
      logical,            optional,  intent(in)    :: nocopy
      type(plumed_error), target :: myerror
      integer(kind=c_int)    :: inocopy
      integer(kind=c_size_t) :: pass_shape(4)
      pass_shape(1)=size(val,3)
      pass_shape(2)=size(val,2)
      pass_shape(3)=size(val,1)
      pass_shape(4)=0
       if(present(nocopy)) then
         if(nocopy) then
           inocopy=1
         endif
       else
         inocopy=0
       endif
       if(present(error) .or. present(ierror)) then
         call plumed_f_cmd_safe_nothrow_long_double(p,key,val,pass_shape,inocopy,c_loc(myerror),c_funloc(error_handler))
         if(present(error)) then
           error=myerror
         endif
         if(present(ierror)) then
           ierror=myerror%code
         endif
       else
         call plumed_f_cmd_safe_nothrow_long_double(p,key,val,pass_shape,inocopy,c_null_ptr,c_null_ptr)
       endif
    end subroutine plumed_f_cmd_real_2_3
    subroutine plumed_f_gcmd_real_2_3(key,val,dummy,error,ierror,nocopy)
      character(kind=c_char,len=*),  intent(in)    :: key
      real(kind=c_long_double), asynchronous              :: val(:,:,:)
      type(dummy_type),   optional                 :: dummy
      type(plumed_error), optional,  intent(out)   :: error
      integer,            optional,  intent(out)   :: ierror
      logical,            optional,  intent(in)    :: nocopy
      character(len=32) :: global
      call plumed_f_global(global)
      call plumed_f_cmd(global,key,val,error=error,ierror=ierror,nocopy=nocopy)
    end subroutine plumed_f_gcmd_real_2_3
    subroutine plumed_f_cmd_real_2_4(p,key,val,dummy,error,ierror,nocopy)
      character(kind=c_char,len=32), intent(in)    :: p
      character(kind=c_char,len=*),  intent(in)    :: key
      real(KIND=c_long_double), asynchronous              :: val(:,:,:,:)
       type(dummy_type),   optional                 :: dummy
      type(plumed_error), optional,  intent(out)   :: error
      integer,            optional,  intent(out)   :: ierror
      logical,            optional,  intent(in)    :: nocopy
      type(plumed_error), target :: myerror
      integer(kind=c_int)    :: inocopy
      integer(kind=c_size_t) :: pass_shape(5)
      pass_shape(1)=size(val,4)
      pass_shape(2)=size(val,3)
      pass_shape(3)=size(val,2)
      pass_shape(4)=size(val,1)
      pass_shape(5)=0
       if(present(nocopy)) then
         if(nocopy) then
           inocopy=1
         endif
       else
         inocopy=0
       endif
       if(present(error) .or. present(ierror)) then
         call plumed_f_cmd_safe_nothrow_long_double(p,key,val,pass_shape,inocopy,c_loc(myerror),c_funloc(error_handler))
         if(present(error)) then
           error=myerror
         endif
         if(present(ierror)) then
           ierror=myerror%code
         endif
       else
         call plumed_f_cmd_safe_nothrow_long_double(p,key,val,pass_shape,inocopy,c_null_ptr,c_null_ptr)
       endif
    end subroutine plumed_f_cmd_real_2_4
    subroutine plumed_f_gcmd_real_2_4(key,val,dummy,error,ierror,nocopy)
      character(kind=c_char,len=*),  intent(in)    :: key
      real(kind=c_long_double), asynchronous              :: val(:,:,:,:)
      type(dummy_type),   optional                 :: dummy
      type(plumed_error), optional,  intent(out)   :: error
      integer,            optional,  intent(out)   :: ierror
      logical,            optional,  intent(in)    :: nocopy
      character(len=32) :: global
      call plumed_f_global(global)
      call plumed_f_cmd(global,key,val,error=error,ierror=ierror,nocopy=nocopy)
    end subroutine plumed_f_gcmd_real_2_4

end module plumed_module

