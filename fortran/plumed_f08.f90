! vim:ft=fortran



module plumed_module_f08
  use iso_c_binding
  implicit none

  private

  public :: plumed
  public :: plumed_create
  public :: plumed_installed
  public :: plumed_error

  type dummy_type
  end type dummy_type

  type plumed
    character(kind=c_char,len=32), private :: handle
    logical,                       private :: initialized = .false.
  contains 
    private
    generic, public :: cmd => &
    pl_cmd_integer_0_0, &
    pl_cmd_integer_0_1, &
    pl_cmd_integer_0_2, &
    pl_cmd_integer_0_3, &
    pl_cmd_integer_0_4, &
    pl_cmd_integer_1_0, &
    pl_cmd_integer_1_1, &
    pl_cmd_integer_1_2, &
    pl_cmd_integer_1_3, &
    pl_cmd_integer_1_4, &
    pl_cmd_integer_2_0, &
    pl_cmd_integer_2_1, &
    pl_cmd_integer_2_2, &
    pl_cmd_integer_2_3, &
    pl_cmd_integer_2_4, &
    pl_cmd_real_0_0, &
    pl_cmd_real_0_1, &
    pl_cmd_real_0_2, &
    pl_cmd_real_0_3, &
    pl_cmd_real_0_4, &
    pl_cmd_real_1_0, &
    pl_cmd_real_1_1, &
    pl_cmd_real_1_2, &
    pl_cmd_real_1_3, &
    pl_cmd_real_1_4, &
    pl_cmd_real_2_0, &
    pl_cmd_real_2_1, &
    pl_cmd_real_2_2, &
    pl_cmd_real_2_3, &
    pl_cmd_real_2_4, &
    pl_cmd, &
    pl_cmd_char, &
    pl_cmd_ptr

    procedure :: pl_cmd_integer_0_0
    procedure :: pl_cmd_integer_0_1
    procedure :: pl_cmd_integer_0_2
    procedure :: pl_cmd_integer_0_3
    procedure :: pl_cmd_integer_0_4
    procedure :: pl_cmd_integer_1_0
    procedure :: pl_cmd_integer_1_1
    procedure :: pl_cmd_integer_1_2
    procedure :: pl_cmd_integer_1_3
    procedure :: pl_cmd_integer_1_4
    procedure :: pl_cmd_integer_2_0
    procedure :: pl_cmd_integer_2_1
    procedure :: pl_cmd_integer_2_2
    procedure :: pl_cmd_integer_2_3
    procedure :: pl_cmd_integer_2_4
    procedure :: pl_cmd_real_0_0
    procedure :: pl_cmd_real_0_1
    procedure :: pl_cmd_real_0_2
    procedure :: pl_cmd_real_0_3
    procedure :: pl_cmd_real_0_4
    procedure :: pl_cmd_real_1_0
    procedure :: pl_cmd_real_1_1
    procedure :: pl_cmd_real_1_2
    procedure :: pl_cmd_real_1_3
    procedure :: pl_cmd_real_1_4
    procedure :: pl_cmd_real_2_0
    procedure :: pl_cmd_real_2_1
    procedure :: pl_cmd_real_2_2
    procedure :: pl_cmd_real_2_3
    procedure :: pl_cmd_real_2_4
    procedure :: pl_cmd
    procedure :: pl_cmd_char
    procedure :: pl_cmd_ptr

    procedure, public :: finalize => pl_finalize
    procedure, public :: incref => pl_incref
    procedure, public :: decref => pl_decref
    generic,   public :: assignment(=) => pl_assign
    final     :: pl_destructor
    procedure, public :: valid => pl_valid
    procedure, public :: use_count => pl_use_count
    procedure :: pl_assign
  end type plumed

  type :: plumed_error
    integer                         :: code=0
    character(len = :), allocatable :: what
  end type plumed_error

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
    subroutine plumed_f_cmd_safe_nothrow_ptr(p,key,val,pass_shape,const,nocopy,e,eh) bind(C)
      import
      character(kind=c_char), intent(in)    :: p(32)
      character(kind=c_char), intent(in)    :: key(*)
      type(c_ptr),                    value :: val
      integer(kind=c_size_t) :: pass_shape(*)
      integer(kind=c_int),            value :: const
      integer(kind=c_int),            value :: nocopy
      type(c_ptr),                    value :: e
      type(c_funptr),                 value :: eh
    end subroutine plumed_f_cmd_safe_nothrow_ptr
  end interface

  interface
    subroutine plumed_f_cmd_safe_nothrow_char(p,key,val,pass_shape,const,nocopy,e,eh) bind(C)
      import
      character(kind=c_char), intent(in)    :: p(32)
      character(kind=c_char), intent(in)    :: key(*)
      character(kind=c_char)                :: val(*)
      integer(kind=c_size_t) :: pass_shape(*)
      integer(kind=c_int),            value :: const
      integer(kind=c_int),            value :: nocopy
      type(c_ptr),                    value :: e
      type(c_funptr),                 value :: eh
    end subroutine plumed_f_cmd_safe_nothrow_char
  end interface

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

  interface
    subroutine plumed_f_cmd_safe_nothrow_int_scalar(p,key,val,pass_shape,const,nocopy,e,eh)&
        & bind(C,name="plumed_f_cmd_safe_nothrow_int")
      import
      character(kind=c_char), intent(in)    :: p(32)
      character(kind=c_char), intent(in)    :: key(*)
      integer(kind=c_int)                           :: val
      integer(kind=c_size_t) :: pass_shape(*)
      integer(kind=c_int),            value :: const
      integer(kind=c_int),            value :: nocopy
      type(c_ptr),                    value :: e
      type(c_funptr),                 value :: eh
    end subroutine plumed_f_cmd_safe_nothrow_int_scalar
  end interface
  interface
    subroutine plumed_f_cmd_safe_nothrow_int(p,key,val,pass_shape,const,nocopy,e,eh) bind(C,name="plumed_f_cmd_safe_nothrow_int")
      import
      character(kind=c_char), intent(in)    :: p(32)
      character(kind=c_char), intent(in)    :: key(*)
      integer(kind=c_int)                           :: val(*)
      integer(kind=c_size_t) :: pass_shape(*)
      integer(kind=c_int),            value :: const
      integer(kind=c_int),            value :: nocopy
      type(c_ptr),                    value :: e
      type(c_funptr),                 value :: eh
    end subroutine plumed_f_cmd_safe_nothrow_int
  end interface
  interface
    subroutine plumed_f_cmd_safe_nothrow_short_scalar(p,key,val,pass_shape,const,nocopy,e,eh)&
        & bind(C,name="plumed_f_cmd_safe_nothrow_short")
      import
      character(kind=c_char), intent(in)    :: p(32)
      character(kind=c_char), intent(in)    :: key(*)
      integer(kind=c_short)                           :: val
      integer(kind=c_size_t) :: pass_shape(*)
      integer(kind=c_int),            value :: const
      integer(kind=c_int),            value :: nocopy
      type(c_ptr),                    value :: e
      type(c_funptr),                 value :: eh
    end subroutine plumed_f_cmd_safe_nothrow_short_scalar
  end interface
  interface
    subroutine plumed_f_cmd_safe_nothrow_short(p,key,val,pass_shape,const,nocopy,e,eh) bind(C,name="plumed_f_cmd_safe_nothrow_short&
        &")
      import
      character(kind=c_char), intent(in)    :: p(32)
      character(kind=c_char), intent(in)    :: key(*)
      integer(kind=c_short)                           :: val(*)
      integer(kind=c_size_t) :: pass_shape(*)
      integer(kind=c_int),            value :: const
      integer(kind=c_int),            value :: nocopy
      type(c_ptr),                    value :: e
      type(c_funptr),                 value :: eh
    end subroutine plumed_f_cmd_safe_nothrow_short
  end interface
  interface
    subroutine plumed_f_cmd_safe_nothrow_long_scalar(p,key,val,pass_shape,const,nocopy,e,eh)&
        & bind(C,name="plumed_f_cmd_safe_nothrow_long")
      import
      character(kind=c_char), intent(in)    :: p(32)
      character(kind=c_char), intent(in)    :: key(*)
      integer(kind=c_long)                           :: val
      integer(kind=c_size_t) :: pass_shape(*)
      integer(kind=c_int),            value :: const
      integer(kind=c_int),            value :: nocopy
      type(c_ptr),                    value :: e
      type(c_funptr),                 value :: eh
    end subroutine plumed_f_cmd_safe_nothrow_long_scalar
  end interface
  interface
    subroutine plumed_f_cmd_safe_nothrow_long(p,key,val,pass_shape,const,nocopy,e,eh) bind(C,name="plumed_f_cmd_safe_nothrow_long")
      import
      character(kind=c_char), intent(in)    :: p(32)
      character(kind=c_char), intent(in)    :: key(*)
      integer(kind=c_long)                           :: val(*)
      integer(kind=c_size_t) :: pass_shape(*)
      integer(kind=c_int),            value :: const
      integer(kind=c_int),            value :: nocopy
      type(c_ptr),                    value :: e
      type(c_funptr),                 value :: eh
    end subroutine plumed_f_cmd_safe_nothrow_long
  end interface
  interface
    subroutine plumed_f_cmd_safe_nothrow_float_scalar(p,key,val,pass_shape,const,nocopy,e,eh)&
        & bind(C,name="plumed_f_cmd_safe_nothrow_float")
      import
      character(kind=c_char), intent(in)    :: p(32)
      character(kind=c_char), intent(in)    :: key(*)
      real(kind=c_float)                           :: val
      integer(kind=c_size_t) :: pass_shape(*)
      integer(kind=c_int),            value :: const
      integer(kind=c_int),            value :: nocopy
      type(c_ptr),                    value :: e
      type(c_funptr),                 value :: eh
    end subroutine plumed_f_cmd_safe_nothrow_float_scalar
  end interface
  interface
    subroutine plumed_f_cmd_safe_nothrow_float(p,key,val,pass_shape,const,nocopy,e,eh) bind(C,name="plumed_f_cmd_safe_nothrow_float&
        &")
      import
      character(kind=c_char), intent(in)    :: p(32)
      character(kind=c_char), intent(in)    :: key(*)
      real(kind=c_float)                           :: val(*)
      integer(kind=c_size_t) :: pass_shape(*)
      integer(kind=c_int),            value :: const
      integer(kind=c_int),            value :: nocopy
      type(c_ptr),                    value :: e
      type(c_funptr),                 value :: eh
    end subroutine plumed_f_cmd_safe_nothrow_float
  end interface
  interface
    subroutine plumed_f_cmd_safe_nothrow_double_scalar(p,key,val,pass_shape,const,nocopy,e,eh)&
        & bind(C,name="plumed_f_cmd_safe_nothrow_double")
      import
      character(kind=c_char), intent(in)    :: p(32)
      character(kind=c_char), intent(in)    :: key(*)
      real(kind=c_double)                           :: val
      integer(kind=c_size_t) :: pass_shape(*)
      integer(kind=c_int),            value :: const
      integer(kind=c_int),            value :: nocopy
      type(c_ptr),                    value :: e
      type(c_funptr),                 value :: eh
    end subroutine plumed_f_cmd_safe_nothrow_double_scalar
  end interface
  interface
    subroutine plumed_f_cmd_safe_nothrow_double(p,key,val,pass_shape,const,nocopy,e,eh)&
        & bind(C,name="plumed_f_cmd_safe_nothrow_double")
      import
      character(kind=c_char), intent(in)    :: p(32)
      character(kind=c_char), intent(in)    :: key(*)
      real(kind=c_double)                           :: val(*)
      integer(kind=c_size_t) :: pass_shape(*)
      integer(kind=c_int),            value :: const
      integer(kind=c_int),            value :: nocopy
      type(c_ptr),                    value :: e
      type(c_funptr),                 value :: eh
    end subroutine plumed_f_cmd_safe_nothrow_double
  end interface
  interface
    subroutine plumed_f_cmd_safe_nothrow_long_double_scalar(p,key,val,pass_shape,const,nocopy,e,eh)&
        & bind(C,name="plumed_f_cmd_safe_nothrow_long_double")
      import
      character(kind=c_char), intent(in)    :: p(32)
      character(kind=c_char), intent(in)    :: key(*)
      real(kind=c_long_double)                           :: val
      integer(kind=c_size_t) :: pass_shape(*)
      integer(kind=c_int),            value :: const
      integer(kind=c_int),            value :: nocopy
      type(c_ptr),                    value :: e
      type(c_funptr),                 value :: eh
    end subroutine plumed_f_cmd_safe_nothrow_long_double_scalar
  end interface
  interface
    subroutine plumed_f_cmd_safe_nothrow_long_double(p,key,val,pass_shape,const,nocopy,e,eh)&
        & bind(C,name="plumed_f_cmd_safe_nothrow_long_double")
      import
      character(kind=c_char), intent(in)    :: p(32)
      character(kind=c_char), intent(in)    :: key(*)
      real(kind=c_long_double)                           :: val(*)
      integer(kind=c_size_t) :: pass_shape(*)
      integer(kind=c_int),            value :: const
      integer(kind=c_int),            value :: nocopy
      type(c_ptr),                    value :: e
      type(c_funptr),                 value :: eh
    end subroutine plumed_f_cmd_safe_nothrow_long_double
  end interface

  contains

     subroutine plumed_f_f08_eh(error_ptr,code,what_ptr,opt_ptr) bind(C)
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
     end subroutine plumed_f_f08_eh

     subroutine plumed_f_cmd_ptr(p,key,val,dummy,error,const,nocopy)
       character(kind=c_char,len=32), intent(in)    :: p
       character(kind=c_char,len=*),  intent(in)    :: key
       type(c_ptr),                     value       :: val
       type(dummy_type),   optional                 :: dummy
       type(plumed_error), optional,  intent(out)   :: error
       logical,            optional,  intent(in)    :: const
       logical,            optional,  intent(in)    :: nocopy
       type(plumed_error), target :: myerror
       integer(kind=c_size_t) :: pass_shape(1)
       integer(kind=c_int)    :: inocopy,iconst
       pass_shape=(/0/)
       if(present(nocopy)) then
         if(nocopy) then
           inocopy=1
         endif
       else
         inocopy=0
       endif
       if(present(const)) then
         if(const) then
           iconst=1
         endif
       else
         iconst=0
       endif
       if(present(error)) then
         call plumed_f_cmd_safe_nothrow_ptr(p,key,val,pass_shape,iconst,inocopy,c_loc(myerror),c_funloc(plumed_f_f08_eh))
         error=myerror
       else
         call plumed_f_cmd_safe_nothrow_ptr(p,key,val,pass_shape,iconst,inocopy,c_null_ptr,c_null_ptr)
       endif
     end subroutine plumed_f_cmd_ptr

     subroutine plumed_f_cmd_char(p,key,val,dummy,error,const,nocopy)
       character(kind=c_char,len=32), intent(in)    :: p
       character(kind=c_char,len=*),  intent(in)    :: key
       character(kind=c_char,len=*), asynchronous   :: val
       type(dummy_type),   optional                 :: dummy
       type(plumed_error), optional,  intent(out)   :: error
       logical,            optional,  intent(in)    :: const
       logical,            optional,  intent(in)    :: nocopy
       type(plumed_error), target :: myerror
       integer(kind=c_size_t) :: pass_shape(2)
       integer(kind=c_int)    :: inocopy,iconst
       pass_shape=(/len(val),0/)
       if(present(nocopy)) then
         if(nocopy) then
           inocopy=1
         endif
       else
         inocopy=0
       endif
       if(present(const)) then
         if(const) then
           iconst=1
         endif
       else
         iconst=0
       endif
       if(present(error)) then
         call plumed_f_cmd_safe_nothrow_char(p,key,val,pass_shape,iconst,inocopy,c_loc(myerror),c_funloc(plumed_f_f08_eh))
         error=myerror
       else
         call plumed_f_cmd_safe_nothrow_char(p,key,val,pass_shape,iconst,inocopy,c_null_ptr,c_null_ptr)
       endif
     end subroutine plumed_f_cmd_char

    subroutine plumed_f_cmd_integer_0_0(p,key,val,dummy,error,const,nocopy)
      character(kind=c_char,len=32), intent(in)    :: p
      character(kind=c_char,len=*),  intent(in)    :: key
      integer(KIND=c_int), asynchronous              :: val
       type(dummy_type),   optional                 :: dummy
      type(plumed_error), optional,  intent(out)   :: error
      logical,            optional,  intent(in)    :: const
      logical,            optional,  intent(in)    :: nocopy
      type(plumed_error), target :: myerror
      integer(kind=c_int)    :: inocopy,iconst
      integer(kind=c_size_t) :: pass_shape(2)
      pass_shape=(/1,0/)
       if(present(nocopy)) then
         if(nocopy) then
           inocopy=1
         endif
       else
         inocopy=0
       endif
       if(present(const)) then
         if(const) then
           iconst=1
         endif
       else
         iconst=0
       endif
       if(present(error)) then
         call plumed_f_cmd_safe_nothrow_int_scalar(p,key,val,pass_shape,iconst,inocopy,c_loc(myerror),c_funloc(plumed_f_f08_eh))
         error=myerror
       else
         call plumed_f_cmd_safe_nothrow_int_scalar(p,key,val,pass_shape,iconst,inocopy,c_null_ptr,c_null_ptr)
       endif
    end subroutine plumed_f_cmd_integer_0_0
    subroutine plumed_f_cmd_integer_0_1(p,key,val,dummy,error,const,nocopy)
      character(kind=c_char,len=32), intent(in)    :: p
      character(kind=c_char,len=*),  intent(in)    :: key
      integer(KIND=c_int), asynchronous              :: val(:)
       type(dummy_type),   optional                 :: dummy
      type(plumed_error), optional,  intent(out)   :: error
      logical,            optional,  intent(in)    :: const
      logical,            optional,  intent(in)    :: nocopy
      type(plumed_error), target :: myerror
      integer(kind=c_int)    :: inocopy,iconst
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
       if(present(const)) then
         if(const) then
           iconst=1
         endif
       else
         iconst=0
       endif
       if(present(error)) then
         call plumed_f_cmd_safe_nothrow_int(p,key,val,pass_shape,iconst,inocopy,c_loc(myerror),c_funloc(plumed_f_f08_eh))
         error=myerror
       else
         call plumed_f_cmd_safe_nothrow_int(p,key,val,pass_shape,iconst,inocopy,c_null_ptr,c_null_ptr)
       endif
    end subroutine plumed_f_cmd_integer_0_1
    subroutine plumed_f_cmd_integer_0_2(p,key,val,dummy,error,const,nocopy)
      character(kind=c_char,len=32), intent(in)    :: p
      character(kind=c_char,len=*),  intent(in)    :: key
      integer(KIND=c_int), asynchronous              :: val(:,:)
       type(dummy_type),   optional                 :: dummy
      type(plumed_error), optional,  intent(out)   :: error
      logical,            optional,  intent(in)    :: const
      logical,            optional,  intent(in)    :: nocopy
      type(plumed_error), target :: myerror
      integer(kind=c_int)    :: inocopy,iconst
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
       if(present(const)) then
         if(const) then
           iconst=1
         endif
       else
         iconst=0
       endif
       if(present(error)) then
         call plumed_f_cmd_safe_nothrow_int(p,key,val,pass_shape,iconst,inocopy,c_loc(myerror),c_funloc(plumed_f_f08_eh))
         error=myerror
       else
         call plumed_f_cmd_safe_nothrow_int(p,key,val,pass_shape,iconst,inocopy,c_null_ptr,c_null_ptr)
       endif
    end subroutine plumed_f_cmd_integer_0_2
    subroutine plumed_f_cmd_integer_0_3(p,key,val,dummy,error,const,nocopy)
      character(kind=c_char,len=32), intent(in)    :: p
      character(kind=c_char,len=*),  intent(in)    :: key
      integer(KIND=c_int), asynchronous              :: val(:,:,:)
       type(dummy_type),   optional                 :: dummy
      type(plumed_error), optional,  intent(out)   :: error
      logical,            optional,  intent(in)    :: const
      logical,            optional,  intent(in)    :: nocopy
      type(plumed_error), target :: myerror
      integer(kind=c_int)    :: inocopy,iconst
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
       if(present(const)) then
         if(const) then
           iconst=1
         endif
       else
         iconst=0
       endif
       if(present(error)) then
         call plumed_f_cmd_safe_nothrow_int(p,key,val,pass_shape,iconst,inocopy,c_loc(myerror),c_funloc(plumed_f_f08_eh))
         error=myerror
       else
         call plumed_f_cmd_safe_nothrow_int(p,key,val,pass_shape,iconst,inocopy,c_null_ptr,c_null_ptr)
       endif
    end subroutine plumed_f_cmd_integer_0_3
    subroutine plumed_f_cmd_integer_0_4(p,key,val,dummy,error,const,nocopy)
      character(kind=c_char,len=32), intent(in)    :: p
      character(kind=c_char,len=*),  intent(in)    :: key
      integer(KIND=c_int), asynchronous              :: val(:,:,:,:)
       type(dummy_type),   optional                 :: dummy
      type(plumed_error), optional,  intent(out)   :: error
      logical,            optional,  intent(in)    :: const
      logical,            optional,  intent(in)    :: nocopy
      type(plumed_error), target :: myerror
      integer(kind=c_int)    :: inocopy,iconst
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
       if(present(const)) then
         if(const) then
           iconst=1
         endif
       else
         iconst=0
       endif
       if(present(error)) then
         call plumed_f_cmd_safe_nothrow_int(p,key,val,pass_shape,iconst,inocopy,c_loc(myerror),c_funloc(plumed_f_f08_eh))
         error=myerror
       else
         call plumed_f_cmd_safe_nothrow_int(p,key,val,pass_shape,iconst,inocopy,c_null_ptr,c_null_ptr)
       endif
    end subroutine plumed_f_cmd_integer_0_4
    subroutine plumed_f_cmd_integer_1_0(p,key,val,dummy,error,const,nocopy)
      character(kind=c_char,len=32), intent(in)    :: p
      character(kind=c_char,len=*),  intent(in)    :: key
      integer(KIND=c_short), asynchronous              :: val
       type(dummy_type),   optional                 :: dummy
      type(plumed_error), optional,  intent(out)   :: error
      logical,            optional,  intent(in)    :: const
      logical,            optional,  intent(in)    :: nocopy
      type(plumed_error), target :: myerror
      integer(kind=c_int)    :: inocopy,iconst
      integer(kind=c_size_t) :: pass_shape(2)
      pass_shape=(/1,0/)
       if(present(nocopy)) then
         if(nocopy) then
           inocopy=1
         endif
       else
         inocopy=0
       endif
       if(present(const)) then
         if(const) then
           iconst=1
         endif
       else
         iconst=0
       endif
       if(present(error)) then
         call plumed_f_cmd_safe_nothrow_short_scalar(p,key,val,pass_shape,iconst,inocopy,c_loc(myerror),c_funloc(plumed_f_f08_eh))
         error=myerror
       else
         call plumed_f_cmd_safe_nothrow_short_scalar(p,key,val,pass_shape,iconst,inocopy,c_null_ptr,c_null_ptr)
       endif
    end subroutine plumed_f_cmd_integer_1_0
    subroutine plumed_f_cmd_integer_1_1(p,key,val,dummy,error,const,nocopy)
      character(kind=c_char,len=32), intent(in)    :: p
      character(kind=c_char,len=*),  intent(in)    :: key
      integer(KIND=c_short), asynchronous              :: val(:)
       type(dummy_type),   optional                 :: dummy
      type(plumed_error), optional,  intent(out)   :: error
      logical,            optional,  intent(in)    :: const
      logical,            optional,  intent(in)    :: nocopy
      type(plumed_error), target :: myerror
      integer(kind=c_int)    :: inocopy,iconst
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
       if(present(const)) then
         if(const) then
           iconst=1
         endif
       else
         iconst=0
       endif
       if(present(error)) then
         call plumed_f_cmd_safe_nothrow_short(p,key,val,pass_shape,iconst,inocopy,c_loc(myerror),c_funloc(plumed_f_f08_eh))
         error=myerror
       else
         call plumed_f_cmd_safe_nothrow_short(p,key,val,pass_shape,iconst,inocopy,c_null_ptr,c_null_ptr)
       endif
    end subroutine plumed_f_cmd_integer_1_1
    subroutine plumed_f_cmd_integer_1_2(p,key,val,dummy,error,const,nocopy)
      character(kind=c_char,len=32), intent(in)    :: p
      character(kind=c_char,len=*),  intent(in)    :: key
      integer(KIND=c_short), asynchronous              :: val(:,:)
       type(dummy_type),   optional                 :: dummy
      type(plumed_error), optional,  intent(out)   :: error
      logical,            optional,  intent(in)    :: const
      logical,            optional,  intent(in)    :: nocopy
      type(plumed_error), target :: myerror
      integer(kind=c_int)    :: inocopy,iconst
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
       if(present(const)) then
         if(const) then
           iconst=1
         endif
       else
         iconst=0
       endif
       if(present(error)) then
         call plumed_f_cmd_safe_nothrow_short(p,key,val,pass_shape,iconst,inocopy,c_loc(myerror),c_funloc(plumed_f_f08_eh))
         error=myerror
       else
         call plumed_f_cmd_safe_nothrow_short(p,key,val,pass_shape,iconst,inocopy,c_null_ptr,c_null_ptr)
       endif
    end subroutine plumed_f_cmd_integer_1_2
    subroutine plumed_f_cmd_integer_1_3(p,key,val,dummy,error,const,nocopy)
      character(kind=c_char,len=32), intent(in)    :: p
      character(kind=c_char,len=*),  intent(in)    :: key
      integer(KIND=c_short), asynchronous              :: val(:,:,:)
       type(dummy_type),   optional                 :: dummy
      type(plumed_error), optional,  intent(out)   :: error
      logical,            optional,  intent(in)    :: const
      logical,            optional,  intent(in)    :: nocopy
      type(plumed_error), target :: myerror
      integer(kind=c_int)    :: inocopy,iconst
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
       if(present(const)) then
         if(const) then
           iconst=1
         endif
       else
         iconst=0
       endif
       if(present(error)) then
         call plumed_f_cmd_safe_nothrow_short(p,key,val,pass_shape,iconst,inocopy,c_loc(myerror),c_funloc(plumed_f_f08_eh))
         error=myerror
       else
         call plumed_f_cmd_safe_nothrow_short(p,key,val,pass_shape,iconst,inocopy,c_null_ptr,c_null_ptr)
       endif
    end subroutine plumed_f_cmd_integer_1_3
    subroutine plumed_f_cmd_integer_1_4(p,key,val,dummy,error,const,nocopy)
      character(kind=c_char,len=32), intent(in)    :: p
      character(kind=c_char,len=*),  intent(in)    :: key
      integer(KIND=c_short), asynchronous              :: val(:,:,:,:)
       type(dummy_type),   optional                 :: dummy
      type(plumed_error), optional,  intent(out)   :: error
      logical,            optional,  intent(in)    :: const
      logical,            optional,  intent(in)    :: nocopy
      type(plumed_error), target :: myerror
      integer(kind=c_int)    :: inocopy,iconst
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
       if(present(const)) then
         if(const) then
           iconst=1
         endif
       else
         iconst=0
       endif
       if(present(error)) then
         call plumed_f_cmd_safe_nothrow_short(p,key,val,pass_shape,iconst,inocopy,c_loc(myerror),c_funloc(plumed_f_f08_eh))
         error=myerror
       else
         call plumed_f_cmd_safe_nothrow_short(p,key,val,pass_shape,iconst,inocopy,c_null_ptr,c_null_ptr)
       endif
    end subroutine plumed_f_cmd_integer_1_4
    subroutine plumed_f_cmd_integer_2_0(p,key,val,dummy,error,const,nocopy)
      character(kind=c_char,len=32), intent(in)    :: p
      character(kind=c_char,len=*),  intent(in)    :: key
      integer(KIND=c_long), asynchronous              :: val
       type(dummy_type),   optional                 :: dummy
      type(plumed_error), optional,  intent(out)   :: error
      logical,            optional,  intent(in)    :: const
      logical,            optional,  intent(in)    :: nocopy
      type(plumed_error), target :: myerror
      integer(kind=c_int)    :: inocopy,iconst
      integer(kind=c_size_t) :: pass_shape(2)
      pass_shape=(/1,0/)
       if(present(nocopy)) then
         if(nocopy) then
           inocopy=1
         endif
       else
         inocopy=0
       endif
       if(present(const)) then
         if(const) then
           iconst=1
         endif
       else
         iconst=0
       endif
       if(present(error)) then
         call plumed_f_cmd_safe_nothrow_long_scalar(p,key,val,pass_shape,iconst,inocopy,c_loc(myerror),c_funloc(plumed_f_f08_eh))
         error=myerror
       else
         call plumed_f_cmd_safe_nothrow_long_scalar(p,key,val,pass_shape,iconst,inocopy,c_null_ptr,c_null_ptr)
       endif
    end subroutine plumed_f_cmd_integer_2_0
    subroutine plumed_f_cmd_integer_2_1(p,key,val,dummy,error,const,nocopy)
      character(kind=c_char,len=32), intent(in)    :: p
      character(kind=c_char,len=*),  intent(in)    :: key
      integer(KIND=c_long), asynchronous              :: val(:)
       type(dummy_type),   optional                 :: dummy
      type(plumed_error), optional,  intent(out)   :: error
      logical,            optional,  intent(in)    :: const
      logical,            optional,  intent(in)    :: nocopy
      type(plumed_error), target :: myerror
      integer(kind=c_int)    :: inocopy,iconst
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
       if(present(const)) then
         if(const) then
           iconst=1
         endif
       else
         iconst=0
       endif
       if(present(error)) then
         call plumed_f_cmd_safe_nothrow_long(p,key,val,pass_shape,iconst,inocopy,c_loc(myerror),c_funloc(plumed_f_f08_eh))
         error=myerror
       else
         call plumed_f_cmd_safe_nothrow_long(p,key,val,pass_shape,iconst,inocopy,c_null_ptr,c_null_ptr)
       endif
    end subroutine plumed_f_cmd_integer_2_1
    subroutine plumed_f_cmd_integer_2_2(p,key,val,dummy,error,const,nocopy)
      character(kind=c_char,len=32), intent(in)    :: p
      character(kind=c_char,len=*),  intent(in)    :: key
      integer(KIND=c_long), asynchronous              :: val(:,:)
       type(dummy_type),   optional                 :: dummy
      type(plumed_error), optional,  intent(out)   :: error
      logical,            optional,  intent(in)    :: const
      logical,            optional,  intent(in)    :: nocopy
      type(plumed_error), target :: myerror
      integer(kind=c_int)    :: inocopy,iconst
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
       if(present(const)) then
         if(const) then
           iconst=1
         endif
       else
         iconst=0
       endif
       if(present(error)) then
         call plumed_f_cmd_safe_nothrow_long(p,key,val,pass_shape,iconst,inocopy,c_loc(myerror),c_funloc(plumed_f_f08_eh))
         error=myerror
       else
         call plumed_f_cmd_safe_nothrow_long(p,key,val,pass_shape,iconst,inocopy,c_null_ptr,c_null_ptr)
       endif
    end subroutine plumed_f_cmd_integer_2_2
    subroutine plumed_f_cmd_integer_2_3(p,key,val,dummy,error,const,nocopy)
      character(kind=c_char,len=32), intent(in)    :: p
      character(kind=c_char,len=*),  intent(in)    :: key
      integer(KIND=c_long), asynchronous              :: val(:,:,:)
       type(dummy_type),   optional                 :: dummy
      type(plumed_error), optional,  intent(out)   :: error
      logical,            optional,  intent(in)    :: const
      logical,            optional,  intent(in)    :: nocopy
      type(plumed_error), target :: myerror
      integer(kind=c_int)    :: inocopy,iconst
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
       if(present(const)) then
         if(const) then
           iconst=1
         endif
       else
         iconst=0
       endif
       if(present(error)) then
         call plumed_f_cmd_safe_nothrow_long(p,key,val,pass_shape,iconst,inocopy,c_loc(myerror),c_funloc(plumed_f_f08_eh))
         error=myerror
       else
         call plumed_f_cmd_safe_nothrow_long(p,key,val,pass_shape,iconst,inocopy,c_null_ptr,c_null_ptr)
       endif
    end subroutine plumed_f_cmd_integer_2_3
    subroutine plumed_f_cmd_integer_2_4(p,key,val,dummy,error,const,nocopy)
      character(kind=c_char,len=32), intent(in)    :: p
      character(kind=c_char,len=*),  intent(in)    :: key
      integer(KIND=c_long), asynchronous              :: val(:,:,:,:)
       type(dummy_type),   optional                 :: dummy
      type(plumed_error), optional,  intent(out)   :: error
      logical,            optional,  intent(in)    :: const
      logical,            optional,  intent(in)    :: nocopy
      type(plumed_error), target :: myerror
      integer(kind=c_int)    :: inocopy,iconst
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
       if(present(const)) then
         if(const) then
           iconst=1
         endif
       else
         iconst=0
       endif
       if(present(error)) then
         call plumed_f_cmd_safe_nothrow_long(p,key,val,pass_shape,iconst,inocopy,c_loc(myerror),c_funloc(plumed_f_f08_eh))
         error=myerror
       else
         call plumed_f_cmd_safe_nothrow_long(p,key,val,pass_shape,iconst,inocopy,c_null_ptr,c_null_ptr)
       endif
    end subroutine plumed_f_cmd_integer_2_4
    subroutine plumed_f_cmd_real_0_0(p,key,val,dummy,error,const,nocopy)
      character(kind=c_char,len=32), intent(in)    :: p
      character(kind=c_char,len=*),  intent(in)    :: key
      real(KIND=c_float), asynchronous              :: val
       type(dummy_type),   optional                 :: dummy
      type(plumed_error), optional,  intent(out)   :: error
      logical,            optional,  intent(in)    :: const
      logical,            optional,  intent(in)    :: nocopy
      type(plumed_error), target :: myerror
      integer(kind=c_int)    :: inocopy,iconst
      integer(kind=c_size_t) :: pass_shape(2)
      pass_shape=(/1,0/)
       if(present(nocopy)) then
         if(nocopy) then
           inocopy=1
         endif
       else
         inocopy=0
       endif
       if(present(const)) then
         if(const) then
           iconst=1
         endif
       else
         iconst=0
       endif
       if(present(error)) then
         call plumed_f_cmd_safe_nothrow_float_scalar(p,key,val,pass_shape,iconst,inocopy,c_loc(myerror),c_funloc(plumed_f_f08_eh))
         error=myerror
       else
         call plumed_f_cmd_safe_nothrow_float_scalar(p,key,val,pass_shape,iconst,inocopy,c_null_ptr,c_null_ptr)
       endif
    end subroutine plumed_f_cmd_real_0_0
    subroutine plumed_f_cmd_real_0_1(p,key,val,dummy,error,const,nocopy)
      character(kind=c_char,len=32), intent(in)    :: p
      character(kind=c_char,len=*),  intent(in)    :: key
      real(KIND=c_float), asynchronous              :: val(:)
       type(dummy_type),   optional                 :: dummy
      type(plumed_error), optional,  intent(out)   :: error
      logical,            optional,  intent(in)    :: const
      logical,            optional,  intent(in)    :: nocopy
      type(plumed_error), target :: myerror
      integer(kind=c_int)    :: inocopy,iconst
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
       if(present(const)) then
         if(const) then
           iconst=1
         endif
       else
         iconst=0
       endif
       if(present(error)) then
         call plumed_f_cmd_safe_nothrow_float(p,key,val,pass_shape,iconst,inocopy,c_loc(myerror),c_funloc(plumed_f_f08_eh))
         error=myerror
       else
         call plumed_f_cmd_safe_nothrow_float(p,key,val,pass_shape,iconst,inocopy,c_null_ptr,c_null_ptr)
       endif
    end subroutine plumed_f_cmd_real_0_1
    subroutine plumed_f_cmd_real_0_2(p,key,val,dummy,error,const,nocopy)
      character(kind=c_char,len=32), intent(in)    :: p
      character(kind=c_char,len=*),  intent(in)    :: key
      real(KIND=c_float), asynchronous              :: val(:,:)
       type(dummy_type),   optional                 :: dummy
      type(plumed_error), optional,  intent(out)   :: error
      logical,            optional,  intent(in)    :: const
      logical,            optional,  intent(in)    :: nocopy
      type(plumed_error), target :: myerror
      integer(kind=c_int)    :: inocopy,iconst
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
       if(present(const)) then
         if(const) then
           iconst=1
         endif
       else
         iconst=0
       endif
       if(present(error)) then
         call plumed_f_cmd_safe_nothrow_float(p,key,val,pass_shape,iconst,inocopy,c_loc(myerror),c_funloc(plumed_f_f08_eh))
         error=myerror
       else
         call plumed_f_cmd_safe_nothrow_float(p,key,val,pass_shape,iconst,inocopy,c_null_ptr,c_null_ptr)
       endif
    end subroutine plumed_f_cmd_real_0_2
    subroutine plumed_f_cmd_real_0_3(p,key,val,dummy,error,const,nocopy)
      character(kind=c_char,len=32), intent(in)    :: p
      character(kind=c_char,len=*),  intent(in)    :: key
      real(KIND=c_float), asynchronous              :: val(:,:,:)
       type(dummy_type),   optional                 :: dummy
      type(plumed_error), optional,  intent(out)   :: error
      logical,            optional,  intent(in)    :: const
      logical,            optional,  intent(in)    :: nocopy
      type(plumed_error), target :: myerror
      integer(kind=c_int)    :: inocopy,iconst
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
       if(present(const)) then
         if(const) then
           iconst=1
         endif
       else
         iconst=0
       endif
       if(present(error)) then
         call plumed_f_cmd_safe_nothrow_float(p,key,val,pass_shape,iconst,inocopy,c_loc(myerror),c_funloc(plumed_f_f08_eh))
         error=myerror
       else
         call plumed_f_cmd_safe_nothrow_float(p,key,val,pass_shape,iconst,inocopy,c_null_ptr,c_null_ptr)
       endif
    end subroutine plumed_f_cmd_real_0_3
    subroutine plumed_f_cmd_real_0_4(p,key,val,dummy,error,const,nocopy)
      character(kind=c_char,len=32), intent(in)    :: p
      character(kind=c_char,len=*),  intent(in)    :: key
      real(KIND=c_float), asynchronous              :: val(:,:,:,:)
       type(dummy_type),   optional                 :: dummy
      type(plumed_error), optional,  intent(out)   :: error
      logical,            optional,  intent(in)    :: const
      logical,            optional,  intent(in)    :: nocopy
      type(plumed_error), target :: myerror
      integer(kind=c_int)    :: inocopy,iconst
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
       if(present(const)) then
         if(const) then
           iconst=1
         endif
       else
         iconst=0
       endif
       if(present(error)) then
         call plumed_f_cmd_safe_nothrow_float(p,key,val,pass_shape,iconst,inocopy,c_loc(myerror),c_funloc(plumed_f_f08_eh))
         error=myerror
       else
         call plumed_f_cmd_safe_nothrow_float(p,key,val,pass_shape,iconst,inocopy,c_null_ptr,c_null_ptr)
       endif
    end subroutine plumed_f_cmd_real_0_4
    subroutine plumed_f_cmd_real_1_0(p,key,val,dummy,error,const,nocopy)
      character(kind=c_char,len=32), intent(in)    :: p
      character(kind=c_char,len=*),  intent(in)    :: key
      real(KIND=c_double), asynchronous              :: val
       type(dummy_type),   optional                 :: dummy
      type(plumed_error), optional,  intent(out)   :: error
      logical,            optional,  intent(in)    :: const
      logical,            optional,  intent(in)    :: nocopy
      type(plumed_error), target :: myerror
      integer(kind=c_int)    :: inocopy,iconst
      integer(kind=c_size_t) :: pass_shape(2)
      pass_shape=(/1,0/)
       if(present(nocopy)) then
         if(nocopy) then
           inocopy=1
         endif
       else
         inocopy=0
       endif
       if(present(const)) then
         if(const) then
           iconst=1
         endif
       else
         iconst=0
       endif
       if(present(error)) then
         call plumed_f_cmd_safe_nothrow_double_scalar(p,key,val,pass_shape,iconst,inocopy,c_loc(myerror),c_funloc(plumed_f_f08_eh))
         error=myerror
       else
         call plumed_f_cmd_safe_nothrow_double_scalar(p,key,val,pass_shape,iconst,inocopy,c_null_ptr,c_null_ptr)
       endif
    end subroutine plumed_f_cmd_real_1_0
    subroutine plumed_f_cmd_real_1_1(p,key,val,dummy,error,const,nocopy)
      character(kind=c_char,len=32), intent(in)    :: p
      character(kind=c_char,len=*),  intent(in)    :: key
      real(KIND=c_double), asynchronous              :: val(:)
       type(dummy_type),   optional                 :: dummy
      type(plumed_error), optional,  intent(out)   :: error
      logical,            optional,  intent(in)    :: const
      logical,            optional,  intent(in)    :: nocopy
      type(plumed_error), target :: myerror
      integer(kind=c_int)    :: inocopy,iconst
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
       if(present(const)) then
         if(const) then
           iconst=1
         endif
       else
         iconst=0
       endif
       if(present(error)) then
         call plumed_f_cmd_safe_nothrow_double(p,key,val,pass_shape,iconst,inocopy,c_loc(myerror),c_funloc(plumed_f_f08_eh))
         error=myerror
       else
         call plumed_f_cmd_safe_nothrow_double(p,key,val,pass_shape,iconst,inocopy,c_null_ptr,c_null_ptr)
       endif
    end subroutine plumed_f_cmd_real_1_1
    subroutine plumed_f_cmd_real_1_2(p,key,val,dummy,error,const,nocopy)
      character(kind=c_char,len=32), intent(in)    :: p
      character(kind=c_char,len=*),  intent(in)    :: key
      real(KIND=c_double), asynchronous              :: val(:,:)
       type(dummy_type),   optional                 :: dummy
      type(plumed_error), optional,  intent(out)   :: error
      logical,            optional,  intent(in)    :: const
      logical,            optional,  intent(in)    :: nocopy
      type(plumed_error), target :: myerror
      integer(kind=c_int)    :: inocopy,iconst
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
       if(present(const)) then
         if(const) then
           iconst=1
         endif
       else
         iconst=0
       endif
       if(present(error)) then
         call plumed_f_cmd_safe_nothrow_double(p,key,val,pass_shape,iconst,inocopy,c_loc(myerror),c_funloc(plumed_f_f08_eh))
         error=myerror
       else
         call plumed_f_cmd_safe_nothrow_double(p,key,val,pass_shape,iconst,inocopy,c_null_ptr,c_null_ptr)
       endif
    end subroutine plumed_f_cmd_real_1_2
    subroutine plumed_f_cmd_real_1_3(p,key,val,dummy,error,const,nocopy)
      character(kind=c_char,len=32), intent(in)    :: p
      character(kind=c_char,len=*),  intent(in)    :: key
      real(KIND=c_double), asynchronous              :: val(:,:,:)
       type(dummy_type),   optional                 :: dummy
      type(plumed_error), optional,  intent(out)   :: error
      logical,            optional,  intent(in)    :: const
      logical,            optional,  intent(in)    :: nocopy
      type(plumed_error), target :: myerror
      integer(kind=c_int)    :: inocopy,iconst
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
       if(present(const)) then
         if(const) then
           iconst=1
         endif
       else
         iconst=0
       endif
       if(present(error)) then
         call plumed_f_cmd_safe_nothrow_double(p,key,val,pass_shape,iconst,inocopy,c_loc(myerror),c_funloc(plumed_f_f08_eh))
         error=myerror
       else
         call plumed_f_cmd_safe_nothrow_double(p,key,val,pass_shape,iconst,inocopy,c_null_ptr,c_null_ptr)
       endif
    end subroutine plumed_f_cmd_real_1_3
    subroutine plumed_f_cmd_real_1_4(p,key,val,dummy,error,const,nocopy)
      character(kind=c_char,len=32), intent(in)    :: p
      character(kind=c_char,len=*),  intent(in)    :: key
      real(KIND=c_double), asynchronous              :: val(:,:,:,:)
       type(dummy_type),   optional                 :: dummy
      type(plumed_error), optional,  intent(out)   :: error
      logical,            optional,  intent(in)    :: const
      logical,            optional,  intent(in)    :: nocopy
      type(plumed_error), target :: myerror
      integer(kind=c_int)    :: inocopy,iconst
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
       if(present(const)) then
         if(const) then
           iconst=1
         endif
       else
         iconst=0
       endif
       if(present(error)) then
         call plumed_f_cmd_safe_nothrow_double(p,key,val,pass_shape,iconst,inocopy,c_loc(myerror),c_funloc(plumed_f_f08_eh))
         error=myerror
       else
         call plumed_f_cmd_safe_nothrow_double(p,key,val,pass_shape,iconst,inocopy,c_null_ptr,c_null_ptr)
       endif
    end subroutine plumed_f_cmd_real_1_4
    subroutine plumed_f_cmd_real_2_0(p,key,val,dummy,error,const,nocopy)
      character(kind=c_char,len=32), intent(in)    :: p
      character(kind=c_char,len=*),  intent(in)    :: key
      real(KIND=c_long_double), asynchronous              :: val
       type(dummy_type),   optional                 :: dummy
      type(plumed_error), optional,  intent(out)   :: error
      logical,            optional,  intent(in)    :: const
      logical,            optional,  intent(in)    :: nocopy
      type(plumed_error), target :: myerror
      integer(kind=c_int)    :: inocopy,iconst
      integer(kind=c_size_t) :: pass_shape(2)
      pass_shape=(/1,0/)
       if(present(nocopy)) then
         if(nocopy) then
           inocopy=1
         endif
       else
         inocopy=0
       endif
       if(present(const)) then
         if(const) then
           iconst=1
         endif
       else
         iconst=0
       endif
       if(present(error)) then
         call plumed_f_cmd_safe_nothrow_long_double_scalar(p,key,val,pass_shape,iconst,inocopy,c_loc(myerror),c_funloc(plumed_f_f08&
             &_eh))
         error=myerror
       else
         call plumed_f_cmd_safe_nothrow_long_double_scalar(p,key,val,pass_shape,iconst,inocopy,c_null_ptr,c_null_ptr)
       endif
    end subroutine plumed_f_cmd_real_2_0
    subroutine plumed_f_cmd_real_2_1(p,key,val,dummy,error,const,nocopy)
      character(kind=c_char,len=32), intent(in)    :: p
      character(kind=c_char,len=*),  intent(in)    :: key
      real(KIND=c_long_double), asynchronous              :: val(:)
       type(dummy_type),   optional                 :: dummy
      type(plumed_error), optional,  intent(out)   :: error
      logical,            optional,  intent(in)    :: const
      logical,            optional,  intent(in)    :: nocopy
      type(plumed_error), target :: myerror
      integer(kind=c_int)    :: inocopy,iconst
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
       if(present(const)) then
         if(const) then
           iconst=1
         endif
       else
         iconst=0
       endif
       if(present(error)) then
         call plumed_f_cmd_safe_nothrow_long_double(p,key,val,pass_shape,iconst,inocopy,c_loc(myerror),c_funloc(plumed_f_f08_eh))
         error=myerror
       else
         call plumed_f_cmd_safe_nothrow_long_double(p,key,val,pass_shape,iconst,inocopy,c_null_ptr,c_null_ptr)
       endif
    end subroutine plumed_f_cmd_real_2_1
    subroutine plumed_f_cmd_real_2_2(p,key,val,dummy,error,const,nocopy)
      character(kind=c_char,len=32), intent(in)    :: p
      character(kind=c_char,len=*),  intent(in)    :: key
      real(KIND=c_long_double), asynchronous              :: val(:,:)
       type(dummy_type),   optional                 :: dummy
      type(plumed_error), optional,  intent(out)   :: error
      logical,            optional,  intent(in)    :: const
      logical,            optional,  intent(in)    :: nocopy
      type(plumed_error), target :: myerror
      integer(kind=c_int)    :: inocopy,iconst
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
       if(present(const)) then
         if(const) then
           iconst=1
         endif
       else
         iconst=0
       endif
       if(present(error)) then
         call plumed_f_cmd_safe_nothrow_long_double(p,key,val,pass_shape,iconst,inocopy,c_loc(myerror),c_funloc(plumed_f_f08_eh))
         error=myerror
       else
         call plumed_f_cmd_safe_nothrow_long_double(p,key,val,pass_shape,iconst,inocopy,c_null_ptr,c_null_ptr)
       endif
    end subroutine plumed_f_cmd_real_2_2
    subroutine plumed_f_cmd_real_2_3(p,key,val,dummy,error,const,nocopy)
      character(kind=c_char,len=32), intent(in)    :: p
      character(kind=c_char,len=*),  intent(in)    :: key
      real(KIND=c_long_double), asynchronous              :: val(:,:,:)
       type(dummy_type),   optional                 :: dummy
      type(plumed_error), optional,  intent(out)   :: error
      logical,            optional,  intent(in)    :: const
      logical,            optional,  intent(in)    :: nocopy
      type(plumed_error), target :: myerror
      integer(kind=c_int)    :: inocopy,iconst
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
       if(present(const)) then
         if(const) then
           iconst=1
         endif
       else
         iconst=0
       endif
       if(present(error)) then
         call plumed_f_cmd_safe_nothrow_long_double(p,key,val,pass_shape,iconst,inocopy,c_loc(myerror),c_funloc(plumed_f_f08_eh))
         error=myerror
       else
         call plumed_f_cmd_safe_nothrow_long_double(p,key,val,pass_shape,iconst,inocopy,c_null_ptr,c_null_ptr)
       endif
    end subroutine plumed_f_cmd_real_2_3
    subroutine plumed_f_cmd_real_2_4(p,key,val,dummy,error,const,nocopy)
      character(kind=c_char,len=32), intent(in)    :: p
      character(kind=c_char,len=*),  intent(in)    :: key
      real(KIND=c_long_double), asynchronous              :: val(:,:,:,:)
       type(dummy_type),   optional                 :: dummy
      type(plumed_error), optional,  intent(out)   :: error
      logical,            optional,  intent(in)    :: const
      logical,            optional,  intent(in)    :: nocopy
      type(plumed_error), target :: myerror
      integer(kind=c_int)    :: inocopy,iconst
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
       if(present(const)) then
         if(const) then
           iconst=1
         endif
       else
         iconst=0
       endif
       if(present(error)) then
         call plumed_f_cmd_safe_nothrow_long_double(p,key,val,pass_shape,iconst,inocopy,c_loc(myerror),c_funloc(plumed_f_f08_eh))
         error=myerror
       else
         call plumed_f_cmd_safe_nothrow_long_double(p,key,val,pass_shape,iconst,inocopy,c_null_ptr,c_null_ptr)
       endif
    end subroutine plumed_f_cmd_real_2_4

     function plumed_installed() result(res)
       logical             :: res
       integer(kind=c_int) :: i
       call plumed_f_installed(i)
       res=i>0
     end function plumed_installed

     impure elemental subroutine plumed_create(this,kernel)
       type(plumed),    intent(out)          :: this
       character(len=*), intent(in), optional :: kernel
       if(present(kernel)) then
         call plumed_f_create_dlopen(kernel // c_null_char,this%handle)
       else
         call plumed_f_create(this%handle)
       endif
       this%initialized=.true.
     end subroutine plumed_create

     impure elemental subroutine pl_finalize(this)
       class(plumed), intent(inout) :: this
       if(this%initialized) then
         call plumed_f_finalize(this%handle)
         this%initialized=.false.
       endif
     end subroutine pl_finalize

     impure elemental subroutine pl_incref(this)
       class(plumed), intent(inout) :: this
       character(kind=c_char,len=32) :: that
       if(.not.this%initialized) then
         call plumed_create(this)
       endif
       call plumed_f_create_reference(this%handle,that)
     end subroutine pl_incref

     impure elemental subroutine pl_decref(this,to)
       class(plumed),     intent(inout) :: this
       integer, optional, intent(in)    :: to
       if(.not.this%initialized) then
         call plumed_create(this)
       endif
       if(present(to)) then
         do while(this%use_count()>to)
           call plumed_f_finalize(this%handle)
         end do
       else
         call plumed_f_finalize(this%handle)
       endif
     end subroutine pl_decref

     ! "impure elemental" needed for the destructor to work on arrays
     impure elemental subroutine pl_destructor(this)
       type(plumed), intent(inout) :: this
       call this%finalize()
     end subroutine pl_destructor

     impure elemental function pl_valid(this) result(valid)
       class(plumed), intent(inout) :: this
       logical :: valid
       integer(c_int) :: i
       if(.not.this%initialized) then
         call plumed_create(this)
       endif
       call plumed_f_valid(this%handle,i)
       valid=i>0
     end function pl_valid

     impure elemental function pl_use_count(this) result(use_count)
       class(plumed), intent(inout) :: this
       integer(c_int) :: use_count
       if(.not.this%initialized) then
         call plumed_create(this)
       endif
       call plumed_f_use_count(this%handle,use_count)
     end function pl_use_count

     impure elemental subroutine pl_assign(this,that)
       class(plumed),intent(out) :: this
       class(plumed),intent(in)  :: that
       if(that%initialized) then
         call plumed_f_create_reference(that%handle,this%handle)
         this%initialized=.true.
       endif
     end subroutine pl_assign

     impure elemental subroutine pl_cmd(this,key,dummy,error,const,nocopy)
       class(plumed),                 intent(inout) :: this ! inout to allow for initialization
       character(kind=c_char,len=*),  intent(in)    :: key
       type(dummy_type),   optional,  intent(inout) :: dummy
       type(plumed_error), optional,  intent(out)   :: error
       logical,            optional,  intent(in)    :: const
       logical,            optional,  intent(in)    :: nocopy
       if(.not.this%initialized) then
         call plumed_create(this)
       endif
       call plumed_f_cmd(this%handle,key // c_null_char,0,error=error,const=const,nocopy=nocopy) ! FIX: replace this to send NULL
     end subroutine pl_cmd

     subroutine pl_cmd_char(this,key,val,dummy,error,const,nocopy)
       class(plumed),                 intent(inout) :: this ! inout to allow for initialization
       character(kind=c_char,len=*),  intent(in)    :: key
       character(kind=c_char,len=*), asynchronous   :: val
       type(dummy_type),   optional,  intent(inout) :: dummy
       type(plumed_error), optional,  intent(out)   :: error
       logical,            optional,  intent(in)    :: const
       logical,            optional,  intent(in)    :: nocopy
       if(.not.this%initialized) then
         call plumed_create(this)
       endif
       call plumed_f_cmd(this%handle,key // c_null_char,val // c_null_char,error=error,const=const,nocopy=nocopy)
     end subroutine pl_cmd_char

     subroutine pl_cmd_ptr(this,key,val,dummy,error,const,nocopy)
       class(plumed),                 intent(inout) :: this ! inout to allow for initialization
       character(kind=c_char,len=*),  intent(in)    :: key
       type(c_ptr),                        value    :: val
       type(dummy_type),   optional,  intent(inout) :: dummy
       type(plumed_error), optional,  intent(out)   :: error
       logical,            optional,  intent(in)    :: const
       logical,            optional,  intent(in)    :: nocopy
       if(.not.this%initialized) then
         call plumed_create(this)
       endif
       call plumed_f_cmd(this%handle,key // c_null_char,val,error=error,const=const,nocopy=nocopy)
     end subroutine pl_cmd_ptr

    subroutine pl_cmd_integer_0_0(this,key,val,dummy,error,const,nocopy)
      class(plumed),                 intent(inout) :: this ! inout to allow for initialization
      character(kind=c_char,len=*),  intent(in)    :: key
      integer(KIND=c_int), asynchronous              :: val
       type(dummy_type),   optional, intent(inout) :: dummy
       type(plumed_error), optional,  intent(out)   :: error
       logical,            optional,  intent(in)    :: const
       logical,            optional,  intent(in)    :: nocopy
       if(.not.this%initialized) then
         call plumed_create(this)
       endif
       call plumed_f_cmd(this%handle,key // c_null_char,val,error=error,const=const,nocopy=nocopy)
    end subroutine pl_cmd_integer_0_0
    subroutine pl_cmd_integer_0_1(this,key,val,dummy,error,const,nocopy)
      class(plumed),                 intent(inout) :: this ! inout to allow for initialization
      character(kind=c_char,len=*),  intent(in)    :: key
      integer(KIND=c_int), asynchronous              :: val(:)
       type(dummy_type),   optional, intent(inout) :: dummy
       type(plumed_error), optional,  intent(out)   :: error
       logical,            optional,  intent(in)    :: const
       logical,            optional,  intent(in)    :: nocopy
       if(.not.this%initialized) then
         call plumed_create(this)
       endif
       call plumed_f_cmd(this%handle,key // c_null_char,val,error=error,const=const,nocopy=nocopy)
    end subroutine pl_cmd_integer_0_1
    subroutine pl_cmd_integer_0_2(this,key,val,dummy,error,const,nocopy)
      class(plumed),                 intent(inout) :: this ! inout to allow for initialization
      character(kind=c_char,len=*),  intent(in)    :: key
      integer(KIND=c_int), asynchronous              :: val(:,:)
       type(dummy_type),   optional, intent(inout) :: dummy
       type(plumed_error), optional,  intent(out)   :: error
       logical,            optional,  intent(in)    :: const
       logical,            optional,  intent(in)    :: nocopy
       if(.not.this%initialized) then
         call plumed_create(this)
       endif
       call plumed_f_cmd(this%handle,key // c_null_char,val,error=error,const=const,nocopy=nocopy)
    end subroutine pl_cmd_integer_0_2
    subroutine pl_cmd_integer_0_3(this,key,val,dummy,error,const,nocopy)
      class(plumed),                 intent(inout) :: this ! inout to allow for initialization
      character(kind=c_char,len=*),  intent(in)    :: key
      integer(KIND=c_int), asynchronous              :: val(:,:,:)
       type(dummy_type),   optional, intent(inout) :: dummy
       type(plumed_error), optional,  intent(out)   :: error
       logical,            optional,  intent(in)    :: const
       logical,            optional,  intent(in)    :: nocopy
       if(.not.this%initialized) then
         call plumed_create(this)
       endif
       call plumed_f_cmd(this%handle,key // c_null_char,val,error=error,const=const,nocopy=nocopy)
    end subroutine pl_cmd_integer_0_3
    subroutine pl_cmd_integer_0_4(this,key,val,dummy,error,const,nocopy)
      class(plumed),                 intent(inout) :: this ! inout to allow for initialization
      character(kind=c_char,len=*),  intent(in)    :: key
      integer(KIND=c_int), asynchronous              :: val(:,:,:,:)
       type(dummy_type),   optional, intent(inout) :: dummy
       type(plumed_error), optional,  intent(out)   :: error
       logical,            optional,  intent(in)    :: const
       logical,            optional,  intent(in)    :: nocopy
       if(.not.this%initialized) then
         call plumed_create(this)
       endif
       call plumed_f_cmd(this%handle,key // c_null_char,val,error=error,const=const,nocopy=nocopy)
    end subroutine pl_cmd_integer_0_4
    subroutine pl_cmd_integer_1_0(this,key,val,dummy,error,const,nocopy)
      class(plumed),                 intent(inout) :: this ! inout to allow for initialization
      character(kind=c_char,len=*),  intent(in)    :: key
      integer(KIND=c_short), asynchronous              :: val
       type(dummy_type),   optional, intent(inout) :: dummy
       type(plumed_error), optional,  intent(out)   :: error
       logical,            optional,  intent(in)    :: const
       logical,            optional,  intent(in)    :: nocopy
       if(.not.this%initialized) then
         call plumed_create(this)
       endif
       call plumed_f_cmd(this%handle,key // c_null_char,val,error=error,const=const,nocopy=nocopy)
    end subroutine pl_cmd_integer_1_0
    subroutine pl_cmd_integer_1_1(this,key,val,dummy,error,const,nocopy)
      class(plumed),                 intent(inout) :: this ! inout to allow for initialization
      character(kind=c_char,len=*),  intent(in)    :: key
      integer(KIND=c_short), asynchronous              :: val(:)
       type(dummy_type),   optional, intent(inout) :: dummy
       type(plumed_error), optional,  intent(out)   :: error
       logical,            optional,  intent(in)    :: const
       logical,            optional,  intent(in)    :: nocopy
       if(.not.this%initialized) then
         call plumed_create(this)
       endif
       call plumed_f_cmd(this%handle,key // c_null_char,val,error=error,const=const,nocopy=nocopy)
    end subroutine pl_cmd_integer_1_1
    subroutine pl_cmd_integer_1_2(this,key,val,dummy,error,const,nocopy)
      class(plumed),                 intent(inout) :: this ! inout to allow for initialization
      character(kind=c_char,len=*),  intent(in)    :: key
      integer(KIND=c_short), asynchronous              :: val(:,:)
       type(dummy_type),   optional, intent(inout) :: dummy
       type(plumed_error), optional,  intent(out)   :: error
       logical,            optional,  intent(in)    :: const
       logical,            optional,  intent(in)    :: nocopy
       if(.not.this%initialized) then
         call plumed_create(this)
       endif
       call plumed_f_cmd(this%handle,key // c_null_char,val,error=error,const=const,nocopy=nocopy)
    end subroutine pl_cmd_integer_1_2
    subroutine pl_cmd_integer_1_3(this,key,val,dummy,error,const,nocopy)
      class(plumed),                 intent(inout) :: this ! inout to allow for initialization
      character(kind=c_char,len=*),  intent(in)    :: key
      integer(KIND=c_short), asynchronous              :: val(:,:,:)
       type(dummy_type),   optional, intent(inout) :: dummy
       type(plumed_error), optional,  intent(out)   :: error
       logical,            optional,  intent(in)    :: const
       logical,            optional,  intent(in)    :: nocopy
       if(.not.this%initialized) then
         call plumed_create(this)
       endif
       call plumed_f_cmd(this%handle,key // c_null_char,val,error=error,const=const,nocopy=nocopy)
    end subroutine pl_cmd_integer_1_3
    subroutine pl_cmd_integer_1_4(this,key,val,dummy,error,const,nocopy)
      class(plumed),                 intent(inout) :: this ! inout to allow for initialization
      character(kind=c_char,len=*),  intent(in)    :: key
      integer(KIND=c_short), asynchronous              :: val(:,:,:,:)
       type(dummy_type),   optional, intent(inout) :: dummy
       type(plumed_error), optional,  intent(out)   :: error
       logical,            optional,  intent(in)    :: const
       logical,            optional,  intent(in)    :: nocopy
       if(.not.this%initialized) then
         call plumed_create(this)
       endif
       call plumed_f_cmd(this%handle,key // c_null_char,val,error=error,const=const,nocopy=nocopy)
    end subroutine pl_cmd_integer_1_4
    subroutine pl_cmd_integer_2_0(this,key,val,dummy,error,const,nocopy)
      class(plumed),                 intent(inout) :: this ! inout to allow for initialization
      character(kind=c_char,len=*),  intent(in)    :: key
      integer(KIND=c_long), asynchronous              :: val
       type(dummy_type),   optional, intent(inout) :: dummy
       type(plumed_error), optional,  intent(out)   :: error
       logical,            optional,  intent(in)    :: const
       logical,            optional,  intent(in)    :: nocopy
       if(.not.this%initialized) then
         call plumed_create(this)
       endif
       call plumed_f_cmd(this%handle,key // c_null_char,val,error=error,const=const,nocopy=nocopy)
    end subroutine pl_cmd_integer_2_0
    subroutine pl_cmd_integer_2_1(this,key,val,dummy,error,const,nocopy)
      class(plumed),                 intent(inout) :: this ! inout to allow for initialization
      character(kind=c_char,len=*),  intent(in)    :: key
      integer(KIND=c_long), asynchronous              :: val(:)
       type(dummy_type),   optional, intent(inout) :: dummy
       type(plumed_error), optional,  intent(out)   :: error
       logical,            optional,  intent(in)    :: const
       logical,            optional,  intent(in)    :: nocopy
       if(.not.this%initialized) then
         call plumed_create(this)
       endif
       call plumed_f_cmd(this%handle,key // c_null_char,val,error=error,const=const,nocopy=nocopy)
    end subroutine pl_cmd_integer_2_1
    subroutine pl_cmd_integer_2_2(this,key,val,dummy,error,const,nocopy)
      class(plumed),                 intent(inout) :: this ! inout to allow for initialization
      character(kind=c_char,len=*),  intent(in)    :: key
      integer(KIND=c_long), asynchronous              :: val(:,:)
       type(dummy_type),   optional, intent(inout) :: dummy
       type(plumed_error), optional,  intent(out)   :: error
       logical,            optional,  intent(in)    :: const
       logical,            optional,  intent(in)    :: nocopy
       if(.not.this%initialized) then
         call plumed_create(this)
       endif
       call plumed_f_cmd(this%handle,key // c_null_char,val,error=error,const=const,nocopy=nocopy)
    end subroutine pl_cmd_integer_2_2
    subroutine pl_cmd_integer_2_3(this,key,val,dummy,error,const,nocopy)
      class(plumed),                 intent(inout) :: this ! inout to allow for initialization
      character(kind=c_char,len=*),  intent(in)    :: key
      integer(KIND=c_long), asynchronous              :: val(:,:,:)
       type(dummy_type),   optional, intent(inout) :: dummy
       type(plumed_error), optional,  intent(out)   :: error
       logical,            optional,  intent(in)    :: const
       logical,            optional,  intent(in)    :: nocopy
       if(.not.this%initialized) then
         call plumed_create(this)
       endif
       call plumed_f_cmd(this%handle,key // c_null_char,val,error=error,const=const,nocopy=nocopy)
    end subroutine pl_cmd_integer_2_3
    subroutine pl_cmd_integer_2_4(this,key,val,dummy,error,const,nocopy)
      class(plumed),                 intent(inout) :: this ! inout to allow for initialization
      character(kind=c_char,len=*),  intent(in)    :: key
      integer(KIND=c_long), asynchronous              :: val(:,:,:,:)
       type(dummy_type),   optional, intent(inout) :: dummy
       type(plumed_error), optional,  intent(out)   :: error
       logical,            optional,  intent(in)    :: const
       logical,            optional,  intent(in)    :: nocopy
       if(.not.this%initialized) then
         call plumed_create(this)
       endif
       call plumed_f_cmd(this%handle,key // c_null_char,val,error=error,const=const,nocopy=nocopy)
    end subroutine pl_cmd_integer_2_4
    subroutine pl_cmd_real_0_0(this,key,val,dummy,error,const,nocopy)
      class(plumed),                 intent(inout) :: this ! inout to allow for initialization
      character(kind=c_char,len=*),  intent(in)    :: key
      real(KIND=c_float), asynchronous              :: val
       type(dummy_type),   optional, intent(inout) :: dummy
       type(plumed_error), optional,  intent(out)   :: error
       logical,            optional,  intent(in)    :: const
       logical,            optional,  intent(in)    :: nocopy
       if(.not.this%initialized) then
         call plumed_create(this)
       endif
       call plumed_f_cmd(this%handle,key // c_null_char,val,error=error,const=const,nocopy=nocopy)
    end subroutine pl_cmd_real_0_0
    subroutine pl_cmd_real_0_1(this,key,val,dummy,error,const,nocopy)
      class(plumed),                 intent(inout) :: this ! inout to allow for initialization
      character(kind=c_char,len=*),  intent(in)    :: key
      real(KIND=c_float), asynchronous              :: val(:)
       type(dummy_type),   optional, intent(inout) :: dummy
       type(plumed_error), optional,  intent(out)   :: error
       logical,            optional,  intent(in)    :: const
       logical,            optional,  intent(in)    :: nocopy
       if(.not.this%initialized) then
         call plumed_create(this)
       endif
       call plumed_f_cmd(this%handle,key // c_null_char,val,error=error,const=const,nocopy=nocopy)
    end subroutine pl_cmd_real_0_1
    subroutine pl_cmd_real_0_2(this,key,val,dummy,error,const,nocopy)
      class(plumed),                 intent(inout) :: this ! inout to allow for initialization
      character(kind=c_char,len=*),  intent(in)    :: key
      real(KIND=c_float), asynchronous              :: val(:,:)
       type(dummy_type),   optional, intent(inout) :: dummy
       type(plumed_error), optional,  intent(out)   :: error
       logical,            optional,  intent(in)    :: const
       logical,            optional,  intent(in)    :: nocopy
       if(.not.this%initialized) then
         call plumed_create(this)
       endif
       call plumed_f_cmd(this%handle,key // c_null_char,val,error=error,const=const,nocopy=nocopy)
    end subroutine pl_cmd_real_0_2
    subroutine pl_cmd_real_0_3(this,key,val,dummy,error,const,nocopy)
      class(plumed),                 intent(inout) :: this ! inout to allow for initialization
      character(kind=c_char,len=*),  intent(in)    :: key
      real(KIND=c_float), asynchronous              :: val(:,:,:)
       type(dummy_type),   optional, intent(inout) :: dummy
       type(plumed_error), optional,  intent(out)   :: error
       logical,            optional,  intent(in)    :: const
       logical,            optional,  intent(in)    :: nocopy
       if(.not.this%initialized) then
         call plumed_create(this)
       endif
       call plumed_f_cmd(this%handle,key // c_null_char,val,error=error,const=const,nocopy=nocopy)
    end subroutine pl_cmd_real_0_3
    subroutine pl_cmd_real_0_4(this,key,val,dummy,error,const,nocopy)
      class(plumed),                 intent(inout) :: this ! inout to allow for initialization
      character(kind=c_char,len=*),  intent(in)    :: key
      real(KIND=c_float), asynchronous              :: val(:,:,:,:)
       type(dummy_type),   optional, intent(inout) :: dummy
       type(plumed_error), optional,  intent(out)   :: error
       logical,            optional,  intent(in)    :: const
       logical,            optional,  intent(in)    :: nocopy
       if(.not.this%initialized) then
         call plumed_create(this)
       endif
       call plumed_f_cmd(this%handle,key // c_null_char,val,error=error,const=const,nocopy=nocopy)
    end subroutine pl_cmd_real_0_4
    subroutine pl_cmd_real_1_0(this,key,val,dummy,error,const,nocopy)
      class(plumed),                 intent(inout) :: this ! inout to allow for initialization
      character(kind=c_char,len=*),  intent(in)    :: key
      real(KIND=c_double), asynchronous              :: val
       type(dummy_type),   optional, intent(inout) :: dummy
       type(plumed_error), optional,  intent(out)   :: error
       logical,            optional,  intent(in)    :: const
       logical,            optional,  intent(in)    :: nocopy
       if(.not.this%initialized) then
         call plumed_create(this)
       endif
       call plumed_f_cmd(this%handle,key // c_null_char,val,error=error,const=const,nocopy=nocopy)
    end subroutine pl_cmd_real_1_0
    subroutine pl_cmd_real_1_1(this,key,val,dummy,error,const,nocopy)
      class(plumed),                 intent(inout) :: this ! inout to allow for initialization
      character(kind=c_char,len=*),  intent(in)    :: key
      real(KIND=c_double), asynchronous              :: val(:)
       type(dummy_type),   optional, intent(inout) :: dummy
       type(plumed_error), optional,  intent(out)   :: error
       logical,            optional,  intent(in)    :: const
       logical,            optional,  intent(in)    :: nocopy
       if(.not.this%initialized) then
         call plumed_create(this)
       endif
       call plumed_f_cmd(this%handle,key // c_null_char,val,error=error,const=const,nocopy=nocopy)
    end subroutine pl_cmd_real_1_1
    subroutine pl_cmd_real_1_2(this,key,val,dummy,error,const,nocopy)
      class(plumed),                 intent(inout) :: this ! inout to allow for initialization
      character(kind=c_char,len=*),  intent(in)    :: key
      real(KIND=c_double), asynchronous              :: val(:,:)
       type(dummy_type),   optional, intent(inout) :: dummy
       type(plumed_error), optional,  intent(out)   :: error
       logical,            optional,  intent(in)    :: const
       logical,            optional,  intent(in)    :: nocopy
       if(.not.this%initialized) then
         call plumed_create(this)
       endif
       call plumed_f_cmd(this%handle,key // c_null_char,val,error=error,const=const,nocopy=nocopy)
    end subroutine pl_cmd_real_1_2
    subroutine pl_cmd_real_1_3(this,key,val,dummy,error,const,nocopy)
      class(plumed),                 intent(inout) :: this ! inout to allow for initialization
      character(kind=c_char,len=*),  intent(in)    :: key
      real(KIND=c_double), asynchronous              :: val(:,:,:)
       type(dummy_type),   optional, intent(inout) :: dummy
       type(plumed_error), optional,  intent(out)   :: error
       logical,            optional,  intent(in)    :: const
       logical,            optional,  intent(in)    :: nocopy
       if(.not.this%initialized) then
         call plumed_create(this)
       endif
       call plumed_f_cmd(this%handle,key // c_null_char,val,error=error,const=const,nocopy=nocopy)
    end subroutine pl_cmd_real_1_3
    subroutine pl_cmd_real_1_4(this,key,val,dummy,error,const,nocopy)
      class(plumed),                 intent(inout) :: this ! inout to allow for initialization
      character(kind=c_char,len=*),  intent(in)    :: key
      real(KIND=c_double), asynchronous              :: val(:,:,:,:)
       type(dummy_type),   optional, intent(inout) :: dummy
       type(plumed_error), optional,  intent(out)   :: error
       logical,            optional,  intent(in)    :: const
       logical,            optional,  intent(in)    :: nocopy
       if(.not.this%initialized) then
         call plumed_create(this)
       endif
       call plumed_f_cmd(this%handle,key // c_null_char,val,error=error,const=const,nocopy=nocopy)
    end subroutine pl_cmd_real_1_4
    subroutine pl_cmd_real_2_0(this,key,val,dummy,error,const,nocopy)
      class(plumed),                 intent(inout) :: this ! inout to allow for initialization
      character(kind=c_char,len=*),  intent(in)    :: key
      real(KIND=c_long_double), asynchronous              :: val
       type(dummy_type),   optional, intent(inout) :: dummy
       type(plumed_error), optional,  intent(out)   :: error
       logical,            optional,  intent(in)    :: const
       logical,            optional,  intent(in)    :: nocopy
       if(.not.this%initialized) then
         call plumed_create(this)
       endif
       call plumed_f_cmd(this%handle,key // c_null_char,val,error=error,const=const,nocopy=nocopy)
    end subroutine pl_cmd_real_2_0
    subroutine pl_cmd_real_2_1(this,key,val,dummy,error,const,nocopy)
      class(plumed),                 intent(inout) :: this ! inout to allow for initialization
      character(kind=c_char,len=*),  intent(in)    :: key
      real(KIND=c_long_double), asynchronous              :: val(:)
       type(dummy_type),   optional, intent(inout) :: dummy
       type(plumed_error), optional,  intent(out)   :: error
       logical,            optional,  intent(in)    :: const
       logical,            optional,  intent(in)    :: nocopy
       if(.not.this%initialized) then
         call plumed_create(this)
       endif
       call plumed_f_cmd(this%handle,key // c_null_char,val,error=error,const=const,nocopy=nocopy)
    end subroutine pl_cmd_real_2_1
    subroutine pl_cmd_real_2_2(this,key,val,dummy,error,const,nocopy)
      class(plumed),                 intent(inout) :: this ! inout to allow for initialization
      character(kind=c_char,len=*),  intent(in)    :: key
      real(KIND=c_long_double), asynchronous              :: val(:,:)
       type(dummy_type),   optional, intent(inout) :: dummy
       type(plumed_error), optional,  intent(out)   :: error
       logical,            optional,  intent(in)    :: const
       logical,            optional,  intent(in)    :: nocopy
       if(.not.this%initialized) then
         call plumed_create(this)
       endif
       call plumed_f_cmd(this%handle,key // c_null_char,val,error=error,const=const,nocopy=nocopy)
    end subroutine pl_cmd_real_2_2
    subroutine pl_cmd_real_2_3(this,key,val,dummy,error,const,nocopy)
      class(plumed),                 intent(inout) :: this ! inout to allow for initialization
      character(kind=c_char,len=*),  intent(in)    :: key
      real(KIND=c_long_double), asynchronous              :: val(:,:,:)
       type(dummy_type),   optional, intent(inout) :: dummy
       type(plumed_error), optional,  intent(out)   :: error
       logical,            optional,  intent(in)    :: const
       logical,            optional,  intent(in)    :: nocopy
       if(.not.this%initialized) then
         call plumed_create(this)
       endif
       call plumed_f_cmd(this%handle,key // c_null_char,val,error=error,const=const,nocopy=nocopy)
    end subroutine pl_cmd_real_2_3
    subroutine pl_cmd_real_2_4(this,key,val,dummy,error,const,nocopy)
      class(plumed),                 intent(inout) :: this ! inout to allow for initialization
      character(kind=c_char,len=*),  intent(in)    :: key
      real(KIND=c_long_double), asynchronous              :: val(:,:,:,:)
       type(dummy_type),   optional, intent(inout) :: dummy
       type(plumed_error), optional,  intent(out)   :: error
       logical,            optional,  intent(in)    :: const
       logical,            optional,  intent(in)    :: nocopy
       if(.not.this%initialized) then
         call plumed_create(this)
       endif
       call plumed_f_cmd(this%handle,key // c_null_char,val,error=error,const=const,nocopy=nocopy)
    end subroutine pl_cmd_real_2_4

end module plumed_module_f08

