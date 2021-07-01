subroutine test3()
use plumed_module
implicit none

character(len=32) :: pl
type(plumed_error) :: error
integer :: ierror
integer :: s
open(10,file="error_codes")

call plumed_f_create_invalid(pl)
call plumed_f_cmd(pl,"init"//achar(0),"a"//achar(0),error=error)
write(10,*)error%code
call plumed_f_finalize(pl)

call plumed_f_create(pl)
call plumed_f_cmd(pl,"initx"//achar(0),"a"//achar(0),error=error)
write(10,*)error%code
call plumed_f_cmd(pl,"initx"//achar(0),"a"//achar(0),ierror=ierror)
write(10,*)ierror
call plumed_f_cmd(pl,"setNatoms"//achar(0),3.0,error=error)
write(10,*)error%code
call plumed_f_finalize(pl)

call plumed_f_create(pl)
call plumed_f_cmd(pl,"init"//achar(0),0)
s=0
call plumed_f_cmd(pl,"setStopFlag"//achar(0),s,nocopy=.true.,error=error)
write(10,*)error%code
call plumed_f_finalize(pl)

close(10)
end subroutine test3
