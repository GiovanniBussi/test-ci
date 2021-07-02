subroutine test3()
use plumed_module
use iso_c_binding
implicit none

character(len=32) :: pl
type(plumed_error) :: error
type(c_ptr)        :: ptr
integer :: ierror
integer :: s
integer, target :: natoms
real(c_double), allocatable :: positions(:,:)
real(c_double), allocatable :: masses(:)
real(c_double), allocatable :: forces(:,:)
real(c_double) :: box(3,3)
real(c_double) :: virial(3,3)
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

call plumed_f_create(pl)
natoms=999
allocate(positions(3,natoms))
positions=3.0
allocate(forces(3,natoms))
forces=3.0
allocate(masses(natoms))
masses=1.0
box=0.0
virial=0.0
call plumed_f_cmd(pl,"setNatoms"//achar(0),c_loc(natoms))
call plumed_f_cmd(pl,"init"//achar(0),0)
call plumed_f_cmd(pl,"readInputLine"//achar(0),"c: CENTER ATOMS=1-999"//achar(0))
call plumed_f_cmd(pl,"readInputLine"//achar(0),"DUMPATOMS ATOMS=c FILE=traj.xyz"//achar(0))
call plumed_f_cmd(pl,"readInputLine"//achar(0),"d: DISTANCE ATOMS=1,2"//achar(0))
call plumed_f_cmd(pl,"readInputLine"//achar(0),"RESTRAINT ARG=d KAPPA=10.0 AT=10.0"//achar(0))
call plumed_f_cmd(pl,"setStep"//achar(0),1)
call plumed_f_cmd(pl,"setPositions"//achar(0),positions)
call plumed_f_cmd(pl,"setMasses"//achar(0),masses)
call plumed_f_cmd(pl,"setForces"//achar(0),forces,const=.true.)
call plumed_f_cmd(pl,"setBox"//achar(0),box)
call plumed_f_cmd(pl,"setVirial"//achar(0),virial)
call plumed_f_cmd(pl,"calc"//achar(0),0,error=error)
if(error%code==0)then
  stop "should give an error"
endif
call plumed_f_cmd(pl,"setStep"//achar(0),1)
call plumed_f_cmd(pl,"setPositions"//achar(0),positions)
call plumed_f_cmd(pl,"setMasses"//achar(0),masses)
call plumed_f_cmd(pl,"setForces"//achar(0),forces)
call plumed_f_cmd(pl,"setBox"//achar(0),box)
call plumed_f_cmd(pl,"setVirial"//achar(0),virial)
call plumed_f_cmd(pl,"calc"//achar(0),0)
call plumed_f_finalize(pl)

close(10)
end subroutine test3
