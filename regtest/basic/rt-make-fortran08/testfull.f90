subroutine testfull()
use plumed_module_f08
use iso_c_binding
implicit none

type(plumed)      :: pl
type(c_ptr)       :: ptr
type(plumed_error) :: error
integer :: s
integer, target :: natoms
real(c_double), allocatable :: positions(:,:)
real(c_double), allocatable :: masses(:)
real(c_double), allocatable :: forces(:,:)
real(c_double) :: box(3,3)
real(c_double) :: virial(3,3)
open(10,file="error_codes_full")

call pl%cmd("initx"//achar(0),"a"//achar(0),error=error)
write(10,*)error%code
call pl%cmd("initx"//achar(0),"a"//achar(0),error=error)
write(10,*)error%code
call pl%cmd("setNatoms"//achar(0),3.0,error=error)
write(10,*)error%code
call pl%finalize()

call plumed_create(pl)
natoms=999
allocate(positions(3,natoms))
positions=3.0
allocate(forces(3,natoms))
forces=3.0
allocate(masses(natoms))
masses=1.0
box=0.0
virial=0.0
call pl%cmd("setNatoms"//achar(0),c_loc(natoms))
call pl%cmd("init"//achar(0),0)
call pl%cmd("readInputLine"//achar(0),"c: CENTER ATOMS=1-999"//achar(0))
call pl%cmd("readInputLine"//achar(0),"DUMPATOMS ATOMS=c FILE=traj.xyz"//achar(0))
call pl%cmd("readInputLine"//achar(0),"d: DISTANCE ATOMS=1,2"//achar(0))
call pl%cmd("readInputLine"//achar(0),"RESTRAINT ARG=d KAPPA=10.0 AT=10.0"//achar(0))
call pl%cmd("setStep"//achar(0),1)
call pl%cmd("setPositions"//achar(0),positions)
call pl%cmd("setMasses"//achar(0),masses)
call pl%cmd("setForces"//achar(0),forces)
call pl%cmd("setBox"//achar(0),box)
call pl%cmd("setVirial"//achar(0),virial)
call pl%cmd("calc"//achar(0),0)

close(10)
end subroutine testfull
