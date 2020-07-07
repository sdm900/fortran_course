module globals

  use precision

  integer(isp), parameter :: dim=3
  integer(isp), parameter :: velocitydat=99, positiondat=98, timedat=97
  real(fdp) :: accuracy=1.0d-4

  type particle

     real(fdp), dimension(dim) :: position, velocity
     real(fdp) :: mass, charge

  end type particle

end module globals
