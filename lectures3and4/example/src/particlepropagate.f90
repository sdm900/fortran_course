program particlepropagation

  !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
  !
  !  This code will propagate a charged particle from an initial position with
  !  an initial velocity for a designated time t under the influence of a
  !  magnetic field b
  !
  !      input.dat          file with input parameters
  !      velocity.dat       file containing the velocity of the particle
  !      position.dat       file containing the position of the particle
  !      time.dat           file containing the time intervals
  !

  use precision
  use globals
  use inout
  use propagation

  implicit none


  type(particle) :: object
  real(fdp), dimension(dim) :: b
  real(fdp) :: t, tstep
  integer(idp) :: tsteps, i
  

  call loadinput(object, b, t, tsteps)

  write(*,*) 't= ', 0.0d0, '    energy= ', energy(object)

  open(unit=velocitydat, file='velocity.dat', action='write');
  open(unit=positiondat, file='position.dat', action='write');
  open(unit=timedat, file='time.dat', action='write');

  do i = 1, tsteps

     tstep = dble(i)*t/dble(tsteps)

     call propagate(object, b, tstep)

     write(*,*) 't= ', tstep, '    energy= ', energy(object)

     call writeoutput(object, t)

  end do

  close(velocitydat)
  close(positiondat)
  close(timedat)

end program particlepropagation
