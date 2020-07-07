program integration2

  implicit none

  integer :: i, j
  real(8) :: x, y, userfunc
  real(8), dimension(10, 10) :: integ

  external userfunc


  integ = 0.0d0

  open(unit=1, file='integration2.dat', action='write')

  write(1, *) integ

  do j = 1, 10

     y = dble(j) * 1.0d0

     do i = 1, 10

        x = dble(i) * 0.25d0

        integ(i, j) = userfunc(x, y)

        write(1, *) integ(i, j)

     end do

  end do

  close(1)

  write(*,'(a,e20.10e3)') 'Integration value = ', sum(integ)

end program integration2
