program integration1

  !
  !  Perform a basic integration
  !

  implicit none

  integer :: i, j
  real(8) :: x, y, integ


  integ = 0.0d0

  open(unit=1, file='integration1.dat', action='write')

  write(1, *) integ

  do j = 1, 10

     y = dble(j) * 1.0d0

     do i = 1, 10

        x = dble(i) * 2.5d-1

        integ = integ + sin(x + y)

        if (ceiling(dble(j)/2.0d0)*2 == j .and. ceiling(dble(i)/2.0d0)*2 == i) then
           
           write(1, *) integ

        end if

     end do

  end do

  close(1)

  write(*,'(a,e20.10e3)') 'Integration value = ', integ

end program integration1
