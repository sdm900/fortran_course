program integration3

  use usermodule1

  implicit none

  integer :: i, j
  real(8) :: x, y, integ


  integ = 0.0d0

  j = 1

  do while (j <= 10)

     y = dble(j) * 1.0d0

     i = 1

     do while (i <= 10)

        x = dble(i) * 2.5d-1

        integ = integ + userfunc(x, y)

        i = i + 1

     end do

     j = j + 1

  end do

  write(*,'(a,e20.10e3)') 'Integration value = ', integ

end program integration3
