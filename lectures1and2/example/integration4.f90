program integration4

  use usermodule2

  implicit none

  real(8) :: x, y, integ, init
  real(8), dimension(2) :: step
  real(8), dimension(4) :: boundary


  call loadinput('input.dat', init, boundary, step)

  integ = init

  y = boundary(3)

  do while (y <= boundary(4))

     x = boundary(1)

     do while (x <= boundary(2))

        integ = integ + userfunc(x, y)

        x = x + step(1)

     end do

     y = y + step(2)

  end do

  call writeoutput('integration4.dat', integ, boundary, x, y)

end program integration4
