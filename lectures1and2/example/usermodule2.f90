module usermodule2

  implicit none

  real(8) :: factor = 2.0d0
  integer, parameter :: filelen = 20

contains

  function userfunc(x, y) result(func)
    
    implicit none
    
    real(8) :: x, y, func
    
    func = factor * sin(x + y)
    
  end function userfunc



  subroutine loadinput(inputfile, init, boundary, step)

    real(8) :: init
    real(8), dimension(2) :: step
    real(8), dimension(4) :: boundary
    character(filelen) :: inputfile

    open(unit=1, file=inputfile, action='read')

    read(1, *) init
    read(1, *) boundary
    read(1, *) step

    close(1)

  end subroutine loadinput



  subroutine writeoutput(inputfile, integ, boundary, x, y)

    real(8) :: integ, x, y
    real(8), dimension(4) :: boundary
    character(filelen) :: inputfile

    open(unit=1, file=inputfile, action='write')

    write(1, *) 'Integration value = ', integ
    write(1, *) 'x-range:    ', boundary(1), ' <= x <= ', boundary(2)
    write(1, *) 'y-range:    ', boundary(3), ' <= y <= ', boundary(4)
    write(1, *) 'Final x and y:    ', x, y

    close(1)    

  end subroutine writeoutput

end module usermodule2
