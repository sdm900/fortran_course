module usermodule1

  implicit none

  real(8) :: factor = 2.0d0

contains

  function userfunc(x, y) result(func)
    
    implicit none
    
    real(8) :: x, y, func
    
    func = factor * sin(x + y)**2
    
  end function userfunc

end module usermodule1
