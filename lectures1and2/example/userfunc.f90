function userfunc(x, y) result(func)

  implicit none

  real(8) :: x, y, func

  func = sin(x + y)

end function userfunc
