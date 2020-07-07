module globalmodule

  integer, parameter :: fdp = selected_real_kind(10, 200)
  integer, parameter :: fsp = selected_real_kind(5, 50)
  integer, parameter :: idp = selected_int_kind(9)
  integer, parameter :: isp = selected_int_kind(6)
  integer, parameter :: cdp = fdp
  integer, parameter :: csp = fsp

  real(fdp), parameter :: pi = 3.14159265359, &
       & h = 6.626075540d-34, &
       & hbar = h/2.0d0/pi

  complex(cdp), parameter :: ii = (0.0d0, 1.0d0)
  
end module globalmodule
