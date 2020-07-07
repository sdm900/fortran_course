module globals

  integer, parameter :: n=10, dim=3
  real(8), parameter :: pi=3.14159265359, e0=8.854187817d-12, dt=1.0d-4, &
       q=1.6d-19, m=9.11d-31, delta=1.0d-8

end module globals



program nbody

  use globals

  implicit none

  real(8), dimension(dim, n) :: a, v, s
  real(8), dimension(dim) :: r
  integer :: i, j
  real(8) :: t, d, e, twrite


  a = 0.0d0
  v = 0.0d0
  twrite = 0.0d0
  t = 0.0d0
  
  call random_number(s)

  open(unit=1, file="nbody.dat", action="write")

  do while ( t < 100.0d0)

     e = 0.0d0

     do j = 1, n
        a(:, j) = 0.0d0
        e = e + 0.5d0*m*sum(v(:, j)**2)

        do i = 1, n
           r = s(:, i) - s(:, j)
           d = max(sqrt(sum(r**2)), delta)
           a(:, j) = a(:, j)  - 0.25d0*q*q*r/m/e0/pi/d**3
           e = e + 0.25d0*q*q/e0/pi/d/2.0d0
        end do
     end do

     do j = 1, n
        s(:, j) = s(:, j) + v(:, j)*dt + 0.5d0*a(:, j)*dt**2
        v(:, j) = v(:, j) + a(:, j)*dt
        
        where(s(:, j) < 0.0d0)
           s(:,j) = -s(:,j)
           v(:,j) = -v(:,j)
        end where
        
        where(s(:, j) > 1.0d0)
           s(:,j) = 2.0d0-s(:,j)
           v(:,j) = -v(:,j)
        end where
     end do

     t = t + dt

     if (t > twrite) then
        do j = 1,n
           write(1, '(3es)') s(:,j)
        end do
        twrite = t+0.001d0
     end if
  end do

  close(1)

end program nbody
