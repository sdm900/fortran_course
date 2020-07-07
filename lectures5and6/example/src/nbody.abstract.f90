module globals

  integer, parameter :: n=100, dim=3
  real(8), parameter :: pi=3.14159265359, e0=8.854187817d-12, dt=1.0d-4, &
       q=1.6d-19, m=9.11d-31, delta=1.0d-8

  type particle
     real(8) :: m, q
     real(8), dimension(dim) :: a, s, v
  end type particle

end module globals



module misc

  use globals

contains

  subroutine init(particles)
    type(particle), dimension(:) :: particles

    integer :: n, i

    n = size(particles)

    do i=1, n
       particles(i)%m = m
       particles(i)%q = q
       particles(i)%a = 0
       particles(i)%v = 0
       
       call random_number(particles(i)%s)
    end do

  end subroutine init



  subroutine acc(p1, p2)
    type(particle) :: p1, p2

    real(8) :: d
    real(8), dimension(3) :: r

    r = p2%s - p1%s
    d = max(sqrt(sum(r**2)), delta)

    p1%a = p1%a  - 0.25d0*p1%q*p2%q*r/p1%m/e0/pi/d**3
    
  end subroutine acc


  
  subroutine vel(p)
    type(particle) :: p

    p%v = p%v + p%a*dt

  end subroutine vel



  subroutine dis(p)
    type(particle) :: p
    
    p%s = p%s + p%v*dt + 0.5d0*p%a*dt**2

  end subroutine dis



  function pote(p1, p2) result(e)
    type(particle) :: p1, p2
    real(8) :: e, d
    real(8), dimension(dim) :: r

    r = p2%s-p1%s
    d = max(sqrt(sum(r**2)), delta)
    e = 0.25d0*p1%q*p2%q/e0/pi/d

  end function pote



  function kine(p) result(e)
    type(particle) :: p
    real(8) :: e

    e = 0.5*p%m*sum(p%v**2)

  end function kine



  subroutine bounds(p)
    type(particle) :: p

    where(p%s(:) < 0.0d0)
       p%s(:) = -p%s(:)
       p%v(:) = -p%v(:)
    end where
    
    where(p%s(:) > 1.0d0)
       p%s(:) = 2.0d0-p%s(:)
       p%v(:) = -p%v(:)
    end where
  end subroutine bounds
  
end module misc



program nbody

  use globals
  use misc

  implicit none

  type(particle), dimension(n) :: particles
  integer :: i, j
  real(8) :: t, e, twrite


  call init(particles)

  t = 0.0d0
  twrite = 0.0d0

  do while ( t < 1.0d0)

     e = 0.0d0

     do j = 1, n
        particles(j)%a = 0.0d0
        e = e + kine(particles(j))

        do i = 1, n
           call acc(particles(j), particles(i))
           e = e + pote(particles(j), particles(i))
        end do
     end do

     do j = 1, n
        call vel(particles(j))
        call dis(particles(j))
        call bounds(particles(j))
     end do

     t = t + dt

     if (t > twrite) then
        write(*,*) e
        twrite = t+0.1d0
     end if
  end do
end program nbody
