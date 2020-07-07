program serialpi
  integer, parameter:: dp = kind(1.0d0)
  real(dp), parameter :: pi25 = 3.141592653589793238462643_dp
  integer :: n, i
  real(dp) :: pi, h, sum, x
  logical :: done

  done = .false.

  do while ( .not.done)
     write(*,'("Enter the number of intervals: (0 quits) ")')
     read(*,*) n

     if (n == 0) exit

     h = 1.0_dp/dble(n)
     sum = 0.0_dp

     do i = 1, n
        x = h * (i - 0.5_dp)
        sum = sum + 4.0_dp/(1.0_dp + x*x)
     enddo

     pi = h * sum

     write(*,'(" pi is approximately ",f16.13," Error is ",f16.13)') &
          pi, abs(pi - pi25)

  end do

end program serialpi
