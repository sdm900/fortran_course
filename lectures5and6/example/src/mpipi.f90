program parallel_pi
  include "mpif.h"
  integer, parameter:: dp = kind(1.0d0)
  real(dp), parameter :: pi25 = 3.141592653589793238462643_dp
  integer :: n, i
  real(dp) :: pi, h, sum, x
  logical :: done

  integer :: myid, numprocs, ierr
  real(dp) :: mypi

  call MPI_Init(ierr)
  call MPI_Comm_size(MPI_COMM_WORLD, numprocs, ierr)
  call MPI_Comm_rank(MPI_COMM_WORLD, myid, ierr)

  done = .false.

  do while ( .not.done)
     if (myid == 0) then
        write(*,'("Enter the number of intervals: (0 quits) ")')
        read(*,*) n
     endif

     call MPI_Bcast(n, 1, MPI_INTEGER, 0, MPI_COMM_WORLD, ierr)

     if (n == 0) exit

     h   = 1.0_dp/dble(n)
     sum = 0.0_dp

     do i = myid+1, n, numprocs
        x = h * (i - 0.5_dp)
        sum = sum + 4.0_dp/(1.0_dp + x*x)
     enddo

     mypi = h * sum

     call MPI_Reduce(mypi, pi, 1, MPI_DOUBLE_PRECISION, MPI_SUM, 0, &
          MPI_COMM_WORLD, ierr)

     if (myid == 0) then
        write(*,'(" pi is approximately ",f16.13," Error is ",f16.13)') &
             pi, abs(pi - pi25)
     end if
  end do

  call MPI_Finalize(ierr)

end program parallel_pi
