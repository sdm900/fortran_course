program mpilaplace
  implicit none
  include "mpif.h"
  integer, parameter :: n=200, nprocs=4, m=n/nprocs
  real(8), dimension(0:n+1,0:m+1) :: u
  real(8), dimension(1:n,1:m) :: unew
  real(8) :: error, tot_error
  integer :: iter, i, j
  integer :: ierror, rank, size, up, down
  integer, dimension(MPI_STATUS_SIZE) :: status

  call MPI_Init(ierror)

  call MPI_Comm_rank(MPI_COMM_WORLD, rank, ierror)
  call MPI_Comm_size(MPI_COMM_WORLD, size, ierror)

  if (size /= nprocs) then
     write(*, *) "Can only run with 4 processesd!"
     call MPI_Finalize(ierror)
     stop
  end if

  up = rank + 1
  if (up == size) up = MPI_PROC_NULL

  down = rank - 1
  if (down == -1) down = MPI_PROC_NULL

  u = 0.0d0
  unew = 0.0d0

  if (rank == size-1) then
     u(:,m+1) = 10.0d0
  end if

  tot_error = 1000.0d0
  iter = 0

  do while ( tot_error > 1.0d-6 .and. iter < 1000)
     do j = 1,m
        do i = 1,n
           unew(i,j) = 0.25d0*(  u(i+1,j) + u(i-1,j) + u(i,j+1) + u(i,j-1) )
        end do
     end do

     error = 0.0d0

     do j = 1,m
        do i = 1,n
           error = error + abs(unew(i,j)-u(i,j))
           u(i,j) = unew(i,j)
        end do
     end do

     call MPI_AllReduce(error, tot_error, 1, MPI_DOUBLE_PRECISION, MPI_SUM, &
          MPI_COMM_WORLD, ierror)

     if ((rank/2)*2 == rank) then
        call MPI_Send( u(:,m), n, MPI_DOUBLE_PRECISION, up, 1, &
             MPI_COMM_WORLD, ierror)
        call MPI_Send( u(:,1), n, MPI_DOUBLE_PRECISION, down, 2, &
             MPI_COMM_WORLD, ierror)
        call MPI_Recv( u(:,m+1), n, MPI_DOUBLE_PRECISION, up, 2, &
             MPI_COMM_WORLD, status, ierror)
        call MPI_Recv( u(:,0), n, MPI_DOUBLE_PRECISION, down, 1, &
             MPI_COMM_WORLD, status, ierror)
     else
        call MPI_Recv( u(:,m+1), n, MPI_DOUBLE_PRECISION, up, 2, &
             MPI_COMM_WORLD, status, ierror)
        call MPI_Recv( u(:,0), n, MPI_DOUBLE_PRECISION, down, 1, &
             MPI_COMM_WORLD, status, ierror)
        call MPI_Send( u(:,m), n, MPI_DOUBLE_PRECISION, up, 1, &
             MPI_COMM_WORLD, ierror)
        call MPI_Send( u(:,1), n, MPI_DOUBLE_PRECISION, down, 2, &
             MPI_COMM_WORLD, ierror)
     end if

     iter = iter+1

     if( mod(iter,100) == 0 .and. rank == 0) then
        write(*, *) "Iteration ",iter,", error = ",tot_error
     end if
  end do

  call MPI_Finalize(ierror)

end program mpilaplace
