program pingpong

  implicit none

  include 'mpif.h'

  integer :: mpierror, mpisize, mpirank
  integer, dimension(MPI_STATUS_SIZE) :: mpistatus
  integer :: sendbuf, recvbuf, to, from


  call MPI_Init(mpierror)

  if (mpierror /= 0) then
     stop 'Error initialising MPI'
  end if

  call MPI_Comm_size(MPI_COMM_WORLD, mpisize, mpierror)
  call MPI_Comm_rank(MPI_COMM_WORLD, mpirank, mpierror)

  to = modulo(mpirank+1, mpisize)
  from = modulo(mpirank-1, mpisize)
 
  sendbuf = mpirank

  if ((mpirank/2)*2 == mpirank) then
     call MPI_Send(sendbuf, 1, MPI_INTEGER, to, 0, MPI_COMM_WORLD, mpierror)
     call MPI_Recv(recvbuf, 1, MPI_INTEGER, from, 0, MPI_COMM_WORLD, &
          & mpistatus, mpierror)
  else
     call MPI_Recv(recvbuf, 1, MPI_INTEGER, from, 0, MPI_COMM_WORLD, &
          & mpistatus, mpierror)
     call MPI_Send(sendbuf, 1, MPI_INTEGER, to, 0, MPI_COMM_WORLD, mpierror)
  end if

  write(*,'(i2,a,i2,a,i2,a,i2)') mpirank, '/', mpisize, '    sent: ', &
       & sendbuf, '    recv: ', recvbuf

  call MPI_Finalize(mpierror)

end program pingpong
