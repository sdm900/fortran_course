program firstmpi

  implicit none

  include 'mpif.h'

  integer :: mpierror, mpisize, mpirank


  call MPI_Init(mpierror)

  if (mpierror /= 0) then
     stop 'Error initialising MPI'
  end if

  call MPI_Comm_size(MPI_COMM_WORLD, mpisize, mpierror)
  call MPI_Comm_rank(MPI_COMM_WORLD, mpirank, mpierror)

  write(*,'(a,i2,a,i2)') 'Process: ', mpirank, '/', mpisize

  call MPI_Finalize(mpierror)

end program firstmpi
