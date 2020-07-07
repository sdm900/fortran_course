program nbody

  implicit none

  include "mpif.h"

  integer, parameter :: n=50, ndim=3
  real(8), parameter :: pi=3.14159265359, e0=8.854187817d-12, &
       e=1.6d-19, m=9.11d-31, delta=1.0d-12, bsize=100.0d0

  integer :: ierr, mpirank, mpisize, i, j, k, l, up, down
  integer, dimension(2) :: requests
  integer, dimension(MPI_STATUS_SIZE, 2) :: statuss
  real(8), dimension(ndim) :: d
  real(8), dimension(ndim, n) :: s, v, a, ibuf, obuf, tbuf
  real(8) :: r1, r2, t, tmax, tstep, tmp, energy, twrite

  call MPI_Init(ierr)
  call MPI_Comm_size(MPI_COMM_WORLD, mpisize, ierr)
  call MPI_Comm_rank(MPI_COMM_WORLD, mpirank, ierr)

  do i = 0, mpirank
     call random_number(s)
  end do
  
  s = bsize*s
  v = 0.0d0
  a = 0.0d0
  tmax = 1.0d0
  tstep = 0.0001d0
  twrite = 0.0d0
  t = 0.0d0
  energy = 0.0d0
  up = modulo(mpirank+1, mpisize)
  down = modulo(mpirank-1, mpisize)

  do while(t <= tmax)
     ibuf = s

     do l = 1, mpisize
        do i = 1, n
           do j = 1, n
              d = s(:, i) - ibuf(:, j)
              r1 = sqrt(sum(d**2))
              r2 = max(r1, delta)
              a(:,i) = a(:,i) + 0.25d0/pi/e0*e*e/r2**3/m * d
           end do
        end do

        obuf = ibuf
        
        call MPI_Irecv(ibuf, n*ndim, MPI_DOUBLE_PRECISION, &
             & down, 1, MPI_COMM_WORLD, requests(1), ierr)
        call MPI_Issend(obuf, n*ndim, MPI_DOUBLE_PRECISION, &
             & up, 1, MPI_COMM_WORLD, requests(2), ierr)
        call MPI_Waitall(2, requests, statuss, ierr)
     end do
     
     do i= 1, n
        tbuf(:, i) = s(:, i) + 0.5d0*v(:, i)*tstep + 0.25d0*a(:, i)*tstep**2
        
        where (tbuf(:, i) > bsize)
           tbuf(:, i) = 2.0d0*bsize-tbuf(:, i)
        end where
        
        where (tbuf(:, i) < 0.0d0)
           tbuf(:, i) = -tbuf(:, i)
        end where

        a(:, i) = 0.0d0
     end do

     ibuf = tbuf

     do l = 1, mpisize
        do i = 1, n
           do j = 1, n
              d = tbuf(:, i) - ibuf(:, j)
              r1 = sqrt(sum(d**2))
              r2 = max(r1, delta)
              a(:,i) = a(:,i) + 0.25d0/pi/e0*e*e/r2**3/m * d
              energy = energy + 0.125d0/pi/e0*e*e/r2**2*r1
           end do
        end do

        obuf = ibuf
        
        call MPI_Irecv(ibuf, n*ndim, MPI_DOUBLE_PRECISION, &
             & down, 1, MPI_COMM_WORLD, requests(1), ierr)
        call MPI_Issend(obuf, n*ndim, MPI_DOUBLE_PRECISION, &
             & up, 1, MPI_COMM_WORLD, requests(2), ierr)
        call MPI_Waitall(2, requests, statuss, ierr)
     end do

     call MPI_Allreduce(energy, tmp, 1, MPI_DOUBLE_PRECISION, &
          & MPI_SUM, MPI_COMM_WORLD, ierr)

     energy = tmp
     
     if (t >= twrite) then
        if (mpirank == 0) then
           write(*,*) energy
        end if
        
        twrite = t+tmax/100.0d0
     end if
     
     energy = 0.0d0

     do i= 1, n
        v(:, i) = v(:, i) + a(:, i)*tstep
        s(:, i) = s(:, i) + v(:, i)*tstep
        
        where (s(:, i) > bsize)
           s(:, i) = 2.0d0*bsize-s(:, i)
           v(:, i) = -v(:, i)
        end where
        
        where (s(:, i) < 0.0d0)
           s(:, i) = -s(:, i)
           v(:, i) = -v(:, i)
        end where
        
        energy = energy + 0.5d0*m*sum(v(:, i)**2)
        a(:, i) = 0.0d0
     end do

     t = t + tstep
  end do
  
  call MPI_Finalize(ierr)
  
end program nbody
