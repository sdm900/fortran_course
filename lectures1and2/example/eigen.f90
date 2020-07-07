program eigen

  !
  !  Demonstrate how to call an external library
  !

  implicit none

  interface

     subroutine zgeev(JOBVL, JOBVR, N, A, LDA, W, VL, LDVL, VR, LDVR, WORK, &
          & LWORK, RWORK, INFO)
       character :: JOBVL, JOBVR
       integer :: INFO, LDA, LDVL, LDVR, LWORK, N
       real(8) :: RWORK(*)
       complex(8) :: A(*), VL(*), VR(*), W(*), WORK(*)
     end subroutine zgeev
     
  end interface
  

  complex(8), dimension(:, :), allocatable :: array, leftvectors, rightvectors
  complex(8), dimension(:), allocatable :: eigenvalues, work1
  real(8), dimension(:), allocatable :: work2
  real(8), dimension(2) :: rand
  integer :: n, i, j, info
  
  
  read(*, *) n
  
  allocate(array(n, n), eigenvalues(n), work1(2*n), work2(2*n), &
       & leftvectors(n, n), rightvectors(n, n))
  
  do j = 1, n
     
     do i = 1, n
        
        call random_number(rand)
        
        array(i, j) = cmplx(rand(1), rand(2))
        
     end do
     
  end do

  call zgeev('V', 'V', n, array, n, eigenvalues, leftvectors, n, rightvectors, &
       & n, work1, 2*n, work2, info)

  write(*,*) 'Return status: ', info
  write(*,*) eigenvalues
  
  open(unit=1, form='unformatted', file='leftvectors', action='write')
  open(unit=2, form='unformatted', file='rightvectors', action='write')

  write(1) leftvectors
  write(2) rightvectors

  close(1)
  close(2)

end program eigen
