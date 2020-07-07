program seriallaplace
  implicit none
  integer, parameter :: n=200
  real(8), dimension(0:n+1, 0:n+1) :: u
  real(8), dimension(1:n, 1:n) :: unew
  real(8) :: error
  integer :: iter, i, j
 
  u = 0.0d0
  unew = 0.0d0
  u(:,n+1) = 10.0d0
  error = 1000.0d0
  iter = 0

  do while ( error > 1.0d-6 .and. iter < 1000)
     
     do j = 1,n
        do i = 1,n
           unew(i,j) = 0.25d0*(  u(i+1,j) + u(i-1,j) + u(i,j+1) + u(i,j-1) )
        end do
     end do
     
     error = 0.0d0

     do j = 1,n
        do i = 1,n
           error = error + abs(unew(i,j)-u(i,j))
           u(i,j) = unew(i,j)
        end do
     end do
     
     iter = iter+1
     
     if (mod(iter,100) == 0) then
        write(*, *) "Iteration ",iter,", error = ",error
     end if
  end do
  
end program seriallaplace



