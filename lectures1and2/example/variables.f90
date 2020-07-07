program variables
  
  !
  !  Demonstrate some properties of variables
  !

  implicit none

  integer :: i
  integer(2) :: j
  integer(4) :: k
  integer(8) :: l

  real :: a
  real(4) :: b
  real(8) :: c

  
  write(*, *) 'Huge: ', huge(i), huge(j), huge(k), huge(l)
  write(*, *) 'Digits: ', digits(i), digits(j), digits(k), digits(l)

  write(*, *) ''

  write(*, *) 'Huge: ', huge(a), huge(b), huge(c)
  write(*, '(a,i,i,i)') 'Digits: ', digits(a), digits(b), digits(c)
  write(*, '(a8,e,e,e)') 'Epsilon: ', epsilon(a), epsilon(b), epsilon(c)
  write(*, '(a9,en,en,en)') 'Epsilon: ', epsilon(a), epsilon(b), epsilon(c)
  write(*, '(a10,3es)') 'Epsilon: ', epsilon(a), epsilon(b), epsilon(c)
  write(*, '(a11,3e20.10e4)') 'Epsilon: ', epsilon(a), epsilon(b), epsilon(c)

end program variables
