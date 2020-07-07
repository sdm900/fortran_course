module inout

  use precision
  use globals

  implicit none


contains



  subroutine loadinput(object, b, t, tsteps)

    type(particle), intent(out) :: object
    real(fdp), dimension(dim), intent(out) :: b
    real(fdp), intent(out) :: t
    integer(idp), intent(out) :: tsteps
    character(256) :: line
    character(15) :: command


    tsteps = 1

    t = 1.0d0

    b = (/ 0.0d0, 0.0d0, 1.0d0 /)

    object%velocity = 1.0d0
    object%position = 0.0d0
    object%charge = 1.0d0
    object%mass = 1.0d0

    command = ''

    open(unit=1, file='input.dat', action='read')
    
    do while (trim(command) /= 'end')

       read(1, '(a)') line

       command = trim(line(1:15))
       
       select case (trim(command))
          
       case('time=')
          read(line(16:), *) t
          
       case('time steps=')
          read(line(16:), *) tsteps
          
       case('b')
          read(line(16:), *) b
          
       case('velocity=')
          read(line(16:), *) object%velocity
          
       case('position=')
          read(line(16:), *) object%position

       case('mass=')
          read(line(16:), *) object%mass

       case('charge=')
          read(line(16:), *) object%charge

       case('accuracy=')
          read(line(16:), *) accuracy

       case('end')
          
       case default

       end select

    end do

  end subroutine loadinput



  subroutine writeoutput(object, t)

    type(particle), intent(in) :: object
    real(fdp), intent(in) :: t

    
    write(velocitydat,'(3e20.10e3)') object%velocity

    write(positiondat,'(3e20.10e3)') object%position

    write(timedat,'(e20.10e3)') t

  end subroutine writeoutput

end module inout
