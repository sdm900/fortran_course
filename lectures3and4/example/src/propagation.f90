module propagation

  use precision
  use globals
  use newtons



contains



#ifdef SINGLE
  !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
  !
  !  Compute the propagation step using a simple euler step
  !

  subroutine propagatedt(object, b, dt)

    type(particle), intent(inout) :: object
    real(fdp), dimension(dim), intent(in) :: b
    real(fdp), intent(in) :: dt
    real(fdp), dimension(dim) :: force, accel
    type(particle) :: tmpobject

    
    tmpobject = object
    
    force = magneticforce(object, b)
    accel = acceleration(object, force)
    
    tmpobject%velocity = velocity(object, accel, dt)
    tmpobject%position = position(object, tmpobject%velocity, dt)
   
    object = tmpobject
    
  end subroutine propagatedt
#endif



#ifdef DOUBLE
  !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
  !
  !  Compute the propagation step using a 2nd order process
  !
  !  This method is the only stable method
  !

  subroutine propagatedt(object, b, dt)

    type(particle), intent(inout) :: object
    real(fdp), dimension(dim), intent(in) :: b
    real(fdp), intent(in) :: dt
    real(fdp), dimension(dim) :: force, accel
    type(particle) :: tmp1object, tmp2object

    
    tmp1object = object
    tmp2object = object
    
    force = magneticforce(tmp1object, b)
    accel = acceleration(tmp1object, force)
    
    tmp1object%velocity = velocity(tmp1object, accel, dt/2.0d0)
    tmp1object%position = position(tmp1object, tmp1object%velocity, dt/2.0d0)
    
    force = magneticforce(tmp1object, b)
    accel = acceleration(tmp1object, force)

    tmp2object%velocity = velocity(object, accel, dt)
    tmp2object%position = position(object, tmp2object%velocity, dt)
    
    object%velocity = tmp2object%velocity
    object%position = tmp2object%position
    
  end subroutine propagatedt
#endif



  subroutine propagate(object, b, t)

    type(particle), intent(inout) :: object
    real(fdp), dimension(dim), intent(in) :: b
    real(fdp), intent(in) :: t
    real(fdp) :: tloop, tstep, loopenergy, initialenergy
    type(particle) :: loopobject
    logical :: finished = .false.
    
    
    initialenergy = energy(object)

    tloop = 0.0d0
    tstep = t / 4.0d0

    loopobject = object
    
    do while(tloop <= t)

       call propagatedt(loopobject, b, tstep)
       
       loopenergy = energy(loopobject)

       if (abs(loopenergy - initialenergy)/initialenergy > accuracy) then

          tstep = min(tstep / max(loopenergy/initialenergy, 10.0d0), &
               & abs(t-tloop))

          loopobject = object

       else

          object = loopobject

          tloop = tloop + tstep

       end if

    end do

    object = loopobject

  end subroutine propagate
  


end module propagation
