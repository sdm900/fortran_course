module newtons

  use precision
  use globals



contains



  function energy(object) result(e)

    type(particle), intent(in) :: object
    real(fdp) :: e

    
    e = 0.5d0 * object%mass * sum(object%velocity**2)

  end function energy



  function crossproduct(v1, v2) result(v3)

    real(fdp), dimension(3), intent(in) :: v1, v2
    real(fdp), dimension(3) :: v3


    v3(1) = v1(2)*v2(3) - v1(3)*v2(2)
    v3(2) = v1(3)*v2(1) - v1(1)*v2(3)
    v3(3) = v1(1)*v2(2) - v1(2)*v2(1)

  end function crossproduct



  function position(object, v, t) result(s)

    type(particle), intent(in) :: object
    real(fdp), dimension(dim) :: v
    real(fdp), intent(in) :: t
    real(fdp), dimension(dim) :: s


    s = object%position + v * t

  end function position



  function velocity(object, a, t) result(v)

    type(particle), intent(in) :: object
    real(fdp), dimension(dim) :: a
    real(fdp), intent(in) :: t
    real(fdp), dimension(dim) :: v


    v = object%velocity + a * t

  end function velocity



  function acceleration(object, force) result(a)

    type(particle), intent(in) :: object
    real(fdp), dimension(dim), intent(in) :: force
    real(fdp), dimension(dim) :: a


    a = force / object%mass

  end function acceleration



  function magneticforce(object, b) result(f)

    type(particle), intent(in) :: object
    real(fdp), dimension(dim), intent(in) :: b
    real(fdp), dimension(dim) :: f


    f = object%charge * crossproduct(object%velocity, b)

  end function magneticforce



end module newtons
